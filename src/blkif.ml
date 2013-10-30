(*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2012 Incubaid BVBA
 *
 * Baardskeerder is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Baardskeerder is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Baardskeerder.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Bs_internal

module Lwt_ = Lwt

exception Unsupported of string

module Store : STORE with type 'a m = 'a Lwt.t =
struct
  (* tricky - pair of fd and current file length; blkifs
   * aren't generally supposed to change length, but baardskeerder
   * uses this (obtained with next, extended with append) to 
   * add new records. The Unix one probably would extend OK but
   * the Xen one certainly wouldn't. Either we need to use the first
   * block/sector of the file to keep the working length, or we
   * need to change baardskeerder to do this in its own metadata
   * blocks.
   *
   * We will also most likely need some kind of cache, as we can 
   * only do writes of pages (4096) which I think are larger than
   * BS's. *)
  type page = {
    pageno : int;
    mutable data : Cstruct.t list
  }
  type t = { 
    blkif : OS.Devices.blkif;
    cache : page option array
  }

  let cachesize = 20

  type 'a m = 'a Lwt_.t
  let bind = Lwt_.bind
  let return = Lwt_.return
  let fail = Lwt_.fail
  let catch = Lwt_.catch

  let (>>=) = bind

  let info x = Printf.printf "%s\n%!" x
  let debug x = () (*Printf.printf "%s\n%!" x*)

  let init name =
    info("Baardskeerder_mirage.Stores.Blkif.init "^name);
    OS.Devices.find_blkif name >>= fun oblkif ->
    match oblkif with
    | None -> Lwt.fail (Failure ("init could not find blkif " ^ name))
    | Some blkif -> return {
        blkif; 
        cache=Array.create cachesize None
      }

  let close ({blkif;_}) =
          info("blkif.close");
          (*blkif#destroy ;*)
          return ()

  (* missing on xen: OS.Io_page.round_to_page_size 1 *)
  let page_size = 4096

  (** from blkif interface - read_512! *)
  let sectorsize = 512
  let sectors_per_page = page_size / sectorsize

  (* helper fn - choose cache slot to use/eject *)
  let get_empty_cache_slot cache pno = 
    if pno==0 then 0 
    else begin
      let highest = match (Array.get cache 1) with 
      | None -> true 
      | Some {pageno;_} -> (pno>=pageno) in
      if (highest) then 1
      (* TODO - use the full set of cache slots - LRU? *)
      else 2
    end 

  (* helper fn - find page in cache *)
  let get_cache_page cache pno =
    let find_page ox el = match ox with
    | Some _ -> ox
    | None -> begin match el with
      | Some page -> if page.pageno==pno then Some page else None
      | None -> None
      end in
    Array.fold_left find_page None cache

  (* helper fn - read one page from cache calling blitfn *)
  let read_from_cache (page:page) (blitfn: Cstruct.t -> int -> int -> int -> unit) blitzero blitlen =
    let start = page.pageno*page_size in
    debug (Printf.sprintf "read_from_cache p=%d to @%d+%d" page.pageno blitzero blitlen);
    (* note: cstructs are in reverse order! *) 
    let rec rread data lastoffset =
      match data with 
      | [] -> ()
      | cstruct :: data' -> begin
          let size = Cstruct.len cstruct in
          let firstoffset = lastoffset-size in
          let minoffset = max blitzero firstoffset in
          let maxoffset = min (blitzero+blitlen) lastoffset in
          let cnt = maxoffset-minoffset in
          if cnt>0 then begin
            debug (Printf.sprintf "- blit %d bytes cstruct+%d -> buf+%d" cnt (minoffset-firstoffset) (minoffset-blitzero));
            blitfn cstruct (minoffset-firstoffset) cnt (minoffset-blitzero)
          end else debug (Printf.sprintf "- !zero blit cstruct %d-%d buf %d-%d" firstoffset lastoffset blitzero (blitzero+blitlen));
          rread data' firstoffset
        end
    in
    rread page.data (start+page_size)  
 
  (* helper fn - read consecutive page block from blkif and
     populate cache and call blitfn via read_from_cache for each *)
  let read_new_pages blkif cache pagefrom pageto blitfn blitzero blitlen = 
    let sectorfrom = sectors_per_page*pagefrom
    and sectorto = (sectors_per_page*(pageto+1))-1 in
    debug (Printf.sprintf "read_new_pages %d-%d to @%d+%d" pagefrom pageto blitzero blitlen);
    let cstructs = blkif#read_512 (Int64.of_int sectorfrom)
      (Int64.of_int (sectorto+1-sectorfrom)) in
    let rec rreadcstructs pno ppos data =
      (* get next cstruct (if any) *)
      Lwt_stream.get cstructs >>= fun co -> match co with
      | None -> return ()
      | Some cstruct ->
        let max = Cstruct.len cstruct in
        rreadcstruct cstruct 0 max pno ppos data 
    and rreadcstruct cstruct cpos cmax pno ppos data = begin 
      (* add available cstruct bytes to current page data *)
      let cnt = max (cmax-cpos) (page_size-ppos) in
      let data' = if cnt>0 then 
        (Cstruct.sub cstruct cpos cnt) :: data
      else data in
      let ppos' = ppos+cnt in
      let (data'',pno'',ppos'') = 
        if ppos' < page_size then (data',pno,ppos') else begin
          (* complete cache page read! *)
          let page = {pageno=pno; data=data'} in
          (* find/make empty cache slot *)
          let cix = get_empty_cache_slot cache pno in
          debug (Printf.sprintf "cache new page %d in slot %d" pno cix);
          Array.set cache cix (Some page);
          read_from_cache page blitfn blitzero blitlen;
          ([],pno+1,0)
        end
      in
      let cpos' = cpos+cnt in
      if cpos'<cmax then 
        rreadcstruct cstruct cpos' cmax pno'' ppos'' data'' 
      else
        rreadcstructs pno'' ppos'' data''
    end in
    rreadcstructs pagefrom 0 []

  let read ({blkif;cache}) offset length =
    debug (Printf.sprintf "Blkif.read %d+%d" offset length); 
    let outstring = String.create length in
    if length==0 then return outstring else
    let pagesize = page_size in
    let pagefrom = offset / pagesize in
    let pageto = (offset+length-1) / pagesize in
    (* helper fn - handle cache hit *)
    let blitfn cstruct cpos clen dpos = 
      Cstruct.blit_to_string cstruct cpos outstring dpos clen in
    let read_from_cache page = read_from_cache page blitfn offset length in
    (* helpder fn - handle multi-page cache miss *)
    let read_new_pages pagefrom pageto = read_new_pages blkif cache pagefrom pageto blitfn offset length in
    (* work through pages, satisfying from cache or accumulating
       a maximal block read *)
    let rec rread_page (pno:int) (opagefrom:int option) =
      if pno>pageto then 
        (* done - any last pages? *)
        match opagefrom with
        | None -> return ()
        | Some pagefrom -> read_new_pages pagefrom pageto
      else begin
        (* check cache *)
        let opage = get_cache_page cache pno in
        (* handle cache hit/miss, updating opagefrom *)
        begin match opage with
        | Some page -> begin
            (* cache hit; do any pending read *)
            begin match opagefrom with 
            | Some pagefrom -> 
              read_new_pages pagefrom (pno-1)
            | None -> return () end >>= fun () -> 
            read_from_cache page;
            return None
          end
        | None -> begin
            match opagefrom with
            (* remember for real read... *)
            | None -> return (Some pno)
            | Some _ -> return opagefrom
          end
        end >>= fun opagefrom' ->
        (* recurse - next page *)
        rread_page (pno+1) opagefrom'  
      end
    in 
    rread_page pagefrom None >>= fun () ->
    return outstring
  
  let read_one_page blkif cache (page:OS.Io_page.t) offset =
    debug (Printf.sprintf "blkif.read_one_page %d\n%!" offset);
    let pagecstruct = OS.Io_page.to_cstruct page in
    let blitfn cstruct cpos clen dpos =
          Cstruct.blit cstruct cpos pagecstruct dpos clen in
    let pno = offset/page_size in
    (* check cache *)
    let opage = get_cache_page cache pno in
    (* handle cache hit/miss, updating opagefrom *)
    match opage with
      | Some page -> (* cache hit *)
        read_from_cache page blitfn offset page_size; return ()
      | None -> 
        read_new_pages blkif cache pno pno blitfn offset page_size
    
  let write ({blkif;cache}) bufstring bufoffset buflength offset =
    debug (Printf.sprintf "write @%d+%d to %d" bufoffset buflength offset);
    (* break request down to Io_pages *)
    let pagesize = page_size in
    let pagefrom = offset / pagesize in
    let pageto = (offset+buflength-1) / pagesize in
    let rec writepage pagei =
      let pagestart = pagei*pagesize in
      let pageend = pagestart+pagesize in
      let page = OS.Io_page.get 1 in
      (if pagestart < offset || pageend > offset+buflength then
        (* need to read existing page content *)
        read_one_page blkif cache page pagestart 
      else return ()) >>= fun () ->
      (* copy in new data *)
      let srcoff = if pagestart > offset then bufoffset+pagestart-offset
        else bufoffset in
      let dstoff = if offset > pagestart then offset-pagestart else 0 in
      let len = min (pagesize-dstoff) (bufoffset+buflength-srcoff) in
      (*OS.Io_page.string_blit bufstring srcoff page dstoff len;*)
      let pagecstruct = OS.Io_page.to_cstruct page in
      Cstruct.blit_from_string bufstring srcoff pagecstruct dstoff len;
      (* update page cache *)
      let cachepage = get_cache_page cache pagei in begin
        match cachepage with None -> () 
        | Some cpage -> 
          debug (Printf.sprintf "update cache of p=%d" pagei);
          cpage.data <- pagecstruct :: []
      end;
      (* actually write *) 
      debug (Printf.sprintf "blkif.write_page %d+%d with new @%d+%d from %d"
        pagestart (OS.Io_page.length page) dstoff len srcoff);
      blkif#write_page (Int64.of_int pagestart) page >>= fun () ->
      if pagei >= pageto then return () else
      writepage (pagei+1)
    in
    writepage pagefrom

  (* no op *)
  let fsync ({blkif;_}) = 
    return ()

  (*next and append is not meaningful for block dev - will not be supported on all stores*)
  let append t buf off len : store_offset Lwt.t =
     Lwt.fail(Unsupported("blkif.append not supportable"))

  let next t : int = 
     raise (Unsupported("blkif.next not supportable"))


  (*no unix file descriptor for general block dev - will not be supported on all stores*)
  type file_descr = int

  let with_fd t (fn:file_descr -> 'a) : 'a Lwt.t =
     Lwt.fail(Unsupported("blkif.with_fd not supportable"))

  (*let run x = Lwt_main.run x*)
end
