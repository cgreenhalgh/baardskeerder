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
    data : Cstruct.t list
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

  let debug = false

  let init name =
    OS.Console.log ("Baardskeerder_mirage.Stores.Blkif.init "^name);
    OS.Devices.find_blkif name >>= fun oblkif ->
    match oblkif with
    | None -> Lwt.fail (Failure ("init could not find blkif " ^ name))
    | Some blkif -> return {
        blkif; 
        cache=Array.create cachesize None
      }

  let close ({blkif;_}) =
          OS.Console.log ("blkif.close");
          (*blkif#destroy ;*)
          return ()

  (** from blkif interface - read_512! *)
  let sectorsize = 512
  let sectors_per_page = page_size / sectorsize

  let read ({blkif;cache}) offset length =
    if debug then OS.Console.log (Printf.sprintf "Blkif.read %d %d" offset length); 
    let outstring = String.create length in
    if length==0 then return outstring else
    let pagesize = page_size in
    let pagefrom = offset / pagesize in
    let pageto = (offset+buflength-1) / pagesize in
    (* helper fn - handle cache hit *)
    let read_from_cache page =
      let start = page.pageno*page_size in
      (* note: cstructs are in reverse order! *) 
      let rec rread data lastoffset =
        match data with 
        | [] -> ()
        | cstruct :: data' -> begin
            let size = Cstruct.length cstruct in
            let firstoffset = lastoffset-size in
            let minoffset = max offset firstoffset in
            let maxoffset = min (offset+length) lastoffset in
            let cnt = maxoffset-minoffset in
            if cnt>0 then
              Cstruct.blit_to_string cstruct (minoffset-firstoffset) 
                outstring (minoffset-offset) cnt
          end
      in
      rread page.data (start+page_size)  
    in
    (* helper fn - choose cache slot to use/eject *)
    let get_empty_cache_slot pno = 
      (* TODO - use the full set of cache slots; page 0 in 0; rest LRU? *)
      return (if pno==0 then 0 else 1) in
    (* helpder fn - handle multi-page cache miss *)
    let read_new_pages pagefrom pageto = 
      let sectorfrom = sectors_per_page*pagefrom
      and sectorto = (sectors_per_page*(pageto+1))-1 in
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
          if ppos' < page_size then (data',pno',ppos') else
          (* complete cache page read! *)
          let page = {pageno=pno'; data=data'} in
          (* find/make empty cache slot *)
          let cix = get_empty_cache_slot pno in
          Array.set cache cix page;
          read_from_cache page;
          ([],pno'+1,0)
        in
        if cpos<cmax then 
          rreadcstruct cstruct (cpos+cnt) cmax pno'' ppos'' data'' 
        else
          rreadcstructs pno'' ppos'' data''
      in
      rreadcstructs pagefrom 0 []
    in
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
        let find_page ox el = match ox with
          | Some _ -> ox
          | None -> begin match el with
            | Some page -> if page.pageno==pno then Some page else None
            | None -> None
            end in
        let opage = Array.fold_left find_page None cache in
        (* handle cache hit/miss, updating opagefrom *)
        lwt opagefrom' = match opage with
        | Some page -> begin
            (* cache hit; do any pending read *)
            lwt _ = match opagefrom with 
            | Some pagefrom -> 
              lwt _ = read_new_pages pagefrom (pno-1)
            | None -> return () in
            lwt _ = read_from_cache page in
            return None
          end
        | None -> begin
            match opagefrom with
            (* remember for real read... *)
            | None -> Some pno
            | Some _ -> opagefrom
          end
        in
        (* recurse - next page *)
        rread_page (pno+1) opagefrom'  
    in 
    lwt _ = rread_page pagefrom None in
    return outstring
  
  let read_one_page blkif page offset =
    if debug then Printf.printf "blkif.read_one_page %d\n%!" offset;
    (* TODO check cache; fill cache *)
    let sectoroffset = offset / sectorsize in
    let sectorlength = (OS.Io_page.length page) / sectorsize in
    let cstructs = blkif#read_512 (Int64.of_int sectoroffset) (Int64.of_int
    sectorlength) in
    let pagecstruct = OS.Io_page.to_cstruct page in
    let rec readcstruct pagepos =
      Lwt_stream.get cstructs >>= fun co -> match co with
        | None -> return ()
        | Some cstruct ->
          let len = Cstruct.len cstruct in
          (* assuming pages are multiple of sectors! *)
          Cstruct.blit cstruct 0 pagecstruct pagepos len;
          readcstruct (pagepos+len)
    in
    readcstruct 0

  (* missing on xen: OS.Io_page.round_to_page_size 1 *)
  let page_size = 4096

  let write ({blkif;_}) bufstring bufoffset buflength offset =
    if debug then Printf.printf "write %d@%d %d\n%!" buflength bufoffset offset;
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
        read_one_page blkif page pagestart 
      else return ()) >>= fun () ->
      (* copy in new data *)
      let srcoff = if pagestart > offset then bufoffset+pagestart-offset
        else bufoffset in
      let dstoff = if offset > pagestart then offset-pagestart else 0 in
      let len = min (pagesize-dstoff) (bufoffset+buflength-srcoff) in
      OS.Io_page.string_blit bufstring srcoff page dstoff len;
      if debug then Printf.printf "blkif.write_page %d len %d with new %d+%d from %d\n%!"
      pagestart (OS.Io_page.length page) dstoff len srcoff;
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
