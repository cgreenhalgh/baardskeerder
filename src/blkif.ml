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

open Lwt_unix

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
  type t = T of OS.Devices.blkif

  type 'a m = 'a Lwt_.t
  let bind = Lwt_.bind
  let return = Lwt_.return
  let fail = Lwt_.fail
  let catch = Lwt_.catch

  let (>>=) = bind

  let init name =
    Lwt.catch
      (fun () ->
        Printf.printf "blkif.init %s\n%!" name;
        Blkdev.create name name >>= fun blkif ->
        return (T (blkif))
      )
      (fun e ->
        let msg = Printf.sprintf "init %s failed with :%s" name (Printexc.to_string e) in
        Lwt.fail (Failure msg))

  let close (T (blkif)) =
          Printf.printf "blkif.close\n%!";
          blkif#destroy ;
          return ()

  (** from blkif interface - read_512! *)
  let sectorsize = 512

  let read (T (blkif)) offset length =
    (*Printf.printf "blkif.read %d %d\n%!" offset length;*)
    let outstring = String.create length in
    if length==0 then return outstring else
    let sectorfrom = offset / sectorsize in
    let sectorto = (offset+length-1) / sectorsize in
    let cstructs = blkif#read_512 (Int64.of_int sectorfrom)
        (Int64.of_int (sectorto+1-sectorfrom)) in
    let rec readcstruct rdoffset outpos = 
      Lwt_stream.get cstructs >>= fun co -> match co with 
        | None -> return ()
        | Some cstruct -> 
          let max = Cstruct.len cstruct in
          let from = if rdoffset >= offset then 0 else (offset-rdoffset) in
          let len = if (rdoffset+max <= offset+length) then 
                  max-from else (offset+length-rdoffset-from) in
          (*Printf.printf "get bytes cstruct %d from %d, buf %d at %d, from=%d
          len=%d\n%!" max rdoffset length outpos from len;*)
          Cstruct.blit_to_string cstruct from outstring outpos len;
          readcstruct (rdoffset+max) (outpos+len)
    in
    readcstruct (sectorfrom*sectorsize) 0 >>= fun () ->
    return outstring
  
  let read_one_page blkif page offset =
    (*Printf.printf "blkif.read_one_page %d\n%!" offset;*)
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

  let write (T (blkif)) bufstring bufoffset buflength offset =
    (*Printf.printf "write %d@%d %d\n%!" buflength bufoffset offset;*)
    (* break request down to Io_pages *)
    let pagesize = OS.Io_page.round_to_page_size 1 in
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
      (*Printf.printf "blkif.write_page %d len %d with new %d+%d from %d\n%!"
      pagestart (OS.Io_page.length page) dstoff len srcoff;*)
      blkif#write_page (Int64.of_int pagestart) page >>= fun () ->
      if pagei >= pageto then return () else
      writepage (pagei+1)
    in
    writepage pagefrom

  (* no op *)
  let fsync (T (blkif)) = 
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

  let run x = Lwt_main.run x
end
