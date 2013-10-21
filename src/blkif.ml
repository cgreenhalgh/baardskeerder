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
  type t = T of Lwt_unix.file_descr * int ref

  type 'a m = 'a Lwt_.t
  let bind = Lwt_.bind
  let return = Lwt_.return

  let (>>=) = bind

  let init name =
    Lwt.catch
      (fun () ->
        Lwt_unix.openfile name [Lwt_unix.O_RDWR; Lwt_unix.O_CREAT;] 0o640 >>= fun fd ->
        Lwt_unix.fstat fd >>= fun stat ->
        let len = stat.st_size in

        Lwt_unix.lseek fd len Lwt_unix.SEEK_SET >>= fun i ->
        assert (i = len);

        return (T (fd, ref len))
      )
      (fun e ->
        let msg = Printf.sprintf "init %s failed with :%s" name (Printexc.to_string e) in
        Lwt.fail (Failure msg))

  let close (T (fd, _)) =
    Lwt_unix.close fd

  let next (T (_, o)) = !o

  let read (T (fd, _)) o l =
    let s = String.create l in

    let rec loop o' = function
      | 0 -> return ()
      | c ->
          Lwt_unix_ext.pread fd s o' c (o + o') >>= fun c' ->
          if c' = 0
          then
            raise End_of_file
          else
            loop (o' + c') (c - c')
    in
    loop 0 l >>= fun () ->

    return s

  let write (T (fd, _)) d p l o =
    let rec loop p' o' = function
      | 0 -> return ()
      | c ->
          Lwt_unix_ext.pwrite fd d p' c o' >>= fun c' ->
          loop (p' + c') (o' + c') (c - c')
    in
    loop p o l

  let append (T (fd, o) as t) d p l =
    let o' = !o in

    write t d p l o' >>= fun () ->
    o := o' + l;
    return o'

  (* no op *)
  let fsync (T (fd, _)) = 
    return ()

  (* this is heavily used in Flog, which basically bypasses the
   * Store API, but isn't used in Flog0, so we'll omit it and stick
   * to Flog0 *)
   let with_fd (T (fd, _)) f =
     Lwt.fail(Unsupported("blkif.with_fd not supportable"))

  let run x = Lwt_main.run x
end
