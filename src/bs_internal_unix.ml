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

type offset = int
type string_offset = offset
type store_offset = offset
type length = int

module type STORE =
sig
  type t
  type 'a m

  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m
  val fail : exn -> 'a m
  val catch : (unit -> 'a m) -> (exn -> 'a m) -> 'a m
  val run : 'a m -> 'a

  val init : string -> t m
  val close : t -> unit m

  (* This will result in a string of *at least* the given length *)
  val read : t -> store_offset -> length -> string m
  (* without append, write must extend a file if the store does
   * not pre-allocate maximum size *)
  val write : t -> string -> string_offset -> length -> store_offset -> unit m

  val fsync : t -> unit m

  (*next and append is not meaningful for block dev - will not be supported on all stores*)
  val append : t -> string -> string_offset -> length -> store_offset m
  val next : t -> int

  (*no unix file descriptor for general block dev - will not be supported on all stores*)
  val with_fd : t -> (Unix.file_descr -> 'a) -> 'a m

end
