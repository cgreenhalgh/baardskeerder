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

module Memory : STORE with type 'a m = 'a =
struct
  type t = T of Buffer.t ref
  type 'a m = 'a

  (* This is a rather ugly hack *)
  let memory_store = Hashtbl.create 16

  let bind v f = f v
  let return v = v
  let fail e = raise e
  let catch t f = try t () with e -> f e

  let init (n:string) =
    let v =
      if Hashtbl.mem memory_store n
      then Hashtbl.find memory_store n
      else
        let v = T (ref (Buffer.create 128)) in
        let () = Hashtbl.replace memory_store n v in
        v
    in
    return v

  let close _ = return ()

  let next (T b) = failwith "Store.Memory.next"

  let read (T b) o l =
    if o < Buffer.length (!b)
    then
      return (Buffer.sub !b o l)
    else
      raise End_of_file

  let write (T b) d p l o =
    let s = Buffer.contents !b in
    let sl = String.length s in

    let s' =
      if o + l < sl
      then s
      else s ^ (String.create (o + l - sl))
    in

    String.blit d p s' o l;

    let b' = Buffer.create 0 in
    Buffer.add_string b' s';

    b := b';

    return ()

  let fsync (T _) = return ()

  let run x = x
end

