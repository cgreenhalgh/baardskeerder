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
    let d' = if p==0 && String.length d==l then d 
      else String.sub d p l in
    let bl = Buffer.length !b in
    let newbl = max bl (o+l) in
    (* over-write *)
    if o<bl then begin
      let pre = Buffer.sub !b 0 o in
      let post = if o+l<bl then 
        Buffer.sub !b (o+l) (bl-(o+l)) else "" in
      b := Buffer.create (newbl+128);
      Buffer.add_string !b pre;
      Buffer.add_string !b d';
      Buffer.add_string !b post
    end else begin
      (* special case append *)
      if o>bl then
        Buffer.add_string !b (String.create (o-bl));
      Buffer.add_string !b d'
    end;
    return ()

  let fsync (T _) = return ()

  let run x = x
end

