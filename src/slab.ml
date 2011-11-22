(*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2011 Incubaid BVBA
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

open Entry
open Pos

type t = { mutable es: entry list; mutable nes: int}

let make () = { es = []; nes = 0}

let string_of_slab s = Printf.sprintf "{ es = %s; nes = %i}" 
  (Pretty.string_of_list entry2s s.es) s.nes
  
let add slab e =
  slab.es <- e :: slab.es;
  let c = slab.nes in
  slab.nes <- c + 1;
  Inner c
    
let length slab = slab.nes
  
let rev_es t= List.rev t.es

let iter_rev f slab = List.iter f (List.rev slab.es)

let mark slab = 
  let r = Array.make (slab.nes) false in
  let maybe_mark = function
    | Outer _ -> ()
    | Inner x -> r.(x) <- true
  in
  let maybe_mark2 (_,p) = maybe_mark p in
  let mark = function
    | NIL | Value _ -> ()
    | Commit p -> maybe_mark p
    | Leaf l -> List.iter maybe_mark2 l
    | Index (p0,kps) -> let () = maybe_mark p0 in List.iter maybe_mark2 kps
  in
  let () = List.iter mark slab.es in
  let () = r.(slab.nes -1) <- true in
  r

let mapping mark = 
  let s = Array.length mark in
  let h = Hashtbl.create s in
  let rec loop i o =
    if i = s then h
    else
      let v = mark.(i) in
      let i' = i + 1 in
      let () = Hashtbl.add h i o in
      let o' = if v then o + 1 else o in
      loop i' o'
  in
  loop 0 0


let compact s = 
  let s_mark = mark s in
  let s_map = mapping s_mark in
  let lookup_pos = function
    | Outer x -> Outer x
    | Inner x -> Inner (Hashtbl.find s_map x)
  in
  let rewrite_leaf kps       = List.map (fun (k,p) -> (k,lookup_pos p)) kps in
  let rewrite_index (p0,kps) = (lookup_pos p0 , rewrite_leaf kps) in
  let esa = Array.of_list (rev_es s) in
  let size = Array.length esa in
  let rec loop acc c i = 
    if i = size 
    then { es = List.rev acc; nes = c}
    else
      begin
	let i' = i + 1 in
	let a = s_mark.(i) in
	if a then
	  let e = esa.(i) in
	  let e' = match e with
	    | Leaf  l -> Leaf (rewrite_leaf l) 
	    | Index i -> Index (rewrite_index i)
	    | _       -> e
	  in
	  let acc' = e' :: acc in
	  let c' = c + 1 in
	  loop acc' c' i' 
	else
	  loop acc c i'
      end
  in
  loop [] 0 0
	
	
        
