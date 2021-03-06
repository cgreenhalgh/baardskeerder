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

open Store

open Base
open Entry
open Pack

exception Unsupported of string

module Flog0 =
  functor(S:Bs_internal.STORE) ->
struct

  type 'a m = 'a S.m
  let bind = S.bind
  let return = S.return
  let fail = S.fail
  let catch = S.catch
  (*let run = S.run*)

  let (>>=) = bind
  module M = Monad.Monad(S)


  let time_to b (t:Time.t) =
    let (x,y,g) = t in
    Pack.vint64_to b x;
    Pack.vint_to b y;
    Pack.bool_to b g


  let input_kp input =
    let k = Pack.input_string input in
    let s = Pack.input_vint input in
    let p = Pack.input_vint input in
    k, Outer (Spindle s, Offset p)


  let pos_to b = function
    | Outer (Spindle s, Offset o) -> Pack.vint_to b s; Pack.vint_to b o
    | Inner _ -> failwith "cannot serialize inner pos"

  let kp_to b (k,p) =
    Pack.string_to b k;
    pos_to b p


  let input_time input =
    let x = Pack.input_vint64 input in
    let y = Pack.input_vint input in
    let g = Pack.input_bool input in
    Time.make x y g


  let _METADATA_SIZE = 4096
  type metadata = {
    commit : (spindle * offset);
    td: int;
    t0: Time.t;
  }

  let metadata2s m =
    let (Spindle s, Offset o) = m.commit in
    Printf.sprintf "{commit=(%i,%i);td=%i;t0=%s}" s o m.td (Time.time2s m.t0)



  let _write_metadata fd m =
    let b = Buffer.create 128 in
    let (Spindle s, Offset o) = m.commit in
    let () = Pack.vint_to b s in
    let () = Pack.vint_to b o in
    let () = Pack.vint_to b m.td in
    let () = time_to b m.t0 in
    let block = String.create _METADATA_SIZE in

    let () = Buffer.blit b 0 block 0 (Buffer.length b) in

    S.write fd block 0 _METADATA_SIZE 0

  exception Uninitialised

  let is_uninitialised s =
    let len = String.length s in
    let rec check s i = 
      if i >= len then true 
      else if (String.get s i) != '\000' then false
      else check s (i+1) in
    check s 0

  let _read_metadata fd =
    S.read fd 0 _METADATA_SIZE >>= fun m ->
    if is_uninitialised m then 
      S.fail Uninitialised else S.return () >>= fun () ->
    let input = Pack.make_input m 0 in
    let s = Pack.input_vint input in
    let o = Pack.input_vint input in
    let commit = (Spindle s, Offset o) in
    let td = Pack.input_vint input in
    let t0 = input_time input in
    return {commit; td;t0}

  type t = {
    spindles : (S.t * int ref) array;
    start : Time.t;
    mutable last: (spindle * offset);
    mutable next_spindle: int;
    mutable d: int;
    mutable now: Time.t;
    filename : string;
  }

  let get_d t = t.d

  let t2s t =
    let (Spindle ls, Offset lo) = t.last in
    let (sp,spnext) = Array.get t.spindles t.next_spindle in
    Printf.sprintf "{...;last=(%i,%i); next=(%i,%i);now=%s}" ls lo
      t.next_spindle !spnext (Time.time2s t.now)

  let last t = let s, o = t.last in Outer (s, o)



  let now t = t.now


  let close t =
    let meta = {commit = t.last; td = t.d; t0 = t.start} in
    M.iter_array (fun (s,snext) -> _write_metadata s meta >>= fun () -> S.close s) t.spindles

  let clear t =
    let commit = (Spindle 0, Offset 0) in
    let meta = {commit;
                td = t.d;
                t0 = Time.zero;
               }
    in
    M.iter_array (fun (s,snext) -> 
      _write_metadata s meta >>= fun () ->
      (* Note: also need to zero start of block 1 for size-sniffing algorithm.
         (Could truncate a file, but this may not be a file) *)
      let s0 = String.create 4 in S.write s s0 0 (String.length s0) _METADATA_SIZE
    ) t.spindles >>= fun () ->
    t.last  <- commit;
    t.next_spindle <- 0;
    t.now  <- Time.zero;

    return ()


  type tag =
    | COMMIT
    | LEAF
    | INDEX
    | VALUE

  let tag_to b = function
    | COMMIT -> Buffer.add_char b '\x01'
    | LEAF   -> Buffer.add_char b '\x02'
    | INDEX  -> Buffer.add_char b '\x03'
    | VALUE  -> Buffer.add_char b '\x04'

  open Pack
  let input_tag input =
    let tc = input.s.[input.p] in
    let () = input.p <- input.p + 1 in
    match tc with
      | '\x01' -> COMMIT
      | '\x02' -> LEAF
      | '\x03' -> INDEX
      | '\x04' -> VALUE
      | _      -> let s = Printf.sprintf "%C tag?" tc in failwith s


  let inflate_action input =
    let t = input_char input in
    match t with
      | 'D' -> let k = input_string input in
               Commit.CDelete k
      | 'S' -> let k = input_string input in
               let s = input_vint input in
               let p = input_vint input in
               Commit.CSet (k, Outer (Spindle s, Offset p))
      | t   -> let s = Printf.sprintf "%C action?" t in failwith s



  let inflate_pos input =
    let s = Pack.input_vint input in
    let p = Pack.input_vint input in
    Outer (Spindle s, Offset p)

  let inflate_commit input =
    let pos = inflate_pos input in
    let previous = inflate_pos input in
    let lookup = inflate_pos input in
    let t = input_time input in
    let actions = Pack.input_list input inflate_action  in
    let explicit = Pack.input_bool input in
    Commit.make_commit ~pos ~previous ~lookup t actions explicit


  let inflate_value input = input_string input

  let input_suffix_list input =
    let prefix = input_string input in
    let suffixes = input_list input input_kp  in
    let kps = List.map (fun (s,p) -> (prefix ^s, p)) suffixes in
    kps

  let inflate_leaf input = input_suffix_list input


  let inflate_index input =
    let s0 = input_vint input in
    let p0 = input_vint input in
    let kps = input_suffix_list input in
    Outer (Spindle s0, Offset p0), kps

  let input_entry input =
    match input_tag input with
      | COMMIT -> Commit (inflate_commit input)
      | LEAF   -> Leaf (inflate_leaf input)
      | INDEX  -> Index (inflate_index input)
      | VALUE  -> Value (inflate_value input)

  let inflate_entry es =
    let input = make_input es 0 in
    input_entry input



  let dump ?(out=Pervasives.stdout) (t:t) =
    let d (s,snext) =
      _read_metadata s >>= fun m ->
      Printf.fprintf out "meta: %s\n%!" (metadata2s m);
      Printf.fprintf out "next: %d\n%!" !snext;
      let rec loop pos=
        S.read s pos 4 >>= fun ls ->
        let l = size_from ls 0 in
        if (l==0) then S.fail End_of_file else 
        S.read s (pos + 4) l >>= fun es ->
        let e = inflate_entry es in
        let () = Printf.fprintf out "%4i : %s\n%!" pos (Entry.entry2s e)  in
        let pos' = pos + 4 + String.length es in
        loop pos'
      in
      S.catch
        (fun () -> loop _METADATA_SIZE)
        (fun exc -> match exc with End_of_file -> return () | exc -> S.fail exc)
    in

    M.iteri_array
      (fun i s ->
        Printf.fprintf out "Spindle %02i\n" i;
        Printf.fprintf out "----------\n";
        d s)
      t.spindles


  let _add_buffer b mb =
    let l = Buffer.length mb in
    size_to b l;
    Buffer.add_buffer b mb;
    l + 4

  let deflate_value b _ v =
    let l = String.length v in
    let mb = Buffer.create (l+5) in
    tag_to mb VALUE;
    string_to mb v;
    _add_buffer b mb

  let pos_remap mb h p =
    let (s, o) = match p with
      | Outer (Spindle s, Offset o) -> (s, o)
      | Inner x when x = -1-> (0,0) (* TODO: don't special case with -1 *)
      | Inner x ->
          let (Spindle s, Offset o) = Hashtbl.find h x in
          (s, o)
    in
    vint_to mb s;
    vint_to mb o

  let kps_to mb h kps =
    let px = Leaf.shared_prefix kps in
    let pxs = String.length px in
    string_to mb px;
    let l = List.length kps in
    vint_to mb l;
    List.iter (fun (k,p) ->
      let suffix = String.sub k pxs (String.length k - pxs) in
      string_to mb suffix;
      pos_remap mb h p)
      kps


  let deflate_index b h (p0, kps) =
    let mb = Buffer.create 128 in
    tag_to mb INDEX;
    pos_remap mb h p0;
    kps_to mb h kps;
    _add_buffer b mb


  let deflate_leaf b h kps =
    let mb = Buffer.create 128 in
    tag_to mb LEAF;
    kps_to mb h kps;
    _add_buffer b mb


  let deflate_action b h = function
    | Commit.CSet (k,p) ->
        Buffer.add_char b 'S';
        string_to b k;
        pos_remap b h p
    | Commit.CDelete k ->
        Buffer.add_char b 'D';
        string_to b k

  let deflate_commit b h c =
    let mb = Buffer.create 8 in
    tag_to mb COMMIT;
    let pos = Commit.get_pos c in
    let previous = Commit.get_previous c in
    let lookup = Commit.get_lookup c in
    let t = Commit.get_time c in

    let actions = Commit.get_cactions c in
    let explicit = Commit.is_explicit c in
    pos_remap mb h pos;
    pos_remap mb h previous;
    pos_remap mb h lookup;
    time_to mb t;
    vint_to mb (List.length actions);
    List.iter (fun a -> deflate_action mb h a) actions;
    bool_to mb explicit;
    _add_buffer b mb

  let deflate_entry (b:Buffer.t) h (e:entry) =
    match e with
      | NIL      -> failwith "NIL?"
      | Value v  -> deflate_value  b h v
      | Index i  -> deflate_index  b h i
      | Leaf l   -> deflate_leaf   b h l
      | Commit c -> deflate_commit b h c


  let write log slab =
    let sl = Slab.length slab in
    let h = Hashtbl.create sl in

    let l = Array.length log.spindles in
    let sid = ref log.next_spindle in
    let buffers = Array.init l (fun _ -> Buffer.create 1024) in
    let starts = Array.init l (fun i -> match (Array.get log.spindles i) with
    _,snext -> let snext' = !snext in ref snext') in

    let do_one i e =
      let sid' = !sid in
      let start = Array.get starts sid' in

      let size = deflate_entry (Array.get buffers sid') h e in
      let () = Hashtbl.replace h i (Spindle sid', Offset !start) in

      let () = start := !start + size in

      sid := (sid' + 1) mod l
    in
    let () = Slab.iteri slab do_one  in

    M.iteri_array
      (fun i b ->
        let ss = Buffer.contents b in
        let slen = String.length ss
        and s,snext = Array.get log.spindles i in
        let offset = !snext in
        (*let n = !(Array.get starts i) in*)

        snext := offset + slen;
        S.write s ss 0 slen offset >>= fun _ ->
        return ())
      buffers >>= fun () ->

    let cp = Hashtbl.find h (sl -1) in
    log.last <- cp;
    log.next_spindle <- !sid;
    log.now <- Slab.time slab;
    return ()


  let sync t =
    M.iter_array (fun (s,_) -> S.fsync s) t.spindles

  let compact ?(min_blocks=1) ?(progress_cb=None) (_:t) =
    ignore min_blocks;
    ignore progress_cb;
    failwith "todo"

  let _read_entry_s fd pos =
    S.read fd pos 4 >>= fun ls ->
    let l = size_from ls 0 in
    S.read fd (pos + 4) l

    (*
      module C = Map.Make(struct
      type t = (int*int)
      let compare = Pervasives.compare

      end)
      module Cache = struct
      let _cache = ref C.empty
      let _size = ref 0

      let add so e =
      let () =
      if !_size = 1000
      then
      begin
      let k,_ = C.min_binding !_cache in
      _cache := C.remove k !_cache;
      decr _size
      end
      in
      _cache := C.add so e !_cache;
      incr _size

      let find so =
      try
      let e = C.find so !_cache in
    (* let () = Printf.eprintf "." in *)
      Some e

      with
      | Not_found -> None
      end
    *)

  let read t pos =
    match pos with
      | Outer (Spindle s, Offset o) ->
          if ((s = 0) && (o = 0))
          then return NIL
          else
            begin
            (*let so = (s,o) in
              match Cache.find so with
              | Some e -> return e
              | None ->*)
              begin
                let sp,_ = Array.get t.spindles s in
                _read_entry_s sp o >>= fun es ->
                let entry = inflate_entry es in
              (* let () = Cache.add so entry in *)
                return entry
              end
            end
      | Inner _ -> failwith "cannot read inner"


  let lookup t =
    let p = last t in
    read t p >>= function
      | Commit c -> let lu = Commit.get_lookup c in return lu
      | e -> failwith "can only do commits"

  let _try_read_metadata sp =
    let handle ex = match ex with 
      | End_of_file
      | Uninitialised -> return {commit=(Spindle 0,Offset 0);td=0;t0=Time.zero}
      | _ -> raise ex in
    let task () = _read_metadata sp in
    S.catch task handle

  let init ?(d=8) fn t0 =
    S.init fn >>= fun s ->
    _try_read_metadata s >>= fun meta ->
    let (Spindle ms, Offset mo) = meta.commit in
    if ms==0 && mo==0 && meta.t0==Time.zero 
    then
      let commit = (Spindle 0, Offset 0) in
      _write_metadata s {commit;td = d; t0} >>= fun () ->
      S.close s
    else
      S.close s >>= fun () ->
    let s = Printf.sprintf "%s already exists" fn in
    failwith s


  let make filename =
    S.init filename >>= fun s ->

    let spindles = Array.make 1 (s,ref 0) in

    let (sp0,spl0) = Array.get spindles 0 in

    _read_metadata sp0 >>= fun m ->
    let d = m.td in
    let _try_read_entry sp0 tbr =
      let handle ex = match ex with 
        | End_of_file -> return None
        | _ -> raise ex in
      let task () =
        _read_entry_s sp0 tbr >>= fun es ->
        if (String.length es)==0 then return None 
        else return (Some es) in
      S.catch task handle
    in
    let rec _scan_forward (ltc,ltt,ltn) (tbr:int) =
        begin
          _try_read_entry sp0 tbr >>= fun eso ->
          match eso with
            | Some es ->
                begin
                  let input = make_input es 0 in
                  let e = input_entry input in
                  let next = tbr + 4 + String.length es in
                  let lt' = 
                    match e with
                      | Commit c ->
                        let time = Commit.get_time c in
                        (Spindle 0, Offset tbr), time, next
                      | e -> (ltc, ltt, next)
                  in
                  _scan_forward lt' next

                end
            | None -> S.return (ltc,ltt,ltn)
        end
    in
    let (_,Offset tbr) = m.commit in
    let corrected_tbr = max _METADATA_SIZE tbr in

    _scan_forward ((Spindle 0, Offset 0),m.t0,corrected_tbr) corrected_tbr >>=
            fun (last, now, next) ->
    (* update next *)
    spl0 := next;
    let flog0 = { spindles=spindles;
                  last=last;
                  next_spindle=0;
                  d=d;
                  now = now;
                  start = m.t0;
                  filename;
                }
    in
    return flog0

   let set_metadata t s = failwith "Flog0.set_metadata"
   let unset_metadata t = failwith "Flog0.unset_metadata"
   let get_metadata t = failwith "Flog0.get_metadata"

end (* module / functor *)
