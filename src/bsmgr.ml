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

open Unix
open Tree
open Arg
open Log
open Dbx
open Sync



type progress_callback = int -> unit

type command =
  | Bench
  | Dump
  | DumpStream
  | Rewrite
  | Punch
  | Info
  | Test
  | ListTest
  | OnlyTest
  | Hudson
  | Help

module type RunLog = 
sig
  include Log.LOG
  val run : 'a m -> 'a
end

let mirage_run = ref false
let providers = Hashtbl.create 8
open Lwt

(* mirage blkif helpder *)
let register_blkif fn frominit : unit Lwt.t =
  OS.Console.log ("register_blkif "^fn);
  let added = try Hashtbl.find providers fn
    with Not_found -> false in
  if (added) then
    Lwt.return()
  else begin
    Hashtbl.add providers fn true;
    OS.Console.log ((if frominit then "create" else "open")^" file "^fn);
    Lwt_unix.openfile fn [Lwt_unix.O_RDWR; Lwt_unix.O_CREAT] 0o666 >>=
    fun fd -> Lwt_unix.close fd >>= fun () ->
    OS.Console.log ("add_provider "^fn);
    Blkdev.add_provider fn fn >>= fun () ->
    if (!mirage_run)==false then begin
      mirage_run := true;
      OS.Console.log "run Mirage main";
      Lwt.async (fun () -> OS.Main.run (return ()); return ())
    end;
    Lwt.return ()
  end

let get_log = function 
 | ("Memory","Flog0") -> 
   let module L = Flog0.Flog0(Store.Memory) in
   (module struct
     include L
     let run x = x
   end : RunLog)
 | ("Sync","Flog0") ->
   let module L = Flog0.Flog0(Store_unix.Sync) in
   (module struct
     include L
     let run x = x
   end : RunLog)
 | ("Lwt","Flog0") ->
   let module L = Flog0.Flog0(Store_unix.Lwt) in
   (module struct
     include L 
     let run x = Lwt_main.run x
   end : RunLog)
 | ("Sync","Flog") ->
   let module L = Flog.Flog(Store_unix.Sync) in
   (module struct
     include L
     let run x = x
   end : RunLog)
 | ("Lwt","Flog") ->
   let module L = Flog.Flog(Store_unix.Lwt) in
   (module struct
     include L
     let run x = Lwt_main.run x
   end : RunLog)
 | ("Blkif","Flog0") ->
   let module L = Flog0.Flog0(Blkif.Store) in
   (module struct
     (*include L*)
     type t = L.t
     type 'a m = 'a L.m
     let bind = L.bind
     let return = L.return
     let init ?(d=8) fn t0 = 
	Lwt.bind (register_blkif fn true) (fun () -> L.init ~d:d fn t0)
     let write = L.write
     let last = L.last
     let lookup = L.lookup
     let read = L.read
     let sync = L.sync
     let make fn = 
        Lwt.bind (register_blkif fn false) (fun () -> L.make fn)
     let close = L.close
     let clear = L.clear
     let get_d = L.get_d
     let now = L.now
     let dump = L.dump
     let compact = L.compact
     let set_metadata = L.set_metadata
     let get_metadata = L.get_metadata
     let unset_metadata = L.unset_metadata

     let run x = Lwt_main.run x
   end : RunLog)
 | (s,n) -> failwith ("Unsupported combination store/log "^s^" "^n)

let () =
  let command = ref Help in

  let dump () = command := Dump in
  let dump_stream () = command := DumpStream in
  let rewrite () = command := Rewrite in
  let punch () = command:= Punch in
  let info () = command:= Info in
  let test () = command:= Test in
  let list_test() = command:=ListTest in
  let hudson () = command := Hudson in
  let only_test () = command := OnlyTest in
  let benchmark () = command := Bench in
  let n  = ref 1_000_000 in
  let m  = ref 100 in
  let vs = ref 2_000 in
  let fn = ref "test.db" in
  let fn2 = ref "test_compacted.db" in
  let d = ref 4 in
  let log_name = ref "Flog0" in
  let store_name = ref "Memory" in
  let mb = ref 1 in
  let test_refs = ref [] in
  let spec = [
    ("--value-size",Set_int vs, Printf.sprintf "size of the values in bytes (%i)" !vs);
    ("--file", Set_string fn, Printf.sprintf "file name for database (%S)" !fn);
    ("--bench-size",Set_int n,  Printf.sprintf "number of sets/gets/deletes (%i)" !n);
    ("--d", Set_int d, Printf.sprintf "1/2 of the fan-out (%i)" !d);
    ("--log-name", Set_string log_name, Printf.sprintf "name of the log implementation (%s)" !log_name);
    ("--store-name", Set_string store_name, Printf.sprintf "name of the store implementation (%s)" !store_name);
    ("--min-blocks", Set_int mb, Printf.sprintf
       "minimal number of consecutive blocks to punch (%i)" !mb);
    ("--dump", Unit dump, Printf.sprintf "doesn't run a benchmark, but dumps file's contents");
    ("--dump-stream", Unit dump_stream, Printf.sprintf "dumps the stream of updates");
    ("--rewrite", Unit rewrite, "rewrite the log into another file");
    ("--punch", Unit punch, "compact the log file through hole punching");
    ("--file2" , Set_string fn2, Printf.sprintf "name of the compacted log file (%s)" !fn2);
    ("--info", Unit info, Printf.sprintf "returns information about the file (%s)" !fn);
    ("--list-test", Unit list_test, Printf.sprintf "lists tests");
    ("--only-test", Tuple[Unit only_test; Arg.String (fun str -> test_refs := str :: ! test_refs)], "runs some tests");
    ("--test", Unit test, "runs testsuite");
    ("--hudson", Unit hudson, "runs testsuite with output suitable for hudson");
    ("--benchmark",Unit benchmark, "runs benchmark");
  ]
  in
  let usage_msg = "simple baardskeerder tester driver and benchmark" in
  let () = Arg.parse spec (fun _ ->()) usage_msg in
  let log = get_log (!store_name, !log_name) in
  let module MyLog = (val log : RunLog) in 
  let module MyDB  = DB(MyLog) in
  let module MyDBX = DBX(MyLog) in
  let module MySync = Sync(MyLog) in
  let (>>=) = MyLog.bind in
  let return = MyLog.return in
  let make_key i = Printf.sprintf "key_%08i" i in

  let set_loop db vs n (cb: progress_callback) =
    let v = String.make vs 'x' in
    let set k v = MyDB.set db k v in
    let rec loop i =
      let () = cb i in
      if i = n
      then MyLog.sync db
      else
        let key = make_key i in
        set key v >>= fun () ->
        loop (i+1)
    in
    loop 0
  in

  let get_loop db n (cb: progress_callback) =
    let now = MyLog.now db in
    let empty = Slab.make now in
    let get k = MyDB._get db empty k in
    let rec loop i =
      let () = cb i in
      if i = n
      then return ()
      else
        let key = make_key i in
        get key >>= fun v ->
        loop (i+1)
    in
    loop 0
  in

  let delete_loop db n (cb: progress_callback) =
    let delete k = MyDB.delete db k in
    let rec loop i =
      let () = cb i in
      if i = n
      then MyLog.sync db
      else
        let key = make_key i in
        delete key >>= function
        | Base.OK () -> loop (i+1)
        | Base.NOK k -> failwith (Printf.sprintf "%s not found" k)
    in
    loop 0
  in

  let set_tx_loop db vs m n (cb: progress_callback)=
    let v = String.make vs 'x' in
    let set_tx b =
      let f tx =
        let rec loop i =
          let kn = b+ i in
          if i = m || kn >= n
          then return (Base.OK ())
          else
            let k = make_key kn in
            MyDBX.set tx k v >>= fun () ->
            loop (i+1)
        in
        loop 0
      in
      MyDBX.with_tx db f
    in
    let rec loop i =
      let () = cb i in
      if i >= n
      then MyLog.sync db
      else
        set_tx i >>= function
        | Base.OK () -> loop (i+m)
        | Base.NOK k -> failwith (Printf.sprintf "NOK %s" k)
    in
    loop 0
  in
  let () =
    let run x = MyLog.run x in
    match !command with
    | Help -> Arg.usage spec usage_msg
    | Test -> let _ = OUnit.run_test_tt_main Test.suite in ()
    | ListTest ->
      List.iter
        (fun pth -> print_endline (OUnit.string_of_path pth))
        (OUnit.test_case_paths Test.suite);
      ()
    | OnlyTest ->
      let nsuite =
        match OUnit.test_filter ~skip:true !test_refs Test.suite with
        | Some test -> test
        | None ->
          failwith ("Filtering test "^
                    (String.concat ", " !test_refs)^
                    " lead to no test")
      in
      let _ = OUnit.run_test_tt nsuite in ()
    | Hudson -> (Hudson_xml.run_test Test.suite)
    | Dump ->
      let t =
        begin
          MyLog.make !fn >>= fun db ->
          MyLog.dump db >>= fun () ->
          MyLog.close db
        end
      in
      run t
    | DumpStream ->
      let t =
        begin
          MyLog.make !fn >>= fun log ->
          let f () t a =
            Printf.printf "%s\t: %s\n%!" (Time.time2s t) (Commit.caction2s a) ;
            ()
          in
          let t0 = Time.zero in
          MySync.fold_actions t0 f () log >>= fun () ->
          MyLog.close log
        end
      in
      run t
    | Info ->
      let t =
        begin
          MyLog.make !fn >>= fun log ->
          let now = MyLog.now log in
          let empty = Slab.make now in
          MyDB.depth log empty >>= fun depth ->
          let last = MyLog.last log in
          let now = MyLog.now log in
          let () = Printf.printf "d:\t%i\n" (MyLog.get_d log) in
          let () = Printf.printf "depth:\t%i\n" depth in
          let () = Printf.printf "last:\t%s\n" (Pos.pos2s last) in
          let () = Printf.printf "now:\t%s\n" (Time.time2s now) in
          MyLog.close log
        end
      in
      run t
    | Rewrite ->
      failwith "needs fixing -sorry"
      (*Not sure how to make this play with change of functors*)
      (*let t =
        begin
          let module MyRewrite = Rewrite.Rewrite(MyF)(MyF)(MyStore) in
          MyLog.make !fn >>= fun l0 ->
          let now0 = MyLog.now l0 in
          let (x0,y0,g0) = now0 in
          let now0' = Time.make x0 y0 true in
          MyLog.init !fn2 now0' >>= fun () ->
          MyLog.make !fn2 >>= fun l1 ->
          MyRewrite.rewrite l0 (MyLog.last l0) l1 >>= fun _ ->
          MyLog.close l0 >>= fun () ->
          MyLog.close l1
        end
      in
      run t*)
    | Punch ->
      let t =
        begin
          MyLog.make !fn >>= fun l0 ->
          let cb =
            let l = ref 0 in
            fun (Pos.Offset t) (Pos.Offset d) ->
              if !l = 0 then (l := t) else ();
              if (!l - d) >= (t / 20)
              then begin
                let p = 100. -. ((float_of_int d /. float_of_int t) *. 100.) in
                Printf.fprintf Pervasives.stderr "Progress: %f%%\n%!" p;
                l := d
              end
              else
                ()
          in
          MyLog.compact ~min_blocks:!mb ~progress_cb:(Some cb) l0 >>= fun () ->
          MyLog.close l0
        end
      in
      run t
    | Bench ->
      let t =
        let measure n f =
          let t0 = Unix.gettimeofday () in
          let a0 = Gc.allocated_bytes () in
          let step = let q = n / 10 in if q = 0 then 1 else q in
          let cb =
            function
            | 0 -> ()
            | i when i mod step = 0 ->
              let ti = Unix.gettimeofday () in
              let d = ti -. t0 in
              Printf.printf "\t%8i (%4.2f)\n%!" i d
            | _ -> ()
          in
          f () n cb >>= fun () ->
          let t1 = Unix.gettimeofday () in
          let a1 = Gc.allocated_bytes() in
          let dt = t1 -. t0 in
          let da = a1 -. a0 in
          return (dt,da)
        in
        begin
          Printf.printf "Using:%s\n%!" !store_name ;
          MyLog.init !fn ~d:!d Time.zero >>= fun () ->
          MyLog.make !fn >>= fun db ->
          let () = Printf.printf "\niterations = %i\nvalue_size = %i\n%!" !n !vs in
          let () = Printf.printf "starting sets\n" in
          measure !n (fun () -> set_loop db !vs) >>= fun (dt,da) ->
          Printf.printf "sets: %fs (burned = %e)\n%!" dt da;
          let () = Printf.printf "starting gets\n" in
          measure !n (fun () -> get_loop db) >>= fun (dt2,da2) ->
          Printf.printf "gets: %fs (burned = %e)\n%!" dt2 da2;
          let () = Printf.printf "starting deletes\n" in
          measure !n (fun () -> delete_loop db) >>= fun (dt3,da3) ->
          Printf.printf "deletes: %fs (burned = %e)\n%!" dt3 da3;
          let () = Printf.printf "starting set_tx (tx_size=%i)\n" !m in
          measure !n (fun () -> set_tx_loop db !vs !m) >>= fun (dt4,da4) ->
          Printf.printf "sets_tx: %fs (burned = %e)\n%!" dt4 da4;
          MyLog.close db
        end
      in
      run t
  in ();;
