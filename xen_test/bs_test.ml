let get_first_blkif () =
  (* get unknown blkif id - current vid e.g. '2049' *)
  let namet,nameu = Lwt.task () in
  Lwt.async (fun () -> (OS.Devices.listen (fun id ->
    OS.Console.log ("found " ^ id); Lwt.wakeup nameu id; Lwt.return ())));
  lwt name = namet in
  OS.Console.log("try to open "^name);
  lwt oblkif = OS.Devices.find_blkif name in 
  match oblkif with 
  | None -> Lwt.fail(Failure ("getting blkif "^name))
  | Some blkif -> Lwt.return blkif

let init_xen () = 
  (* Blkfront will map xen block devs to Mirage blkif *)
  lwt () = Blkfront.register () in
  Lwt.return ()

(* mirage Baardseerker type *)
open Baardskeerder
module MyBench = Bench(Logs.Flog0)(Baardskeerder_mirage.Stores.Blkif)
module MyLog = MyBench.MyLog
open MyBench
open Lwt 

let vs = ref 2000
let n  = ref 10_000 
let m  = ref 100 
let d = ref 4

let main () =
  OS.Console.log "hello" ;
  lwt () = init_xen () in
  (* get unknown blkif id - current vid e.g. '2049' *)
  lwt blkif = get_first_blkif () in 
  let name = blkif#id in
  (* benchmark from bsmgr *)
        let measure n f =
          let t0 = OS.Clock.time () in
          let a0 = Gc.allocated_bytes () in
          let step = let q = n / 10 in if q = 0 then 1 else q in
          let cb =
            function
            | 0 -> ()
            | i when i mod step = 0 ->
              let ti = OS.Clock.time () in
              let d = ti -. t0 in
              Printf.printf "\t%8i (%4.2f)\n%!" i d
            | _ -> ()
          in
          f () n cb >>= fun () ->
          let t1 = OS.Clock.time () in
          let a1 = Gc.allocated_bytes() in
          let dt = t1 -. t0 in
          let da = a1 -. a0 in
          return (dt,da)
        in
          OS.Console.log (Printf.sprintf "Using:%s\n%!" "Blkif") ;
          init ~d:!d name >>= fun () ->
          MyLog.make name >>= fun db ->
          let () = OS.Console.log(Printf.sprintf "\niterations = %i\nvalue_size = %i\n%!" !n !vs) in
          let () = OS.Console.log(Printf.sprintf "starting sets\n") in
          measure !n (fun () -> set_loop db !vs) >>= fun (dt,da) ->
          OS.Console.log(Printf.sprintf "sets: %fs (burned = %e)\n%!" dt da);
          let () = OS.Console.log(Printf.sprintf "starting gets\n") in
          measure !n (fun () -> get_loop db) >>= fun (dt2,da2) ->
          OS.Console.log(Printf.sprintf "gets: %fs (burned = %e)\n%!" dt2 da2);
          let () = OS.Console.log(Printf.sprintf "starting deletes\n") in
          measure !n (fun () -> delete_loop db) >>= fun (dt3,da3) ->
          OS.Console.log(Printf.sprintf "deletes: %fs (burned = %e)\n%!" dt3 da3);
          let () = OS.Console.log(Printf.sprintf "starting set_tx (tx_size=%i)\n" !m) in
          measure !n (fun () -> set_tx_loop db !vs !m) >>= fun (dt4,da4) ->
          OS.Console.log(Printf.sprintf "sets_tx: %fs (burned = %e)\n%!" dt4 da4);
          MyLog.close db >>= fun () ->
          return ()

