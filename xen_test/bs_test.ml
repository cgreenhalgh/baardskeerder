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
open Lwt 

module MyLog = Logs.Flog0(Baardskeerder_mirage.Stores.Blkif) 
module MyDB = DB(MyLog)
module MyDBX = DBX(MyLog)

let vs = ref 2000
let n  = ref 10_000 
let m  = ref 100 


(* benchmarks tasks from bsmgr.ml *)
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
          MyLog.init name ~d:!d Time.zero >>= fun () ->
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
          OS.Console.log(Printf.printf "sets_tx: %fs (burned = %e)\n%!" dt4 da4);
          MyLog.close db >>= fun () ->
          return ()

