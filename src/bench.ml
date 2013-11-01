(* benchmark routines - making them accessible from xen/mirage *)

module Bench = functor (LF: functor(S: Bs_internal.STORE) -> Log.LOG with type 'a m = 'a S.m) ->
    functor (S: Bs_internal.STORE) ->
struct
  module MyLog = LF(S)
  module MyDB = Tree.DB(MyLog) 
  module MyDBX = Dbx.DBX(MyLog)
  module MySlab = Slab

  let (>>=) = MyLog.bind
  let return = MyLog.return

  let init ?(d=8) name = MyLog.init ~d:d name Time.zero 

  type progress_callback = int -> unit
  let make_key i = Printf.sprintf "key_%08i" i

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
    let empty = MySlab.make now in
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

end

