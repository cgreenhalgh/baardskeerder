let size_from s pos =
  let byte_of i= Char.code s.[pos + i] in
  let b0 = byte_of 0
  and b1 = byte_of 1
  and b2 = byte_of 2
  and b3 = byte_of 3 in
  let result = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)
  in result

module Pack = struct

  type output = Buffer.t

  let make_output h = Buffer.create h

  let size_to b (i:int) = 
    let char_at pos =
      let mask = 0xff lsl pos in
      let code = (i land mask) lsr pos in
      Char.unsafe_chr code
    in
    let add pos = Buffer.add_char b (char_at pos) in
    add 0;
    add 8;
    add 16;
    add 24


  let bool_to b (v:bool) = 
    let c = if v then '1' else '0' in
    Buffer.add_char b c

  let vint_to b n = 
    let add c = Buffer.add_char b c in
    let rec loop = function
      | 0 -> add '\x00'
      | n when n < 128 -> add (Char.unsafe_chr n)
      | n -> let byte = (n land 0x7f) lor 0x80  in
	     let () = add (Char.unsafe_chr byte) in
	     let r = n lsr 7 in
	     loop r
    in loop n

  let vint64_to b (n:int64) = 
    let add c = Buffer.add_char b c in
    let rec loop = function
      | 0L -> add '\x00'
      | n when n < 128L -> add (Char.chr (Int64.to_int n))
      | n -> 
        let last = (Int64.to_int n) land 0x7f in
        let byte = last lor 0x80  in
        let () = add (Char.chr byte) in
        let r = Int64.shift_right n 7 in
        loop r
    in loop n
    
  let string_to b s = 
    let l = String.length s in
    vint_to b l;
    Buffer.add_string b s

  let option_to b a_to = function
    | None   -> bool_to b false
    | Some a -> let () = bool_to b true in
                a_to b a


  let string_option_to b so = option_to b string_to so
      
  let list_to b e_to list = 
    let l = Leaf.length list in
    vint_to b l;
    List.iter (e_to b) list

  let close_output b = 
    let s = Buffer.contents b in
    let size = String.length s in
    let b2 = Buffer.create 4 in
    let () = size_to b2 size in
    let b2s = Buffer.contents b2 in
    b2s ^ s
    
  type input = {s:string; mutable p:int}  

  let input2s input = Printf.sprintf "{%S;%i}" input.s input.p

  let make_input s p = {s;p}

  let input_char input = 
    let c = input.s.[input.p] in
    let () = input.p <- input.p + 1 in
    c

  let input_bool input = 
    let c = input_char input in
    match c with
      | '0' -> false
      | '1' -> true
      | _ -> let msg = Printf.sprintf "'%C' is not a bool" c in failwith msg


  let input_size input = 
    let p = input.p in
    let () = input.p <- p + 4 in
    size_from input.s p
      
  let input_vint input = 
    let s = input.s in
    let start = input.p in
    let rec loop v shift p = 
      let c = s.[p] in
      let cv = Char.code c in
      if cv < 0x80 
      then 
        let () = input.p <- p+ 1  in
        v + (cv lsl shift)
      else 
        let v' = v + ((cv land 0x7f) lsl shift) in
        loop v' (shift + 7) (p+1)
    in loop 0 0 start
    
  let input_vint64 input = 
    let (+:) = Int64.add in
    let ( <<: ) = Int64.shift_left in
    let s = input.s in
    let start = input.p in
    let rec loop v shift p = 
      let c = s.[p] in
      let cv = Int64.of_int (Char.code c) in
      if cv < 0x80L 
      then 
        let () = input.p <- p+ 1  in
        v +: (cv <<: shift)
      else 
        let v' = v +: ((Int64.logand cv  0x7fL) <<: shift) in
        loop v' (shift + 7) (p+1)
    in loop 0L 0 start

  let input_string input = 
    let l = input_vint input in
    let s = String.sub input.s input.p l in
    let () = input.p <- input.p + l in
    s

  let input_raw input l = 
    let s = String.sub input.s input.p l in
    let () = input.p <- input.p + l in
    s

  let input_option input_a input =
    let some = input_bool input in
    if some
    then let a = input_a input in Some a
    else None

  let input_string_option input = input_option input_string input

  let input_list input_e input = 
    let l = input_vint input in
    let rec loop acc = function
      | 0 -> List.rev acc
      | n -> let e = input_e input in
	     loop (e :: acc) (n-1)
    in
    loop [] l      
end
