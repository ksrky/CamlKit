type node = INT of int | CONS of node * int | HOLE

and stack = node array

let stack : stack = Array.make 10000 HOLE

let sp : int ref = ref 0

let push (k : int) : unit =
  stack.(!sp) <- stack.(!sp - k);
  incr sp

let push_node (n : node) : unit =
  stack.(!sp) <- n;
  incr sp

let push_int (n : int) : unit =
  stack.(!sp) <- INT n;
  incr sp

let push_hole () : unit =
  stack.(!sp) <- HOLE;
  incr sp

let rec decr_sp (k : int) : unit =
  if k = 0 then ()
  else (
    decr sp;
    decr_sp (k - 1) )

let pop () : node =
  let k = !sp in
  decr sp; stack.(k)
