type node = INT of int | CONS of node * int | AP of node * node | FUN of int * Code.t | HOLE

and stack = node array

let stack : stack = Array.make 10000 HOLE

let code : Code.t ref = ref []

let dump : (stack * Code.t) list ref = ref []

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

let pop_int () : int = match pop () with INT n -> n | _ -> failwith "INT required"

let pop_fun () : int * Code.t =
  match pop () with FUN (k, c) -> (k, c) | _ -> failwith "FUN required"
