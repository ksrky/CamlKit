type prim = Add | Sub | Mul | Div

type instr =
  | PushInt of int
  | Access of int
  (* Application *)
  | Appterm
  | Apply
  | Pushmark
  (* Abstractions *)
  | Grab
  | Cur of code
  | Return
  (* Local declaratitons *)
  | Let
  | Endlet
  | Dummy
  | Update
  | Prim of prim

and code = instr list

val run_instr : unit -> unit
