type prim = Add | Sub | Mul | Div

type instr =
  | PushInt of int
  | Access of int
  (* Application *)
  | Appterm
  | Apply
  | Pushmark
  (* Abstractions *)
  | Cur of t
  | Grab
  | Return
  (* Local declaratitons *)
  | Let
  | Endlet
  | Dummy
  | Update
  | Prim of prim

and t = instr list
