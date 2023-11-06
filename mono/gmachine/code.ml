type instr =
  | Push of int
  | PushInt of int
  | PushGlb of string
  | Pop of int
  | MkAp
  | Slide of int
  | Update of int
  | Alloc of int
  | Add
  | Sub
  | Mul
  | Div

type t = instr list
