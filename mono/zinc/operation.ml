type instr =
  | Ldi of int
  | Access of int
  | Closure of code
  | Let
  | Endlet
  | Add
  | Sub
  | Mul
  | Div
  | Apply
  | Return

and code = instr list
