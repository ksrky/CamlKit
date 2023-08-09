type label = string

type arithop = Add | Sub | Mul | Div

type relop = Eq | Neq | Lt | Le | Gt | Ge

type segment = Const | Arg | Temp

type instr =
  | Push of segment * int
  | Label of label
  | Goto of label
  | IfGoto of relop * label
  | Call of label * int
  | Arith of arithop
  | Return

type frag = Proc of label * int * instr list
