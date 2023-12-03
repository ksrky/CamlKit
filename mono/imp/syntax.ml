type id = Id.t

type binop = PLUS | MINUS | TIMES | DIVIDE

type relop = EQ | NE | LT | GT | LE | GE

type ty = INT | VOID

type exp =
  | CONST of int
  | VAR of id
  | BINOP of binop * exp * exp
  | CALL of exp * exp list
  | MALLOC of ty list
  | UPDATE of exp * int * exp
  | PROJ of exp * int
  | ESEQ of stm * exp

and stm =
  | EXP of exp
  | IF of exp * exp * exp
  | ASSIGN of id * exp
  | EXIT of exp
  | SEQ of stm * stm

type frag = PROC of {name: id; body: stm}
