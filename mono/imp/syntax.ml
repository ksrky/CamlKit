type id = Id.t

type value = Const of int | Var of id

and exp =
  | Let of {dec: dec; body: exp}
  | App of {fcn: value; args: value list}
  | If of {oper: relop; left: value; right: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: id; val_: value}
  | PrimDec of {name: id; oper: arithop; args: value list}
  | ProjDec of {name: id; val_: value; idx: int}
  | MallocDec of {name: id; len: int}
  | UpdateDec of {name: id; var: id; idx: int; val_: value}

and arithop = Add | Sub | Mul | Div

and relop = Eq | Ne | Lt | Le | Gt | Ge

type heap =
  | Code of {name: id; vars: id list; body: exp}
  | Tuple of {name: id; vals: value list}

type prog = heap list * exp

let let_decs decs body =
  List.fold_right (fun dec body -> Let {dec; body}) decs body

(*

type name = string
type label 

type binop = PLUS | MINUS | TIMES | DIVIDE

type relop = EQ | NE | LT | GT | LE | GE

type ty = INT | VOID

type exp =
  | CONST of int
  | VAR of name
  | BINOP of binop * exp * exp
  | CALL of exp * exp list
  | MALLOC of ty list
  | UPDATE of exp * int * exp
  | PROJ of exp * int
  | ESEQ of stm * exp

and stm =
  | Let of dec * exp
  | CJUMP of relop * exp * exp * label * label
  | ASSIGN of name * exp
  | EXIT of exp 
  | LABEL of label

type frag = PROC of {name: name; body: stm}*)
