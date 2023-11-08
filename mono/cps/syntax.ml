type id = Id.t

type value =
  | Var of id
  | Int of int
  | Lam of {vars: id list; body: exp}
  | Tuple of value list

and exp =
  | Let of {decs: dec list; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | VarDec of {name: id; value: value}
  | ProjDec of {name: id; tuple: value; index: int}
  | PrimDec of {name: id; oper: string; args: value list}
