type id = Id.t

type prim = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Bool of bool

type exp =
  | Const of const
  | Var of id
  | App of {fcn: exp; args: exp list}
  | Prim of {prim: prim; args: exp list}
  | Lam of {vars: id list; body: exp}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {isrec: bool; vars: id list; bnds: exp list; body: exp}

type prog = exp
