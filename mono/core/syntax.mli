type id = Id.t

type oper = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Bool of bool

type exp =
  | Const of const
  | Var of id
  | App of {fcn: exp; arg: exp}
  | Lam of {var: id; body: exp}
  | Prim of {left: exp; oper: oper; right: exp}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {isrec: bool; vars: id list; bnds: exp list; body: exp}

val lams : id list -> exp -> exp

val unlam : exp -> id list * exp

val ppr_const : const -> string

val ppr_oper : oper -> string

val ppr_exp : (id -> string) -> exp -> string
