type id = Id.t

type oper = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Nil

type exp =
  | Const of const
  | Var of id
  | App of {fcn: exp; args: exp list}
  | Lam of {vars: id list; body: exp}
  | Prim of {oper: oper; args: exp list}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {isrec: bool; vars: id list; bnds: exp list; body: exp}
  | Tuple of exp list
  | Split of {inp: exp; vars: id list; body: exp}

val lams : id list -> exp -> exp

val unlam : exp -> id list * exp

val ppr_const : const -> string

val ppr_oper : oper -> string

val ppr_exp : (id -> string) -> exp -> string
