type id = Id.t

type oper = Core.Syntax.oper

type const = Core.Syntax.const

type value =
  | Const of const
  | Var of id
  | Glb of id
  | Lam of {vars: id list; body: exp}
  | Tuple of value list

and fundef = {name: id; vars: id list; body: exp}

and exp =
  | Let of {dec: dec; body: exp}
  | Letrec of {fundefs: fundef list; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: id; val_: value}
  | PrimDec of {name: id; left: value; oper: oper; right: value}
  | ProjDec of {name: id; val_: value; idx: int}

type prog = exp

val mk_vars : id list -> value list

val mk_lam : id -> exp -> value

val mk_lams : id list -> exp -> value

val mk_app : value -> value -> exp

val mk_apps : value -> value list -> exp

val mk_let : dec list -> exp -> exp

val mk_projs : value -> id list -> dec list

val pp_print_exp : Format.formatter -> exp -> unit

val pp_print_fundef : Format.formatter -> fundef -> unit

val print_prog : prog -> unit
