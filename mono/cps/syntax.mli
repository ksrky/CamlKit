type id = Id.t

type oper = Core.Syntax.oper

type const = Core.Syntax.const

type ty = IntTy | BoolTy | ContTy of ty list | TupleTy of ty list

type var = id * ty

type value =
  | Const of const
  | Var of var
  | Glb of var
  | Lam of {vars: var list; body: exp}
  | Tuple of value list

and exp =
  | Let of {dec: dec; body: exp}
  | Letrec of {fundefs: fundef list; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Halt of value

and fundef = {var: var; params: var list; body: exp}

and dec =
  | ValDec of {var: var; val_: value}
  | PrimDec of {var: var; left: value; oper: oper; right: value}
  | ProjDec of {var: var; val_: value; idx: int}

type prog = exp

val mk_vars : var list -> value list

val mk_lam : var -> exp -> value

val mk_lams : var list -> exp -> value

val mk_app : value -> value -> exp

val mk_apps : value -> value list -> exp

val mk_let : dec list -> exp -> exp

val mk_projs : value -> var list -> dec list

val pp_print_exp : Format.formatter -> exp -> unit

val pp_print_fundef : Format.formatter -> fundef -> unit

val print_prog : prog -> unit
