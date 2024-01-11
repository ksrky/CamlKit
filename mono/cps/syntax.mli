type id = Id.t

type oper = Lambda.Syntax.oper

type const = Lambda.Syntax.const

type ty =
  | IntTy
  | BoolTy
  | ContTy of ty list
  | TupleTy of ty list
  | ExistsTy of id * ty

type var = id * ty

type value =
  | Const of const
  | Var of id
  | Glb of id
  | Lam of {vars: var list; body: exp}
  | Tuple of valty list
  | Pack of {ty: ty; val_: valty; exty: ty}

and valty = value * ty

and exp =
  | Let of {dec: dec; body: exp}
  | App of {fcn: valty; args: valty list}
  | If of {cond: valty; then_: exp; else_: exp}
  | Halt of valty

and def = {var: var; params: var list; body: exp}

and dec =
  | ValDec of {var: var; val_: valty}
  | PrimDec of {var: var; left: valty; oper: oper; right: valty}
  | ProjDec of {var: var; val_: valty; idx: int}
  | UnpackDec of {tyvar: var; var: var; val_: valty}

type prog = exp

val mk_lam : var -> exp -> value

val mk_lams : var list -> exp -> value

val mk_app : valty -> valty -> exp

val mk_apps : valty -> valty list -> exp

val mk_let : dec list -> exp -> exp

val mk_projs : valty -> var list -> dec list

val pp_print_ty : Format.formatter -> ty -> unit

val pp_print_val0 : Format.formatter -> value -> unit

val pp_print_exp : Format.formatter -> exp -> unit

val pp_print_def : Format.formatter -> def -> unit

val print_prog : prog -> unit
