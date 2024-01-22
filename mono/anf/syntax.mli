module L = Lambda.Syntax

type id = Id.t

type oper = L.oper

type const = L.const

type ty =
  | IntTy
  | BoolTy
  | FunTy of ty list * ty
  | TupleTy of ty list
  | CodeTy of ty * ty list * ty

type var = id * ty

type value =
  | Const of const
  | Var of id
  | Glb of id
  | Lam of {vars: var list; body: expty}
  | Tuple of valty list

and valty = value * ty

and exp =
  | Let of {dec: dec; body: expty}
  | If of {cond: valty; then_: expty; else_: expty}
  | Ret of valty

and dec =
  | ValDec of {var: var; val_: valty}
  | CallDec of {var: var; fcn: valty; args: valty list}
  | PrimDec of {var: var; left: valty; oper: oper; right: valty}
  | ProjDec of {var: var; val_: valty; idx: int}

and expty = exp * ty

type prog = expty

type def = {var: var; params: var list; body: expty}

val mk_let : dec list -> expty -> expty

val pp_print_ty : Format.formatter -> ty -> unit

val pp_print_val0 : Format.formatter -> value -> unit

val pp_print_exp : Format.formatter -> exp -> unit

val print_prog : prog -> unit
