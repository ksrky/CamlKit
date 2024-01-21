module L = Lambda.Syntax

type id = Id.t

type oper = L.oper

type const = L.const

type ty =
  | IntTy
  | BoolTy
  | FunTy of ty list * ty
  | TupleTy of ty list
  | ExistsTy of id * ty

type var = id * ty

type value =
  | Const of const
  | Var of id
  | Glb of id
  | Lam of {vars: var list; body: expty}
  | Tuple of valty list
  | Pack of {ty: ty; val_: valty; exty: ty}

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
  | UnpackDec of {tyvar: var; var: var; val_: valty}

and expty = exp * ty

type prog = exp

type def = {var: var; params: var list; body: expty}

val mk_let : dec list -> expty -> expty

val pp_print_exp : Format.formatter -> exp -> unit

val print_prog : exp -> unit
