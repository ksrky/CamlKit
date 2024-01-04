type id = Id.t

type oper = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Bool of bool

type ty = IntTy | BoolTy | FunTy of ty * ty | TupleTy of ty list

type var = id * ty

type exp =
  | Const of const
  | Var of var
  | App of {fcn: expty; arg: expty}
  | Lam of {var: var; body: expty}
  | Prim of {left: expty; oper: oper; right: expty}
  | If of {cond: expty; then_: expty; else_: expty}
  | Let of {var: var; bnd: exp; body: expty}
  | Fix of {defs: def list; body: expty}
  | Tuple of expty list
  | Proj of {tup: expty; idx: int}

and expty = exp * ty

and def = {var: var; params: var list; body: expty}

type prog = expty

val fun_ty : ty -> ty -> ty

val fun_tys : ty list -> ty -> ty

val lams : var list -> expty -> expty

val unlam : expty -> var option * expty

val mk_let : (var * exp) list -> expty -> expty

val pp_print_ty0 : Format.formatter -> ty -> unit

val pp_print_exp0 : Format.formatter -> exp -> unit

val print_prog : prog -> unit
