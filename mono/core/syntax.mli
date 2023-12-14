type id = Id.t

type oper = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Bool of bool

type ty = IntTy | BoolTy | FunTy of ty * ty

type var = id * ty

type exp =
  | Const of const
  | Var of id
  | App of {fcn: expty; arg: expty}
  | Lam of {var: var; body: expty}
  | Prim of {left: expty; oper: oper; right: expty}
  | If of {cond: expty; then_: expty; else_: expty}
  | Let of {isrec: bool; vars: var list; bnds: expty list; body: expty}

and expty = exp * ty

type prog = exp

val fun_ty : ty -> ty -> ty

val fun_tys : ty list -> ty -> ty

val lams : var list -> expty -> expty

val unlam : expty -> var option * expty

val print_prog : prog -> unit
