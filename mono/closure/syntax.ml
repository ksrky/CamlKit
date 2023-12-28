module L = Lambda.Syntax

type id = Id.t

type oper = L.oper

type const = L.const

type ty = IntTy | BoolTy | FunTy of ty * ty | ClosTy of ty * ty

type var = id * ty

type exp =
  | Const of const
  | Var of id
  | Glb of id
  | ClosApp of {fcn: expty; env: expty; arg: expty}
  | TailApp of {fcn: expty; arg: expty}
  | Lam of {var: var; body: expty}
  | Prim of {left: expty; oper: oper; right: expty}
  | If of {cond: expty; then_: expty; else_: expty}
  | Let of {isrec: bool; vars: var list; bnds: expty list; body: expty}
  | Tuple of expty list
  | Proj of {tup: expty; idx: int}

and expty = exp * ty

type prog = expty
