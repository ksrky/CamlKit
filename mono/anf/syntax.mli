module L = Lambda.Syntax

type id = Id.t

type oper = L.oper

type const = L.const

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
  | Lam of {vars: var list; body: expty}
  | Call of {fcn: valty; args: valty list}
  | Prim of {left: valty; oper: oper; right: valty}
  | Tuple of valty list
  | Proj of {val_: valty; idx: int}
  | Pack of {ty: ty; val_: valty; exty: ty}

and valty = value * ty

and exp =
  | Let of {var: var; params: var list; bind: expty; body: expty}
  | Unpack of {tyvar: var; var: var; bind: expty; body: expty}
  | If of {cond: valty; then_: expty; else_: expty}
  | Ret of valty

and expty = exp * ty

type prog = exp
