type label = string

type ty = TyInt | TyBool | TyFun of ty * ty

type term = Var of Id.t | Abs of {var: Id.t; var_ty: ty; body: term}

type bp = Bind of {var: Id.t; var_ty: ty; body: term}

type bind = Nonrec of bp | Rec of bp list

type kont = Kont of {left: term; right: kont} | Ret

type command =
  | Let of {bnd: bind; cmd: command}
  | Cut of {left: term; right: kont}
  | Jump of {lab: label; args: term list}
