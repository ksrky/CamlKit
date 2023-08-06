type id = Ident.t

type ids = id list

type exps = exp list

and exp =
  | Int of int
  | Nil
  | Var of id
  | App of exp * exps
  | Lam of ids * exp
  | Builtin of string * exps
  | Let of ids * exps * exp
  | Letrec of ids * exps * exp
  | If of exp * exp * exp

type def = {name: id; params: ids; body: exp}

type defs = def list
