type id = Id.t

type oper = Core.Syntax.oper

type const = Core.Syntax.const

type value =
  | Const of const
  | Var of id
  | Lam of {vars: id list; body: exp}
  | Tuple of value list

and exp =
  | Let of {decs: dec list; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Split of {inp: value; vars: id list; body: exp}
  | Halt of value

and dec =
  | VarDec of {name: id; value: value}
  | ProjDec of {name: id; tuple: value; index: int}
  | PrimDec of {name: id; oper: oper; args: value list}

let lam var body = Lam {vars= [var]; body}

let lams vars body = Lam {vars; body}

let app fcn arg = App {fcn; args= [arg]}

let apps fcn args = App {fcn; args}
