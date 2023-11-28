type exp =
  | Const of Syntax.const
  | Var of Syntax.id
  | App of {fcn: exp; arg: exp}
  | Prim of {oper: Syntax.oper; args: exp list}
  | Let of {isrec: bool; vars: Syntax.id list; bnds: exp list; body: exp}
  | If of {cond: exp; then_: exp; else_: exp}
  | Clos of clos
  | Select of {clos: clos; idx: int}

and clos =
  | CVar of Syntax.id
  | CAbs of {cvar: Syntax.id; var: Syntax.id; body: exp; env: escapes}

and escapes = Syntax.id list

val ppr_exp : exp -> string

val cc_prog : Syntax.exp -> exp
