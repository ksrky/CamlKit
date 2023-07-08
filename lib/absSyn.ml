type id = string

type exp =
  | VarExp of id
  | NilExp
  | IntExp of int
  | AppExp of {func: exp; arg: exp}
  | LamExp of {var: id; body: exp}
  | OpExp of {left: exp; oper: oper; right: exp}
  | IfExp of {test: exp; then': exp; else': exp}
  | LetExp of {decs: dec list; body: exp}

and dec = {name: id; params: id list; body: exp}

and oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp