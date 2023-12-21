type id = Id.t

type lit = IntLit of int | BoolLit of bool

type exp =
  | VarExp of id
  | NilExp
  | BoolExp of bool
  | IntExp of int
  | AppExp of {fcn: exp; arg: exp}
  | LamExp of {vars: id list; body: exp}
  | OpExp of {left: exp; op: op; right: exp}
  | IfExp of {cond: exp; then_: exp; else_: exp}
  | LetExp of {bnds: bnd list; body: exp}
  | LetrecExp of {bnds: bnd list; body: exp}

and bnd = Bind of {name: id; params: id list; body: exp}

and op =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp

type ty = NilTy | BoolTy | IntTy | FunTy of ty * ty | MetaTy of tyvar

and tyvar = {uniq: int; mutable repres: ty option}

type var = id * ty

type aexp =
  | VarAExp of var
  | NilAExp
  | BoolAExp of bool
  | IntAExp of int
  | AppAExp of {fcn: expty; arg: expty}
  | LamAExp of {params: var list; body: expty}
  | OpAExp of {left: expty; op: op; right: expty}
  | IfAExp of {cond: expty; then_: expty; else_: expty}
  | LetAExp of {bnds: abnd list; body: expty}
  | LetrecAExp of {bnds: abnd list; body: expty}

and expty = aexp * ty

and abnd = ABind of {name: id; params: var list; body: expty}

type aprog = expty

val pp_print_ty0 : Format.formatter -> ty -> unit
