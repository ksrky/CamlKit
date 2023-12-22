type id = Id.t

type ty =
  | I1Ty
  | I32Ty
  | PtrTy of ty option
  | FunTy of ty * ty list
  | StrctTy of ty list

type const = I1 of int | I32 of int

type var = id * ty

type value = Const of const | Var of var | Glb of var

and exp =
  | Let of {dec: dec; body: exp}
  | If of {oper: relop; left: value; right: value; then_: exp; else_: exp}
  | Return of value

and dec =
  | ValDec of {var: var; val_: value}
  | PrimDec of {var: var; left: value; oper: arithop; right: value}
  | CallDec of {var: var; fcn: value; args: value list}
  | SubscrDec of {var: var; val_: value; idx: int}
  | MallocDec of {var: var; len: int}
  | UpdateDec of {var: var; strct: value; idx: int; val_: value}

and arithop = Add | Sub | Mul | Div

and relop = Eq | Ne | Lt | Le | Gt | Ge

type heap =
  | Code of {var: var; params: var list; body: exp}
  | Tuple of {name: id; vals: value list}

type prog = heap list * exp

val mk_let : dec list -> exp -> exp

val return_type : ty

val deref_type : ty -> ty

val print_prog : prog -> unit
