type id = Id.t

type ty =
  | I1Ty
  | I32Ty
  | PtrTy of ty
  | FunTy of ty * ty list
  | StrctTy of ty list

type const = I1 of int | I32 of int

type var = id * ty

type value = Const of const | Var of id | Glb of id

and exp =
  | Let of {dec: dec; body: exp}
  | App of {fcn: value; args: value list}
  | If of {oper: relop; left: value; right: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: id; val_: value}
  | PrimDec of {name: id; left: value; oper: arithop; right: value}
  | SubscrDec of {name: id; val_: value; idx: int}
  | MallocDec of {name: id; len: int}
  | UpdateDec of {name: id; var: id; idx: int; val_: value}

and arithop = Add | Sub | Mul | Div

and relop = Eq | Ne | Lt | Le | Gt | Ge

type heap =
  | Code of {name: id; params: (id * ty) list; ret_ty: ty; body: exp}
  | Tuple of {name: id; vals: value list}

type prog = heap list * exp

val mk_let : dec list -> exp -> exp

val print_prog : prog -> unit
