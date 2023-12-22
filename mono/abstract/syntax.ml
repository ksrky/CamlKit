type id = Id.t

(** Literal *)
type lit = IntLit of int | BoolLit of bool

(** Binary operator *)
type op =
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

(** Expression *)
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

(** Binding *)
and bnd = Bind of {name: id; params: id list; body: exp}

(** Program *)
type prog = exp

(** Note: Type checker translates [prog] (without type annotations) into [aprog]
    (explicitly typed representation) *)

(** Type *)
type ty = NilTy | BoolTy | IntTy | FunTy of ty * ty | MetaTy of tyvar

(** Meta type variable that will be replaced by an actual type after type
    checking *)
and tyvar = {uniq: int; mutable repres: ty option}

(** Variable *)
type var = id * ty

(** Annotated expression *)
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

(** Pair of annotaed expression and type *)
and expty = aexp * ty

(** Annotaed binding *)
and abnd = ABind of {name: id; params: var list; body: expty}

(** Annotated program *)
type aprog = expty
