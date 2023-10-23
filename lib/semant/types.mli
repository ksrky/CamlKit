module Ident = Language.Ident

type ty = NIL | TyconTy of {con: Ident.t; args: ty list} | FunTy of ty * ty | MetaTy of tyvar
and tyvar = {uniq: int; mutable repres: ty option}

val ppr_ty : ty -> string

val tNIL : ty
val tINT : ty
val tBOOL : ty
val tUNIT : ty
val (-->) : ty list -> ty -> ty

val new_tyvar : unit -> ty
val get_tyvars : ty -> tyvar list
val zonk_type : ty -> ty