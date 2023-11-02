open Language.Syntax

val ( --> ) : ty list -> ty -> ty

val new_tyvar : unit -> ty

val get_tyvars : ty -> tyvar list

val zonk_ty : ty -> ty

val zonk_aexp : aexp -> aexp

val zonk_abnd : abnd -> abnd
