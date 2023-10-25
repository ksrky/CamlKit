open Language.Syntax

val ( --> ) : ty list -> ty -> ty

val new_tyvar : unit -> ty

val get_tyvars : ty -> tyvar list

val zonk_type : ty -> ty
