val tNIL : AbsSyn.ty
val tINT : AbsSyn.ty
val tBOOL : AbsSyn.ty
val tUNIT : AbsSyn.ty
val (-->) : AbsSyn.ty list -> AbsSyn.ty -> AbsSyn.ty

val new_tyvar : unit -> AbsSyn.ty
val get_tyvars : AbsSyn.ty -> AbsSyn.tyvar list
val zonk_type : AbsSyn.ty -> AbsSyn.ty