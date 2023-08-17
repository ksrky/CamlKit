type scope

val empty : scope
val initial : scope

val scoping_exp : scope -> AbsSyn.exp -> AbsSyn.exp
val scoping_bnds : scope -> AbsSyn.bnd list -> AbsSyn.bnd list