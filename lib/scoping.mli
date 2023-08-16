type scope

val empty : scope
val initial : scope

val scoping_exp : scope -> AbsSyn.exp -> AbsSyn.exp
val scoping_decs : scope -> AbsSyn.dec list -> AbsSyn.dec list