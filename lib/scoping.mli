type scope

val empty : scope
val initial : scope

val get_reservedid : string -> Ident.t
val scoping_exp : scope -> AbsSyn.exp -> AbsSyn.exp
val scoping_bnds : scope -> AbsSyn.bnd list -> AbsSyn.bnd list
val scoping_defs : scope -> AbsSyn.def list -> AbsSyn.def list