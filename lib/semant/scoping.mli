
module Ident = Language.Ident

type scope

val empty : scope
val initial : scope

val get_reservedid : string -> Ident.t
val scoping_exp : scope -> Language.Syntax.exp -> Language.Syntax.exp
val scoping_bnds : scope -> Language.Syntax.bnd list -> Language.Syntax.bnd list 