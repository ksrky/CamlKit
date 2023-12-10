type scope

val empty : scope

val scoping_exp : scope -> Abstract.Syntax.exp -> Abstract.Syntax.exp

val scoping_bnds : scope -> Abstract.Syntax.bnd list -> Abstract.Syntax.bnd list
