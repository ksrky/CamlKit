module A = Abstract.Syntax

val unify : A.ty -> A.ty -> unit

val unify_funs : 'a list -> A.ty -> A.ty list * A.ty
