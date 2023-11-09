module L = Language.Syntax

val unify : L.ty -> L.ty -> unit

val unify_funs : 'a list -> L.ty -> L.ty list * L.ty
