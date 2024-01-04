module A = Abstract.Syntax

val has_error : bool ref

val scope_error : Id.t -> unit

val binop_error : A.op -> A.ty -> A.ty -> unit

val letrec_error : Id.t -> unit

val unification_error : A.ty -> A.ty -> unit

val infinite_type_error : A.tyvar -> A.ty -> unit
