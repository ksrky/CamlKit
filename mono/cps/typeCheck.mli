type tyctx

val empty : tyctx

val check_exp : tyctx -> Syntax.exp -> unit

val check_prog : tyctx -> Syntax.prog -> unit
