type tyctx

val empty : tyctx

val check_exp : tyctx -> Syntax.expty -> unit

val check_prog : tyctx -> Syntax.prog -> unit
