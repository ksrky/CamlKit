type tyctx

val empty : tyctx

val check_exp : tyctx -> Syntax.exp -> unit

val check_prog : tyctx -> Syntax.prog -> unit

val check_prog_cc : tyctx -> ClosConv.prog -> unit
