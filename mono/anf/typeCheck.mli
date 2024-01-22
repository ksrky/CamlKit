type tyctx = Syntax.ty Id.table

val empty : 'a Id.table

val check_exp : tyctx -> Syntax.expty -> unit

val check_prog : tyctx -> Syntax.expty -> unit

val check_prog_cc : Syntax.ty Id.table -> ClosConv.prog -> unit
