type prog = Syntax.def list * Syntax.expty

val cc_prog : Syntax.expty -> prog

val print_prog : Syntax.def list * Syntax.expty -> unit
