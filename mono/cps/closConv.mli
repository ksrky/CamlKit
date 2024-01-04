type prog = Syntax.def list * Syntax.exp

val cc_prog : Syntax.exp -> prog

val print_prog : prog -> unit
