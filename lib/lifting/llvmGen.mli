val the_module : Llvm.llmodule ref

val codegen_expr : Syntax.exp -> Llvm.llvalue

val codegen_proto : string * Syntax.id list -> unit

val codegen_func : Syntax.frag -> unit

val codegen : string -> Syntax.frags -> unit
