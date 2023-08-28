val the_module : Llvm.llmodule ref

val codegen_expr : IntSyn.exp -> Llvm.llvalue
val codegen_proto : string * IntSyn.binders -> unit
val codegen_func : IntSyn.frag -> unit
val codegen : string -> IntSyn.frags -> unit