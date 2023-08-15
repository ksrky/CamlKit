val the_module : Llvm.llmodule

val codegen_expr : IntSyn.exp -> Llvm.llvalue
val codegen_proto : string * Ident.t list -> unit
val codegen_func : IntSyn.def -> unit
val codegen : IntSyn.defs -> unit