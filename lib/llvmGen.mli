val the_module : Llvm.llmodule

val codegen_expr : IntSyn.exp -> Llvm.llvalue
val codegen_proto : string * Ident.t list -> Llvm.llvalue
val codegen_func : IntSyn.def -> Llvm.llvalue
val codegen_builtins  : unit -> unit