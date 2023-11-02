(*module C = Combinator.Syntax

  val the_module : Llvm.llmodule ref

  val codegen_expr : C.exp -> Llvm.llvalue

  val codegen_proto : string * C.id list -> unit

  val codegen_func : C.frag -> unit

  val codegen : string -> C.frags -> unit
*)
