open Syntax

val codegen_exp : Llvm.llmodule -> exp -> Llvm.llvalue

val codegen_proto : Llvm.llmodule -> string -> id list -> unit

val codegen_func : Llvm.llmodule -> code -> unit

val codegen : string -> codes -> Llvm.llmodule
