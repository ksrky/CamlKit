module Syntax = Syntax

val codegen_exp : Llvm.llmodule -> Syntax.exp -> Llvm.llvalue

val codegen_proto : Llvm.llmodule -> string -> Syntax.id list -> unit

val codegen_func : Llvm.llmodule -> Syntax.code -> unit

val codegen : string -> Syntax.codes -> Llvm.llmodule

val format : string -> Llvm.llmodule -> unit
