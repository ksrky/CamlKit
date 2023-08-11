let compile (src : string) : unit =
  let abs = Parse.parse src in
  (* print_endline (AbsSyn.pretty_exp abs); *)
  let intsyn = Semant.trans_exp Env.empty abs in
  let intsyn' = ClosConv.f intsyn in
  let defs = Lifting.f intsyn' in
  (*print_endline (IntSyn.ppr_defs defs*)
  Llvm.dump_value (LlvmGen.codegen_proto ("WRITEC", [Ident.from_string "x"]));
  Llvm.dump_value (LlvmGen.codegen_proto ("READC", [Ident.from_string "x"]));
  List.iter (fun def -> Llvm.dump_value (LlvmGen.codegen_func def)) defs
(*Llvm.dump_module LlvmGen.the_module*)
