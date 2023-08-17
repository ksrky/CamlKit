(** [run path] evaluates a source file on the virtual machine. *)
let run (path : string) =
  let abssyn = Parse.parse path in
  let abssyn' = Scoping.scoping_defs Scoping.initial abssyn in
  (* print_endline (AbsSyn.ppr_exp abssyn); *)
  let intsyn = Semant.trans_defs Env.empty abssyn' in
  (* print_endline (IntSyn.ppr_exp Ident.name intsyn); *)
  let instrs = Compile.f intsyn in
  (* print_endline (Machine.show_instrs instrs); *)
  Stack.init (); Machine.load_instrs instrs; Machine.run_commands ()

(** [eval inp] evaluates string [inp] on the virtual machine. *)
let eval (inp : string) =
  let abssyn = Parse.parse_line inp in
  let abssyn' = Scoping.scoping_exp Scoping.initial abssyn in
  (* print_endline (AbsSyn.ppr_exp abssyn); *)
  let intsyn = Semant.trans_exp Env.empty abssyn' in
  (* print_endline (IntSyn.ppr_exp Ident.name intsyn); *)
  let instrs = Compile.f intsyn in
  (* print_endline (Machine.show_instrs instrs); *)
  Stack.init (); Machine.load_instrs instrs; Machine.run_commands ()

(** [compile path] compiles a source file to LLVM IR and output to a .ll file. *)
let compile (path : string) : unit =
  let abssyn = Parse.parse path in
  let abssyn' = Scoping.scoping_defs Scoping.initial abssyn in
  (* print_endline (AbsSyn.ppr_exp abssyn'); *)
  let intsyn = Semant.trans_defs Env.empty abssyn' in
  (* print_endline (IntSyn.ppr_exp Ident.name intsyn); *)
  let intsyn2 = Contraction.steps Contraction.max_steps intsyn in
  let intsyn3 = Simplify.f intsyn2 in
  let intsyn4 = ClosConv.f intsyn3 in
  (* print_endline (IntSyn.ppr_exp Ident.name intsyn4); *)
  let defs = Lifting.f intsyn4 in
  (* print_endline (IntSyn.ppr_defs defs); *)
  LlvmGen.codegen (Filename.basename path) defs;
  Llvm.print_module (Filename.remove_extension path ^ ".ll") !LlvmGen.the_module
