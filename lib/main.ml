(** [run path] evaluates a source file on the virtual machine. *)
let run (path : string) =
  let abssyn = Parse.parse path in
  (* print_endline (AbsSyn.ppr_exp abssyn); *)
  let intsyn = Semant.trans_exp Env.empty abssyn in
  (* print_endline (IntSyn.ppr_exp intsyn); *)
  let instrs = Compile.compile intsyn in
  (* print_endline (Machine.show_instrs instrs); *)
  Stack.init (); Machine.load_instrs instrs; Machine.run_commands ()

(** [eval inp] evaluates string [inp] on the virtual machine. *)
let eval (inp : string) =
  let abssyn = Parse.parse_line inp in
  (* print_endline (AbsSyn.ppr_exp abssyn); *)
  let intsyn = Semant.trans_exp Env.empty abssyn in
  (* print_endline (IntSyn.ppr_exp intsyn); *)
  let instrs = Compile.compile intsyn in
  (* print_endline (Machine.show_instrs instrs); *)
  Stack.init (); Machine.load_instrs instrs; Machine.run_commands ()

(** [compile path] compiles a source file to LLVM IR and output to a .ll file. *)
let compile (path : string) : unit =
  let abssyn = Parse.parse path in
  (* print_endline (AbsSyn.ppr_exp abssyn); *)
  let intsyn = Semant.trans_exp Env.empty abssyn in
  (* print_endline (IntSyn.ppr_exp intsyn); *)
  let intsyn' = ClosConv.f intsyn in
  let defs = Lifting.f intsyn' in
  (* print_endline (IntSyn.ppr_defs defs); *)
  LlvmGen.codegen_builtins ();
  List.iter (fun def -> Llvm.dump_value (LlvmGen.codegen_func def)) defs;
  Llvm.print_module (Filename.remove_extension path ^ ".ll") LlvmGen.the_module
