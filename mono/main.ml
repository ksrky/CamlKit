let semant abssyn =
  let abssyn' = Semant.Scoping.scoping_exp Semant.Scoping.initial abssyn in
  (* print_endline (Language.Syntax.ppr_exp abssyn'); *)
  let _aabssyn = Semant.TypeCheck.check_prog Semant.Env.empty abssyn' in
  (* print_endline (Language.Syntax.ppr_aexp 0 _aabssyn); *)
  abssyn'

let run_secd instrs =
  (* print_endline (Secd.Operation.show_instrs instrs); *)
  Secd.State.init ();
  Secd.Operation.load_instrs instrs;
  Secd.Operation.run_commands ();
  print_int (Secd.State.exit_code ());
  print_newline ()

(** [run path] evaluates a source file on the virtual machine. *)
let run (path : string) =
  let abssyn = Parse.parse path in
  let abssyn' = semant abssyn in
  let coresyn = LangToCore.l2c_exp abssyn' in
  (* print_endline (Core.Syntax.ppr_exp Id.name coresyn); *)
  let instrs = CoreToSecd.f coresyn in
  run_secd instrs

(** [eval inp] evaluates string [inp] on the virtual machine. *)
let eval (inp : string) =
  let abssyn = Parse.parse_line inp in
  let abssyn' = semant abssyn in
  let coresyn = LangToCore.l2c_exp abssyn' in
  (* print_endline (Core.Syntax.ppr_exp Id.name coresyn); *)
  let instrs = CoreToSecd.f coresyn in
  run_secd instrs

(** [compile path] compiles a source file to LLVM IR and output to a .ll file. *)
let compile (path : string) : unit =
  let abssyn = Parse.parse path in
  let abssyn' = semant abssyn in
  let coresyn = LangToCore.l2c_exp abssyn' in
  (* print_endline (Core.Syntax.ppr_exp Id.name coresyn); *)
  let llcodes = CoreToLlvm.c2l_exp coresyn in
  (* print_endline (LlvmGen.Syntax.ppr_codes llcodes); *)
  let llmod = LlvmGen.CodeGen.codegen (Filename.basename path) llcodes in
  LlvmGen.CodeGen.format path llmod

(*
    (** [compile path] compiles a source file to LLVM IR and output to a .ll file. *)
    let compile (path : string) : unit =
      let abssyn = Parse.parse path in
      let abssyn' = Semant.Scoping.scoping_exp Semant.Scoping.initial abssyn in
      (* print_endline (Language.Syntax.ppr_exp abssyn); *)
      let intsyn, _ = Semant.TypeCheck.check_prog Semant.Env.empty abssyn' in
      (* print_endline (IntSyn.ppr_exp Id.name intsyn); *)
      let intsyn2 = Contraction.steps Contraction.max_steps intsyn in
      let intsyn3 = Simplify.f intsyn2 in
      (* print_endline (IntSyn.ppr_exp Id.name intsyn4); *)
      let frags = Lifting.f intsyn3 in
      (* print_endline (IntSyn.ppr_frags frags); *)
      LlvmGen.codegen (Filename.basename path) frags;
      Llvm.print_module (Filename.remove_extension path ^ ".ll") !LlvmGen.the_module
*)
