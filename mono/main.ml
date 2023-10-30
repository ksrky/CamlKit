let semant abssyn =
  let abssyn' = Semant.Scoping.scoping_exp Semant.Scoping.initial abssyn in
  (* print_endline (Language.Syntax.ppr_exp abssyn); *)
  let _aabssyn = Semant.TypeCheck.check_prog Semant.Env.empty abssyn' in
  abssyn'

let secd instrs =
  Secd.State.init (); Secd.Operation.load_instrs instrs; Secd.Operation.run_commands ()

(** [run path] evaluates a source file on the virtual machine. *)
let run (path : string) =
  let abssyn = Parse.parse path in
  let abssyn' = semant abssyn in
  (* print_endline (IntSyn.ppr_exp Id.name intsyn); *)
  let coresyn = LangToCore.trexp abssyn' in
  (* print_endline (CoreSyn.ppr_exp Id.name coresyn); *)
  let instrs = CoreToSecd.f coresyn in
  (* print_endline (Secd.Operation.show_instrs instrs); *)
  secd instrs; print_newline ()

(** [eval inp] evaluates string [inp] on the virtual machine. *)
let eval (inp : string) =
  let abssyn = Parse.parse_line inp in
  let abssyn' = semant abssyn in
  (* print_endline (IntSyn.ppr_exp Id.name intsyn); *)
  let coresyn = LangToCore.trexp abssyn' in
  let instrs = CoreToSecd.f coresyn in
  (* print_endline (Secd.Operation.show_instrs instrs); *)
  secd instrs; print_newline ()

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
