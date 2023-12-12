let semant (abssyn : Abstract.Syntax.exp) : Abstract.Syntax.aexp =
  (* print_endline (Abstract.Syntax.ppr_exp abssyn); *)
  let abssyn' = Semant.Scoping.scoping_exp Semant.Scoping.empty abssyn in
  (* print_endline (Abstract.Syntax.ppr_exp abssyn'); *)
  let aabssyn = Semant.TypeCheck.check_prog Semant.Env.empty abssyn' in
  (* print_endline (Abstract.Syntax.ppr_aexp 0 _aabssyn); *)
  aabssyn

let run_secd instrs =
  (* print_endline (Secd.Operation.show_instrs instrs); *)
  Secd.State.init ();
  Secd.Operation.load_instrs instrs;
  Secd.Operation.run_commands ()

(** [run path] evaluates a source file on the virtual machine. *)
let run (path : string) =
  let abssyn = Parse.parse path in
  let aabssyn = semant abssyn in
  if !Semant.Error.has_error then exit 1;
  let coresyn = AbsToCore.a2c_exp aabssyn in
  (* Core.Syntax.print_prog coresyn; *)
  let instrs = CoreToSecd.c2s_prog coresyn in
  run_secd instrs

(** [eval inp] evaluates string [inp] on the virtual machine. *)
let eval (inp : string) =
  let abssyn = Parse.parse_line inp in
  let aabssyn = semant abssyn in
  if !Semant.Error.has_error then exit 1;
  let coresyn = AbsToCore.a2c_exp aabssyn in
  (* Core.Syntax.print_prog coresyn; *)
  let instrs = CoreToSecd.c2s_prog coresyn in
  run_secd instrs

(** [compile path] compiles a source file to LLVM IR and output to a .ll file. *)
let compile (path : string) : unit =
  let abssyn = Parse.parse path in
  let abssyn' = semant abssyn in
  if !Semant.Error.has_error then exit 1;
  let coresyn = AbsToCore.a2c_exp abssyn' in
  (* Core.Syntax.print_prog coresyn; *)
  let cpssyn = CoreToCps.c2k_prog coresyn in
  (* Cps.Syntax.print_prog cpssyn; *)
  let cpssyn' = Cps.ClosConv.cc_prog cpssyn in
  (* Cps.ClosConv.print_prog cpssyn'; *)
  let impsyn = CpsToImp.c2i_prog cpssyn' in
  (* Imp.Syntax.print_prog impsyn; *)
  let llmod = Imp.LlvmGen.codegen (Filename.basename path) impsyn in
  Imp.LlvmGen.format path llmod
