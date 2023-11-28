let semant abssyn =
  (* print_endline (Language.Syntax.ppr_exp abssyn); *)
  let abssyn' = Semant.Scoping.scoping_exp Semant.Scoping.empty abssyn in
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
  if !Semant.Error.has_error then exit 1;
  let coresyn = LangToCore.l2c_exp abssyn' in
  (* print_endline (Core.Syntax.ppr_exp Id.name coresyn); *)
  let instrs = CoreToSecd.c2s_prog coresyn in
  run_secd instrs

(** [eval inp] evaluates string [inp] on the virtual machine. *)
let eval (inp : string) =
  let abssyn = Parse.parse_line inp in
  let abssyn' = semant abssyn in
  if !Semant.Error.has_error then exit 1;
  let coresyn = LangToCore.l2c_exp abssyn' in
  (* print_endline (Core.Syntax.ppr_exp Id.name coresyn); *)
  let instrs = CoreToSecd.c2s_prog coresyn in
  run_secd instrs

(** [compile path] compiles a source file to LLVM IR and output to a .ll file. *)
let compile (path : string) : unit =
  let abssyn = Parse.parse path in
  let abssyn' = semant abssyn in
  if !Semant.Error.has_error then exit 1;
  let coresyn = LangToCore.l2c_exp abssyn' in
  (* print_endline (Core.Syntax.ppr_exp Id.name coresyn); *)
  let coresyn' = Core.ClosConv.cc_prog coresyn in
  (* print_endline (Core.Syntax.ppr_exp Id.name coresyn'); *)
  let cgcodes = CoreToCg.c2cg_exp coresyn' in
  (* print_endline (CodeGen.Syntax.ppr_codes cgcodes); *)
  let llmod = CodeGen.codegen (Filename.basename path) cgcodes in
  CodeGen.format path llmod
