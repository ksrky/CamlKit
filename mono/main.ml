let semant (abssyn : Abstract.Syntax.exp) : Abstract.Syntax.aprog =
  let open Abstract in
  let open Semant in
  (* Printer.print_prog abssyn; *)
  let abssyn' = Scoping.scoping_prog Scoping.empty abssyn in
  (* Printer.print_prog abssyn'; *)
  let aabssyn = TypeCheck.check_prog Env.empty abssyn' in
  (* Printer.print_aprog aabssyn; *)
  aabssyn

let run_secd instrs =
  let open Secd in
  (* print_endline (Operation.show_instrs instrs); *)
  State.init ();
  Operation.load_instrs instrs;
  Operation.run_commands ()

(** [run path] evaluates a source file on the virtual machine. *)
let run (path : string) =
  let abssyn = Parse.parse path in
  let aabssyn = semant abssyn in
  if !Semant.Error.has_error then exit 1;
  let sexpsyn = AbsToSexp.a2s_prog aabssyn in
  (* Sexp.Syntax.print_prog sexpsyn; *)
  let instrs = SexpToSecd.sx2s_prog sexpsyn in
  run_secd instrs

(** [eval inp] evaluates string [inp] on the virtual machine. *)
let eval (inp : string) =
  let abssyn = Parse.parse_line inp in
  let aabssyn = semant abssyn in
  if !Semant.Error.has_error then exit 1;
  let sexpsyn = AbsToSexp.a2s_prog aabssyn in
  (* Sexp.Syntax.print_prog lamsyn; *)
  let instrs = SexpToSecd.sx2s_prog sexpsyn in
  run_secd instrs

(** [compile path] compiles a source file to LLVM IR and output to a .ll file. *)
let compile (path : string) : unit =
  let abssyn = Parse.parse path in
  let aabssyn = semant abssyn in
  if !Semant.Error.has_error then exit 1;
  let lamsyn = AbsToLam.a2c_prog aabssyn in
  (* Lambda.Syntax.print_prog lamsyn; *)
  let cpssyn = LamToCps.c2k_prog lamsyn in
  (* Cps.Syntax.print_prog cpssyn; *)
  let cpssyn' = Cps.ClosConv.cc_prog cpssyn in
  (* Cps.ClosConv.print_prog cpssyn'; *)
  let impsyn = CpsToAlloc.c2i_prog cpssyn' in
  (* Alloc.Syntax.print_prog impsyn; *)
  let llmod = Alloc.LlvmGen.codegen (Filename.basename path) impsyn in
  Alloc.LlvmGen.emit path llmod

(* let compile (path : string) : unit =
   let abssyn = Parse.parse path in
   let aabssyn = semant abssyn in
   if !Semant.Error.has_error then exit 1;
   let lamsyn = AbsToLam.a2c_prog aabssyn in
   (* Lambda.Syntax.print_prog lamsyn; *)
   let anfsyn = LamToAnf.l2a_prog lamsyn in
    Anf.Syntax.print_prog anfsyn; 
   let anfsyn' = Anf.ClosConv.cc_prog anfsyn in
   Anf.ClosConv.print_prog anfsyn';
*)
