let eval (inp : string) =
  let abs = Parse.parse_line inp in
  (* print_endline (AbsSyn.pretty (-1) abs); *)
  let sexp = Sexp.abs2sexp abs in
  let instrs = Compile.compile sexp in
  (* print_endline (Instrs.show_instrs instrs); *)
  Machine.init (); Instrs.load_instrs instrs; Instrs.run_instrs ()