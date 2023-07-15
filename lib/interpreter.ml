let eval (inp : string) =
  let abs = Parse.parse_line inp in
  (* print_endline (AbsSyn.pretty_exp abs); *)
  let sexp = Sexp.abs2sexp abs in
  let instrs = Compile.compile sexp in
  (* print_endline (Machine.show_instrs instrs); *)
  Stack.init (); Machine.load_instrs instrs; Machine.run_instrs ()
