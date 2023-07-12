let eval (inp : string) =
  let abs = Parse.parse_line inp in
  let sexp = Sexp.abs2sexp abs in
  let instrs = Compile.compile sexp in
  (* print_endline (Instrs.show_instrs instrs); *)
  Instrs.load_instrs instrs; Instrs.run_instrs ()
