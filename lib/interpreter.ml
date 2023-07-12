let eval () =
  let abs = Parse.parse_line () in
  let sexp = Sexp.abs2sexp abs in
  let instrs = Compile.compile sexp in
  Instrs.load_instrs instrs; Instrs.run_instrs ()
