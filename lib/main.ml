let run (path : string) =
  let abs = Parse.parse path in
  (* print_endline (AbsSyn.pretty_exp abs); *)
  let sexp = Semant.trans_exp Env.empty abs in
  let instrs = Compile.compile sexp in
  (* print_endline (Machine.show_instrs instrs); *)
  Stack.init (); Machine.load_instrs instrs; Machine.run_instrs ()

let eval (inp : string) =
  let abs = Parse.parse_line inp in
  (* print_endline (AbsSyn.pretty_exp abs); *)
  let sexp = Semant.trans_exp Env.empty abs in
  let instrs = Compile.compile sexp in
  (* print_endline (Machine.show_instrs instrs); *)
  Stack.init (); Machine.load_instrs instrs; Machine.run_instrs ()
