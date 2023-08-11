let emit (frag : Virtual.frag) : unit =
  let ilist = X86Gen.procEntryExit frag in
  print_endline (String.concat "\n" ilist)

let compile (src : string) : unit =
  let abs = Parse.parse src in
  (* print_endline (AbsSyn.pretty_exp abs); *)
  let intsyn = Semant.trans_exp Env.empty abs in
  let intsyn' = ClosConv.f intsyn in
  let defs = Lifting.f intsyn' in
  let frags = List.map Compile.compile_def defs in
  (* print_endline (Machine.show_instrs instrs); *)
  List.iter emit frags
