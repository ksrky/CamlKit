let test_line inp =
  let abssyn = Parse.parse_line inp in
  let abssyn' = Semant.Scoping.scoping_prog Semant.Scoping.empty abssyn in
  let aabssyn = Semant.TypeCheck.check_prog Semant.Env.empty abssyn' in
  let lamsyn = AbsToLam.a2l_prog aabssyn in
  let anfsyn = LamToAnf.l2a_prog lamsyn in
  let anfsyn' = Anf.ClosConv.cc_prog anfsyn in
  let alcsyn = AnfToAlloc.a2a_prog anfsyn' in
  Alloc.TypeCheck.check_prog Alloc.TypeCheck.empty alcsyn

let _ = test_line "42"

(*let _ = test_line "let x = 5 in x"

  let _ =
    test_line "let quad x = let double x = x + x in double (double x) in quad 12" *)
