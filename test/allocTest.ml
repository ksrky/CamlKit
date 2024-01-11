let test_line inp =
  let abssyn = Parse.parse_line inp in
  let abssyn' = Semant.Scoping.scoping_prog Semant.Scoping.empty abssyn in
  let aabssyn = Semant.TypeCheck.check_prog Semant.Env.empty abssyn' in
  let lamsyn = AbsToLam.a2c_prog aabssyn in
  let cpssyn = LamToCps.c2k_prog lamsyn in
  let cpssyn' = Cps.ClosConv.cc_prog cpssyn in
  let impsyn = CpsToAlloc.c2i_prog cpssyn' in
  Alloc.Syntax.print_prog impsyn;
  Alloc.TypeCheck.check_prog Alloc.TypeCheck.empty impsyn

let _ = test_line "42"

let _ = test_line "let x = 5 in x"

let _ =
  test_line "let quad x = let double x = x + x in double (double x) in quad 12"
