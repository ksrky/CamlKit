let test_line inp =
  let abssyn = Parse.parse_line inp in
  let abssyn' = Semant.Scoping.scoping_prog Semant.Scoping.empty abssyn in
  let aabssyn = Semant.TypeCheck.check_prog Semant.Env.empty abssyn' in
  let coresyn = AbsToLam.a2c_prog aabssyn in
  Lambda.TypeCheck.check_prog Lambda.TypeCheck.empty coresyn

let _ = test_line "42"

let _ = test_line "let x = 5 in x"

let _ =
  test_line "let quad x = let double x = x + x in double (double x) in quad 12"
