let test_line inp =
  let abssyn = Parse.parse_line inp in
  let abssyn' = Semant.Scoping.scoping_prog Semant.Scoping.empty abssyn in
  let aabssyn = Semant.TypeCheck.check_prog Semant.Env.empty abssyn' in
  let coresyn = AbsToCore.a2c_prog aabssyn in
  let cpssyn = CoreToCps.c2k_prog coresyn in
  Cps.TypeCheck.check_prog Cps.TypeCheck.empty cpssyn

let _ = test_line "42"

let _ = test_line "let x = 5 in x"

let _ =
  test_line "let quad x = let double x = x + x in double (double x) in quad 12"

let _ =
  test_line "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5"

let _ =
  test_line
    "let rec iseven n = if n = 0 then 1 else isodd (n - 1)\n\
     and isodd n = if n = 0 then 0 else iseven (n - 1) in\n\
     iseven 10"
