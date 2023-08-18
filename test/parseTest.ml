open CamlKit
open AbsSyn

let%test _ = Parse.parse_line "42" = IntExp 42

let%test _ = Parse.parse_line "3 - 7" = OpExp {left= IntExp 3; op= MinusOp; right= IntExp 7}

let%test _ = AbsSyn.ppr_exp (Parse.parse_line "fun x y -> x + y") = "fun x y -> x + y"

let%test _ =
  Parse.parse_line "if 1 then 0 else 1" = IfExp {test= IntExp 1; then_= IntExp 0; else_= IntExp 1}

let%test _ = AbsSyn.ppr_exp (Parse.parse_line "let f x y = y in f 1") = "let f x y = y in f 1"

let%test _ = AbsSyn.ppr_exp (Parse.parse_line "- f x") = "0 - f x"
