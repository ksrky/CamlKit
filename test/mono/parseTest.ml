open Mono
open Abstract.Syntax

let%test _ = Parse.parse_line "42" = IntExp 42

let%test _ =
  Parse.parse_line "3 - 7" = OpExp {left= IntExp 3; op= MinusOp; right= IntExp 7}

let%test _ = ppr_exp (Parse.parse_line "fun x y -> x + y") = "fun x y -> x + y"

let%test _ =
  Parse.parse_line "if true then 0 else 1"
  = IfExp {cond= BoolExp true; then_= IntExp 0; else_= IntExp 1}

let%test _ =
  ppr_exp (Parse.parse_line "let f x y = y in f 1") = "let f x y = y in f 1"

let%test _ = ppr_exp (Parse.parse_line "- f x") = "0 - f x"

let%test _ =
  ppr_exp (Parse.parse_line "let rec o x = e (x-1) and e x = o (x-1) in o 11")
  = "let rec o x = e (x - 1) and e x = o (x - 1) in o 11"
