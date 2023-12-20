open Common
open Abstract.Syntax

let%test _ = Parse.parse_line "42" = IntExp 42

let%test _ =
  Parse.parse_line "3 - 7" = OpExp {left= IntExp 3; op= MinusOp; right= IntExp 7}

let%test _ =
  Parse.parse_line "if true then 0 else 1"
  = IfExp {cond= BoolExp true; then_= IntExp 0; else_= IntExp 1}
