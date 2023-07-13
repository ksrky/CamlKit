open CamlKit
open AbsSyn

let%test _ = Parse.parse_line "nil" = NilExp

let%test _ = Parse.parse_line "42" = IntExp 42

let%test _ = Parse.parse_line "3 - 7" = OpExp {left= IntExp 3; oper= MinusOp; right= IntExp 7}

let%test _ =
  Parse.parse_line "fun x y -> x + y"
  = LamExp {vars= ["x"; "y"]; body= OpExp {left= VarExp "x"; oper= PlusOp; right= VarExp "y"}}

let%test _ =
  Parse.parse_line "if 1 then 0 else 1" = IfExp {test= IntExp 1; then'= IntExp 0; else'= IntExp 1}

let%test _ =
  Parse.parse_line "let f x y = y in f 1 end"
  = LetExp
      { decs= [{name= "f"; params= ["x"; "y"]; body= VarExp "y"}]
      ; body= AppExp {fcn= VarExp "f"; arg= IntExp 1} }
