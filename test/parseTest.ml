open CamlKit
open AbsSyn

let x = Ident.from_string "x"

let y = Ident.from_string "y"

let f = Ident.from_string "f"

let%test _ = Parse.parse_line "42" = IntExp 42

let%test _ = Parse.parse_line "3 - 7" = OpExp {left= IntExp 3; oper= MinusOp; right= IntExp 7}

let%test _ =
  Parse.parse_line "fun x y -> x + y"
  = LamExp {vars= [x; y]; body= OpExp {left= VarExp x; oper= PlusOp; right= VarExp y}}

let%test _ =
  Parse.parse_line "if 1 then 0 else 1" = IfExp {test= IntExp 1; then_= IntExp 0; else_= IntExp 1}

let%test _ =
  Parse.parse_line "let f x y = y in f 1"
  = LetExp
      { decs= [{name= f; params= [x; y]; body= VarExp y}]
      ; body= AppExp {fcn= VarExp f; arg= IntExp 1} }

let%test _ =
  Parse.parse_line "- f x"
  = OpExp {left= IntExp 0; oper= MinusOp; right= AppExp {fcn= VarExp f; arg= VarExp x}}
