open CamlKit
open AbsSyn
open Sexp

let x = Ident.from_string "x"

let y = Ident.from_string "y"

let f = Ident.from_string "f"

let%test _ = abs2sexp NilExp = Nil

let%test _ = abs2sexp (IntExp 42) = Int 42

let%test _ =
  abs2sexp (LamExp {vars= [x; y]; body= OpExp {left= VarExp x; oper= PlusOp; right= VarExp y}})
  = Lam ([x; y], Special (Builtin ("ADD", [Var x; Var y])))

let%test _ =
  abs2sexp
    (LetExp
       { decs= [{name= f; params= [x; y]; body= VarExp y}]
       ; body= AppExp {fcn= VarExp f; arg= IntExp 1} } )
  = Special (Let ([f], [Lam ([x; y], Var y)], Seq [Var f; Int 1]))
