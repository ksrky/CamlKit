open CamlKit
open AbsSyn
open IntSyn
open Semant

let x = Ident.from_string "x"

let y = Ident.from_string "y"

let f = Ident.from_string "f"

let%test _ = trans_exp Env.empty NilExp = Nil

let%test _ = trans_exp Env.empty (IntExp 42) = Int 42

let%test _ =
  trans_exp Env.empty
    (LamExp {vars= [x; y]; body= OpExp {left= VarExp x; oper= PlusOp; right= VarExp y}})
  = Lam ([x; y], Builtin ("add", [Var x; Var y]))

let%test _ =
  trans_exp Env.empty
    (LetExp
       { decs= [{name= f; params= [x; y]; body= VarExp y}]
       ; body= AppExp {fcn= VarExp f; arg= IntExp 1} } )
  = Let (false, [f], [Lam ([x; y], Var y)], App (Var f, [Int 1]))
