open CamlKit
open AbsSyn
open IntSyn
open Semant

let x = Ident.from_string "x"

let y = Ident.from_string "y"

let f = Ident.from_string "f"

let%test _ = check_exp Env.empty NilExp NIL = Nil

let%test _ = check_exp Env.empty (IntExp 42) NIL = Int 42

let%test _ =
  check_exp Env.empty
    (LamExp {vars= [x; y]; body= OpExp {left= VarExp x; op= PlusOp; right= VarExp y}})
    NIL
  = Lam ([x; y], Builtin ("add", [Var x; Var y]))

let%test _ =
  check_exp Env.empty
    (LetExp
       { bnds= [{name= f; params= [x; y]; body= VarExp y}]
       ; body= AppExp {fcn= VarExp f; arg= IntExp 1} } )
    NIL
  = Let (false, [f], [Lam ([x; y], Var y)], App (Var f, [Int 1]))
