open CamlKit
open IntSyn
open Machine

let x = Ident.from_string "x"

let y = Ident.from_string "y"

let%test _ = Compile.f Nil = [NIL; STOP]

let%test _ = Compile.f (Int 42) = [LDC 42; STOP]

let%test _ = Compile.f (Prim ("mul", [Int 2; Int 4])) = [LDC 4; LDC 2; MUL; STOP]

let%test _ =
  Compile.f (Lam ([x; y], Prim ("add", [Var x; Var y])))
  = [LDF [LD (1, 2); LD (1, 1); ADD; RTN]; STOP]

let%test _ =
  Compile.f (Let (false, [x], [Int 1], Prim ("printi", [Var x])))
  = [NIL; LDC 1; CONS; LDF [LD (1, 1); PRINTI; RTN]; AP; STOP]
