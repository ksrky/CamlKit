open CamlKit
open IntSyn
open Machine

let x = Ident.from_string "x"

let y = Ident.from_string "y"

let%test _ = Compile.compile Nil = [NIL; STOP]

let%test _ = Compile.compile (Int 42) = [LDC 42; STOP]

let%test _ = Compile.compile (Builtin ("mul", [Int 2; Int 4])) = [LDC 4; LDC 2; MUL; STOP]

let%test _ =
  Compile.compile (Lam ([x; y], Builtin ("add", [Var x; Var y])))
  = [LDF [LD (1, 2); LD (1, 1); ADD; RTN]; STOP]

let%test _ =
  Compile.compile (Let (false, [x], [Int 1], Builtin ("printi", [Var x])))
  = [NIL; LDC 1; CONS; LDF [LD (1, 1); PRINTI; RTN]; AP; STOP]