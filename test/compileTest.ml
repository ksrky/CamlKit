open CamlKit
open Sexp
open Instrs

let%test _ = Compile.compile Nil = [NIL; STOP]

let%test _ = Compile.compile (Int 42) = [LDC 42; STOP]

let%test _ = Compile.compile (Special (Builtin ("MUL", [Int 2; Int 4]))) = [LDC 4; LDC 2; MUL; STOP]

let%test _ =
  Compile.compile (Lam (["x"; "y"], Special (Builtin ("ADD", [Var "x"; Var "y"]))))
  = [LDF [LD (1, 2); LD (1, 1); ADD; RTN]; STOP]

let%test _ =
  Compile.compile (Special (Let (["x"], [Int 1], Special (Builtin ("PRINT", [Var "x"])))))
  = [NIL; LDC 1; CONS; LDF [LD (1, 1); WRITEC; RTN]; AP; STOP]
