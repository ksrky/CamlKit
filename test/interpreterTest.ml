open CamlKit

let%test _ = Interpreter.eval "nil" = ()

let%test _ = Interpreter.eval "42" = ()

let%test _ = Interpreter.eval "1 + 2" = ()

let%test _ = Interpreter.eval "if 1 then 10 else -10" = ()

let%expect_test _ = Interpreter.eval "print 1"; [%expect {|  1  |}]

let%expect_test _ =
  Interpreter.eval "let x = 5 in print x end";
  [%expect {|  5  |}]

let%expect_test _ =
  Interpreter.eval "print (let f x y = x + y in f (2 * 3) (6 - 4) end)";
  [%expect {|  8  |}]
