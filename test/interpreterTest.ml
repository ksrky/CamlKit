open CamlKit

let%test _ = Interpreter.eval "nil" = ()

let%test _ = Interpreter.eval "42" = ()

let%test _ = Interpreter.eval "1 + 2" = ()

let%test _ = Interpreter.eval "6 > 9" = ()

let%test _ = Interpreter.eval "if 1 then 10 else -10" = ()

let%expect_test _ = Interpreter.eval "print_int 1"; [%expect {|  1  |}]

let%expect_test _ =
  Interpreter.eval "let x = 5 in print_int x";
  [%expect {|  5  |}]

let%expect_test _ =
  Interpreter.eval "print_int (let f x y = x + y in f (2 * 3) (6 - 4))";
  [%expect {|  8  |}]

let%expect_test _ =
  Interpreter.eval "print_int (if 1 = 0 then 1 else 0)";
  [%expect {|  0  |}]
