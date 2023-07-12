open CamlKit

let%test _ = Interpreter.eval "nil" = ()

let%test _ = Interpreter.eval "42" = ()

let%test _ = Interpreter.eval "1 + 2" = ()

let%test _ = Interpreter.eval "if 1 then 10 else -10" = ()

let%expect_test _ = Interpreter.eval "print 1"; [%expect {|  1  |}]
