open CamlKit

let%test _ = Main.eval "42" = ()

let%test _ = Main.eval "1 + 2" = ()

let%test _ = Main.eval "6 > 9" = ()

let%test _ = Main.eval "if true then 10 else -10" = ()

let%expect_test _ = Main.eval "print_int 1"; [%expect {|  1  |}]

let%expect_test _ =
  Main.eval "let x = 5 in print_int x";
  [%expect {|  5  |}]

let%expect_test _ =
  Main.eval "print_int (let f x y = x + y in f (2 * 3) (6 - 4))";
  [%expect {|  8  |}]

let%expect_test _ =
  Main.eval "print_int (if 1 = 0 then 1 else 0)";
  [%expect {|  0  |}]

let%expect_test _ =
  Main.eval "let rec fact n = if n = 0 then 1 else n * fact (n-1) in print_int (fact 5)";
  [%expect {|  120  |}]

let%expect_test _ =
  Main.eval "let add2 n = 2 + n and double f x = f (f x) in print_int (double add2 3)";
  [%expect {|  7  |}]
