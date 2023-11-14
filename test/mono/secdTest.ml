open Mono

(* simple integer *)
let%expect_test _ = Main.eval "42"; [%expect {| 42 |}]

(* binary operator *)
let%expect_test _ = Main.eval "3 - 7"; [%expect {| -4 |}]

(* if expression *)
let%expect_test _ =
  Main.eval "if true then 10 else -10";
  [%expect {| 10 |}]

(* lambda abstraction and application *)
let%expect_test _ =
  Main.eval "(fun x -> x + 1) 2";
  [%expect {| 3 |}]

(* let expression with mutiple arguments *)
let%expect_test _ =
  Main.eval "let mod x y = x - y * (x / y) in mod 10 3";
  [%expect {| 1 |}]

(* higher order function *)
let%expect_test _ =
  Main.eval "let twice f x = f (f x) in twice (fun x -> x + 1) 0";
  [%expect {| 2 |}]

(* recursive function *)
let%expect_test _ =
  Main.eval "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5";
  [%expect {| 120 |}]

(* recursive function with multiple arguments *)
let%expect_test _ =
  Main.eval
    "let rec gcd x y = if y = 0 then x else gcd y (x - y * (x / y)) in gcd 8 12";
  [%expect {| 4 |}]

(* nested function definitions *)
let%expect_test _ =
  Main.eval "let f x = let g y = let h z = x + y + z in h 3 in g 2 in f 1";
  [%expect {| 6 |}]

(* mutually recursive functions *)
let%expect_test _ =
  Main.eval
    "let rec even n = if n = 0 then true else odd (n - 1)\n\
    \    and odd n = if n = 0 then false else even (n - 1) in even 10";
  [%expect {| 1 |}]
