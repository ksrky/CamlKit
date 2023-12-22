exception Bug_error

let bug () = raise Bug_error

exception Unreachable

open Format

let with_paren ?(b = true) pp ppf =
  if b then fprintf ppf "(";
  pp ppf;
  if b then fprintf ppf ")"
