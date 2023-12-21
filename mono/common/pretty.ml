open Format

let pp_with_paren pp ppf = fprintf ppf "(%a)" pp

let pp_print_lparen ppf = fprintf ppf "("

let pp_print_rparen ppf = fprintf ppf ")"

let with_prec outer inner pp ppf =
  if outer > inner then pp_print_lparen ppf;
  pp ppf;
  if outer > inner then pp_print_rparen ppf
