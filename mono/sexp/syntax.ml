type id = Id.t

type prim = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Bool of bool

type exp =
  | Const of const
  | Var of id
  | App of {fcn: exp; args: exp list}
  | Prim of {prim: prim; args: exp list}
  | Lam of {vars: id list; body: exp}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {isrec: bool; vars: id list; bnds: exp list; body: exp}

type prog = exp

open Format

let pp_print_prim ppf prim =
  pp_print_string ppf
    (List.assoc prim
       [ (Add, "+"); (Sub, "-"); (Mul, "*"); (Div, "/"); (Eq, "="); (Ne, "<>")
       ; (Lt, "<"); (Le, "<="); (Gt, ">"); (Ge, ">=") ] )

let rec pp_print_exp ppf = function
  | Const (Int i) -> fprintf ppf "%d" i
  | Const (Bool b) -> fprintf ppf "%b" b
  | Var x -> Id.pp_print_id ppf x
  | App {fcn; args} ->
      fprintf ppf "@[<2>(%a@ %a)@]" pp_print_exp fcn
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_exp)
        args
  | Prim {prim; args} ->
      fprintf ppf "@[<2>(%a@ %a)@]" pp_print_prim prim
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_exp)
        args
  | Lam {vars; body} ->
      fprintf ppf "@[<2>(fun@ (%a)@ %a)@]"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") Id.pp_print_id)
        vars pp_print_exp body
  | If {cond; then_; else_} ->
      fprintf ppf "@[<2>(if@ %a@ then@ %a@ else@ %a)@]" pp_print_exp cond
        pp_print_exp then_ pp_print_exp else_
  | Let {isrec; vars; bnds; body} ->
      fprintf ppf "@[<2>(let%a@ %a@ %a@ %a)@]"
        (fun ppf () -> if isrec then fprintf ppf " rec")
        ()
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") Id.pp_print_id)
        vars
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_exp)
        bnds pp_print_exp body

let pp_print_prog ppf prog = pp_print_exp ppf prog

let print_prog prog = pp_print_prog std_formatter prog
