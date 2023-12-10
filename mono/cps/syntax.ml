module C = Core.Syntax

type id = Id.t

type oper = C.oper

type const = C.const

type value =
  | Const of const
  | Var of id
  | Glb of id
  | Lam of {vars: id list; body: exp}
  | Tuple of value list

and fundef = {name: id; vars: id list; body: exp}

and exp =
  | Let of {dec: dec; body: exp}
  | Letrec of {fundefs: fundef list; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: id; val_: value}
  | PrimDec of {name: id; left: value; oper: oper; right: value}
  | ProjDec of {name: id; val_: value; idx: int}

type prog = exp

let mk_vars xs = List.map (fun x -> Var x) xs

let mk_lam var body = Lam {vars= [var]; body}

let mk_lams vars body = Lam {vars; body}

let mk_app fcn arg = App {fcn; args= [arg]}

let mk_apps fcn args = App {fcn; args}

let mk_let (decs : dec list) (body : exp) : exp =
  List.fold_right (fun d e -> Let {dec= d; body= e}) decs body

let mk_projs val_ (names : id list) : dec list =
  List.mapi (fun i name -> ProjDec {name; val_; idx= i + 1}) names

let parens (outer : int) (prec : int) s =
  if outer > prec then "(" ^ s ^ ")" else s

open Format

let pp_print_id ppf id = fprintf ppf "%s" (Id.unique_name id)

let pp_print_const ppf = function
  | Core.Syntax.Int i -> fprintf ppf "%d" i
  | Core.Syntax.Nil -> fprintf ppf "nil"

let rec pp_print_val paren ppf = function
  | Const c -> pp_print_const ppf c
  | Var x -> pp_print_id ppf x
  | Glb x -> fprintf ppf "$%s" (Id.unique_name x)
  | Lam {vars; body} ->
      fprintf ppf
        (if paren then "(@[<1>fun %a ->@ %a@])" else "@[<1>fun %a ->@ %a@]")
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_id)
        vars pp_print_exp body
  | Tuple vals ->
      fprintf ppf "(%a)"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
           (pp_print_val false) )
        vals

and pp_print_exp ppf = function
  | Let {dec; body} ->
      fprintf ppf "@[<2>let@ %a@]@ in@ %a" pp_print_dec dec pp_print_exp body
  | Letrec {fundefs; body} ->
      fprintf ppf "let rec@ %a@ in@ %a"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf "@ and ")
           pp_print_fundef )
        fundefs pp_print_exp body
  | App {fcn; args} ->
      fprintf ppf "@[<2>%a@ %a@]" (pp_print_val true) fcn
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf " ")
           (pp_print_val true) )
        args
  | If {cond; then_; else_} ->
      fprintf ppf "if %a then@ %a@]@;@[<v 2>else@ %a@]" (pp_print_val true) cond
        pp_print_exp then_ pp_print_exp else_
  | Halt val_ -> fprintf ppf "halt %a" (pp_print_val true) val_

and pp_print_fundef ppf {name; vars; body} =
  fprintf ppf "@[<2>%a %a =@ %a@]" pp_print_id name
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_id)
    vars pp_print_exp body

and pp_print_dec ppf : dec -> unit = function
  | ValDec {name; val_} ->
      fprintf ppf "%a =@ %a" pp_print_id name (pp_print_val false) val_
  | PrimDec {name; left; oper; right} ->
      fprintf ppf "%a =@ %a %s %a" pp_print_id name (pp_print_val true) left
        (List.assoc oper
           [ (C.Add, "+"); (C.Sub, "-"); (C.Mul, "*"); (C.Div, "/"); (C.Eq, "=")
           ; (C.Ne, "<>"); (C.Lt, "<"); (C.Le, "<="); (C.Gt, ">"); (C.Ge, ">=")
           ] )
        (pp_print_val true) right
  | ProjDec {name; val_; idx} ->
      fprintf ppf "%a =@ %a.%i" pp_print_id name (pp_print_val true) val_ idx

let pp_print_prog ppf exp = pp_print_exp ppf exp; print_newline ()

let print_prog exp = pp_print_prog std_formatter exp
