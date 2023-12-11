type id = Id.t

type oper = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Bool of bool

type ty = IntTy | BoolTy | FunTy of ty * ty

type exp =
  | Const of const
  | Var of id
  | App of {fcn: exp; arg: exp}
  | Lam of {var: id; body: exp}
  | Prim of {left: exp; oper: oper; right: exp}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {isrec: bool; vars: id list; bnds: exp list; body: exp}

type aexp = exp * ty

type prog = exp

let lams (ids : id list) (exp : exp) : exp =
  List.fold_right (fun id exp -> Lam {var= id; body= exp}) ids exp

let unlam : exp -> id list * exp = function
  | Lam {var; body} -> ([var], body)
  | exp -> ([], exp)

open Format

let pp_print_oper ppf oper : unit =
  pp_print_string ppf
    (List.assoc oper
       [ (Add, "+"); (Sub, "-"); (Mul, "*"); (Div, "/"); (Eq, "="); (Ne, "<>")
       ; (Lt, "<"); (Le, "<="); (Gt, ">"); (Ge, ">=") ] )

let pp_print_const ppf = function
  | Int i -> pp_print_int ppf i
  | Bool b -> pp_print_bool ppf b

let rec pp_print_exp outer ppf = function
  | Const c -> pp_print_const ppf c
  | Var id -> Id.pp_print_id ppf id
  | App {fcn; arg} ->
      ( if outer > 8 then fprintf ppf "(@[<1>%a %a@])"
        else fprintf ppf "@[<1>%a %a@]" )
        (pp_print_exp 8) fcn (pp_print_exp 9) arg
  | Lam {var; body} ->
      ( if outer > 0 then fprintf ppf "fun %a ->@;<1 2>%a"
        else fprintf ppf "fun %a ->@;<1 2>%a" )
        Id.pp_print_id var (pp_print_exp 0) body
  | Prim {left; oper= (Add | Sub) as oper; right} ->
      (if outer > 6 then fprintf ppf "(%a %a %a)" else fprintf ppf "%a %a %a")
        (pp_print_exp 6) left pp_print_oper oper (pp_print_exp 7) right
  | Prim {left; oper= (Mul | Div) as oper; right} ->
      (if outer > 7 then fprintf ppf "(%a %a %a)" else fprintf ppf "%a %a %a")
        (pp_print_exp 7) left pp_print_oper oper (pp_print_exp 8) right
  | Prim {left; oper= (Eq | Ne | Lt | Le | Gt | Ge) as oper; right} ->
      (if outer > 4 then fprintf ppf "(%a %a %a)" else fprintf ppf "%a %a %a")
        (pp_print_exp 4) left pp_print_oper oper (pp_print_exp 4) right
  | If {cond; then_; else_} ->
      ( if outer > 2 then
          fprintf ppf "(if %a@;<1 0>then@;<1 2>@[%a]@;<1 0>else@;<1 2>@[%a]@])"
        else
          fprintf ppf "if %a@;<1 0>then@;<1 2>@[%a@]@;<1 0>else@;<1 2>@[%a@]@]"
      )
        (pp_print_exp 0) cond (pp_print_exp 0) then_ (pp_print_exp 0) else_
  | Let {isrec; vars; bnds; body} ->
      ( if isrec then fprintf ppf "@[<hv 0>let rec %a@ in@;%a@]"
        else fprintf ppf "@[<hv 0>let@;<1 2>%a@ in@;%a@]" )
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf "@ and ")
           pp_print_bnd )
        (List.combine vars bnds) (pp_print_exp 0) body

and pp_print_bnd ppf (var, bnd) =
  fprintf ppf "%a =@;<1 2>%a" Id.pp_print_id var (pp_print_exp 0) bnd

let print_prog exp =
  pp_print_exp 0 std_formatter exp;
  print_newline ()
