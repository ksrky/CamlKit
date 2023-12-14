type id = Id.t

type oper = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Bool of bool

type ty = IntTy | BoolTy | FunTy of ty * ty

type var = id * ty

type exp =
  | Const of const
  | Var of id
  | App of {fcn: expty; arg: expty}
  | Lam of {var: var; body: expty}
  | Prim of {left: expty; oper: oper; right: expty}
  | If of {cond: expty; then_: expty; else_: expty}
  | Let of {isrec: bool; vars: var list; bnds: expty list; body: expty}

and expty = exp * ty

type prog = exp

let fun_ty ty1 ty2 = FunTy (ty1, ty2)

let fun_tys (tys : ty list) (ty : ty) : ty =
  List.fold_right (fun ty1 ty2 -> FunTy (ty1, ty2)) tys ty

let mk_funty (vars : var list) = fun_tys (List.map snd vars)

let lams (ids : var list) (expty : expty) : expty =
  List.fold_right
    (fun var exp -> (Lam {var; body= exp}, fun_ty (snd var) (snd exp)))
    ids expty

let unlam : expty -> var option * expty = function
  | Lam {var; body}, _ -> (Some var, body)
  | exp -> (None, exp)

open Format

let pp_print_oper ppf oper : unit =
  pp_print_string ppf
    (List.assoc oper
       [ (Add, "+"); (Sub, "-"); (Mul, "*"); (Div, "/"); (Eq, "="); (Ne, "<>")
       ; (Lt, "<"); (Le, "<="); (Gt, ">"); (Ge, ">=") ] )

let pp_print_const ppf = function
  | Int i -> pp_print_int ppf i
  | Bool b -> pp_print_bool ppf b

let rec pp_print_ty outer ppf = function
  | IntTy -> pp_print_string ppf "int"
  | BoolTy -> pp_print_string ppf "bool"
  | FunTy (ty1, ty2) ->
      (if outer > 1 then fprintf ppf "(%a ->@ %a)" else fprintf ppf "%a ->@ %a")
        (pp_print_ty 2) ty1 (pp_print_ty 1) ty2

let pp_print_var ppf (id, ty) =
  fprintf ppf "%a: %a" Id.pp_print_id id (pp_print_ty 0) ty

let rec pp_print_exp outer ppf = function
  | Const c -> pp_print_const ppf c
  | Var id -> Id.pp_print_id ppf id
  | App {fcn; arg} ->
      ( if outer > 8 then fprintf ppf "(@[<1>%a %a@])"
        else fprintf ppf "@[<1>%a %a@]" )
        (pp_print_expty 8) fcn (pp_print_expty 9) arg
  | Lam {var; body} ->
      ( if outer > 0 then fprintf ppf "fun %a ->@;<1 2>%a"
        else fprintf ppf "fun %a ->@;<1 2>%a" )
        pp_print_var var (pp_print_expty 0) body
  | Prim {left; oper= (Add | Sub) as oper; right} ->
      (if outer > 6 then fprintf ppf "(%a %a %a)" else fprintf ppf "%a %a %a")
        (pp_print_expty 6) left pp_print_oper oper (pp_print_expty 7) right
  | Prim {left; oper= (Mul | Div) as oper; right} ->
      (if outer > 7 then fprintf ppf "(%a %a %a)" else fprintf ppf "%a %a %a")
        (pp_print_expty 7) left pp_print_oper oper (pp_print_expty 8) right
  | Prim {left; oper= (Eq | Ne | Lt | Le | Gt | Ge) as oper; right} ->
      (if outer > 4 then fprintf ppf "(%a %a %a)" else fprintf ppf "%a %a %a")
        (pp_print_expty 4) left pp_print_oper oper (pp_print_expty 4) right
  | If {cond; then_; else_} ->
      ( if outer > 2 then
          fprintf ppf "(if %a@;<1 0>then@;<1 2>@[%a]@;<1 0>else@;<1 2>@[%a]@])"
        else
          fprintf ppf "if %a@;<1 0>then@;<1 2>@[%a@]@;<1 0>else@;<1 2>@[%a@]@]"
      )
        (pp_print_expty 0) cond (pp_print_expty 0) then_ (pp_print_expty 0)
        else_
  | Let {isrec; vars; bnds; body} ->
      ( if isrec then fprintf ppf "@[<hv 0>let rec %a@ in@;%a@]"
        else fprintf ppf "@[<hv 0>let@;<1 2>%a@ in@;%a@]" )
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf "@ and ")
           pp_print_bnd )
        (List.combine vars bnds) (pp_print_expty 0) body

and pp_print_expty outer ppf (exp, ty) = pp_print_exp outer ppf exp

and pp_print_bnd ppf (var, bnd) =
  fprintf ppf "%a =@;<1 2>%a" pp_print_var var (pp_print_expty 0) bnd

let print_prog exp =
  pp_print_exp 0 std_formatter exp;
  print_newline ()
