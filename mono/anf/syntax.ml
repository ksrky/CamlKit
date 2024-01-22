module L = Lambda.Syntax

type id = Id.t

type oper = L.oper

type const = L.const

type ty =
  | IntTy
  | BoolTy
  | FunTy of ty list * ty
  | TupleTy of ty list
  | CodeTy of ty * ty list * ty

type var = id * ty

type value =
  | Const of const
  | Var of id
  | Glb of id
  | Lam of {vars: var list; body: expty}
  | Tuple of valty list

and valty = value * ty

and exp =
  | Let of {dec: dec; body: expty}
  | If of {cond: valty; then_: expty; else_: expty}
  | Ret of valty

and dec =
  | ValDec of {var: var; val_: valty}
  | CallDec of {var: var; fcn: valty; args: valty list}
  | PrimDec of {var: var; left: valty; oper: oper; right: valty}
  | ProjDec of {var: var; val_: valty; idx: int}

and expty = exp * ty

type prog = expty

type def = {var: var; params: var list; body: expty}

let mk_lam var body = Lam {vars= [var]; body}

let mk_lams vars body = Lam {vars; body}

let mk_let (decs : dec list) (body : expty) : expty =
  List.fold_right (fun d e -> (Let {dec= d; body= e}, snd e)) decs body

open Format

let pp_print_id ppf id = fprintf ppf "%s" (Id.unique_name id)

let pp_print_const ppf = function
  | Lambda.Syntax.Int i -> fprintf ppf "%i" i
  | Lambda.Syntax.Bool b -> fprintf ppf "%b" b

let rec pp_print_ty ppf = function
  | IntTy -> fprintf ppf "int"
  | BoolTy -> fprintf ppf "bool"
  | FunTy (tys1, ty2) ->
      fprintf ppf "(%a) -> %a"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_ty)
        tys1 pp_print_ty ty2
  | TupleTy tys ->
      fprintf ppf "(%a)"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_ty)
        tys
  | CodeTy (ty1, tys, ty2) ->
      fprintf ppf "code(%a, %a) -> %a"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_ty)
        tys pp_print_ty ty1 pp_print_ty ty2

let pp_print_var ppf (id, ty) =
  fprintf ppf "%a : %a" pp_print_id id pp_print_ty ty

let rec pp_print_val paren ppf = function
  | Const c -> pp_print_const ppf c
  | Var x -> pp_print_id ppf x
  | Glb f -> fprintf ppf "$%s" (Id.unique_name f)
  | Lam {vars; body} ->
      Utils.with_paren ?b:paren
        (fun ppf ->
          fprintf ppf "@[<1>fun (%a) ->@ %a@]"
            (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_var)
            vars pp_print_expty body )
        ppf
  | Tuple vals ->
      fprintf ppf "(%a)"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
           (pp_print_valty false) )
        vals

and pp_print_val0 ppf : value -> unit = pp_print_val None ppf

and pp_print_valty paren ppf (val_, _) =
  fprintf ppf "%a" (pp_print_val (Some paren)) val_

and pp_print_exp ppf = function
  | Let {dec; body} ->
      fprintf ppf "let %a in@;%a" pp_print_dec dec pp_print_expty body
  | If {cond; then_; else_} ->
      fprintf ppf "if %a then %a@;else %a" (pp_print_valty true) cond
        pp_print_expty then_ pp_print_expty else_
  | Ret val_ -> fprintf ppf "ret %a" (pp_print_valty true) val_

and pp_print_dec ppf : dec -> unit = function
  | ValDec {var; val_} ->
      fprintf ppf "%a = %a" pp_print_var var (pp_print_valty false) val_
  | CallDec {var; fcn; args} ->
      fprintf ppf "%a = %a(%a)" pp_print_var var (pp_print_valty true) fcn
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ", ")
           (pp_print_valty true) )
        args
  | PrimDec {var; left; oper; right} ->
      fprintf ppf "%a = %a %s %a" pp_print_var var (pp_print_valty true) left
        (List.assoc oper
           [ (L.Add, "+"); (L.Sub, "-"); (L.Mul, "*"); (L.Div, "/"); (L.Eq, "=")
           ; (L.Ne, "<>"); (L.Lt, "<"); (L.Le, "<="); (L.Gt, ">"); (L.Ge, ">=")
           ] )
        (pp_print_valty true) right
  | ProjDec {var; val_; idx} ->
      fprintf ppf "%a = %a.%i" pp_print_var var (pp_print_valty true) val_ idx

and pp_print_expty ppf (exp, _) = pp_print_exp ppf exp

let pp_print_prog ppf expty = pp_print_expty ppf expty; print_newline ()

let print_prog expty = pp_print_prog std_formatter expty
