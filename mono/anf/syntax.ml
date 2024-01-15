module L = Lambda.Syntax

type id = Id.t

type oper = L.oper

type const = L.const

type ty =
  | IntTy
  | BoolTy
  | ContTy of ty list
  | TupleTy of ty list
  | ExistsTy of id * ty

type var = id * ty

type value =
  | Const of const
  | Var of id
  | Lam of {vars: var list; body: expty}
  | Call of {fcn: valty; args: valty list}
  | Prim of {left: valty; oper: oper; right: valty}
  | Tuple of valty list
  | Proj of {val_: valty; idx: int}
  | Pack of {ty: ty; val_: valty; exty: ty}

and valty = value * ty

and exp =
  | Let of {var: var; params: var list; bind: expty; body: expty}
  | Unpack of {tyvar: var; var: var; bind: expty; body: expty}
  | If of {cond: valty; then_: expty; else_: expty}
  | Ret of valty

and expty = exp * ty

type prog = exp

let mk_lam var body = Lam {vars= [var]; body}

let mk_lams vars body = Lam {vars; body}

let mk_call fcn arg = Call {fcn; args= [arg]}

let mk_apps fcn args = Call {fcn; args}

let parens (outer : int) (prec : int) s =
  if outer > prec then "(" ^ s ^ ")" else s

open Format

let pp_print_id ppf id = fprintf ppf "%s" (Id.unique_name id)

let pp_print_const ppf = function
  | Lambda.Syntax.Int i -> fprintf ppf "%i" i
  | Lambda.Syntax.Bool b -> fprintf ppf "%b" b

let rec pp_print_ty ppf = function
  | IntTy -> fprintf ppf "int"
  | BoolTy -> fprintf ppf "bool"
  | ContTy tys ->
      fprintf ppf "[%a] -> void"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_ty)
        tys
  | TupleTy tys ->
      fprintf ppf "(%a)"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_ty)
        tys
  | ExistsTy (id, ty) ->
      fprintf ppf "exists %a. %a" pp_print_id id pp_print_ty ty

let pp_print_var ppf (id, ty) =
  fprintf ppf "%a : %a" pp_print_id id pp_print_ty ty

let rec pp_print_val paren ppf = function
  | Const c -> pp_print_const ppf c
  | Var x -> pp_print_id ppf x
  | Lam {vars; body} ->
      Utils.with_paren ?b:paren
        (fun ppf ->
          fprintf ppf "@[<1>fun (%a) ->@ %a@]"
            (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_var)
            vars pp_print_expty body )
        ppf
  | Call {fcn; args} ->
      fprintf ppf "@[<2>%a@ %a@]" (pp_print_valty true) fcn
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ", ")
           (pp_print_valty true) )
        args
  | Prim {left; oper; right} ->
      fprintf ppf "%a %s %a" (pp_print_valty true) left
        (List.assoc oper
           [ (L.Add, "+"); (L.Sub, "-"); (L.Mul, "*"); (L.Div, "/"); (L.Eq, "=")
           ; (L.Ne, "<>"); (L.Lt, "<"); (L.Le, "<="); (L.Gt, ">"); (L.Ge, ">=")
           ] )
        (pp_print_valty true) right
  | Tuple vals ->
      fprintf ppf "(%a)"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
           (pp_print_valty false) )
        vals
  | Pack {ty; val_; exty} ->
      Utils.with_paren ?b:paren
        (fun ppf ->
          fprintf ppf "@[<1>pack [%a, %a] as@ %a@]" pp_print_ty ty
            (pp_print_valty false) val_ pp_print_ty exty )
        ppf
  | Proj {val_; idx} -> fprintf ppf "%a.%i" (pp_print_valty true) val_ idx

and pp_print_val0 ppf : value -> unit = pp_print_val None ppf

and pp_print_valty paren ppf (val_, _) =
  fprintf ppf "%a" (pp_print_val (Some paren)) val_

and pp_print_exp ppf = function
  | Let {var; params; bind; body} ->
      fprintf ppf "@[<2>let %a = %a@]@ in@ %a" pp_print_var var pp_print_expty
        bind pp_print_expty body
  | Unpack {tyvar; var; bind; body} ->
      fprintf ppf "[%a, %a] =@ unpack %a in@ %a" pp_print_var tyvar pp_print_var
        var pp_print_expty bind pp_print_expty body
  | If {cond; then_; else_} ->
      fprintf ppf "@[<v 2>if %a then@ %a@]@;@[<v 2>else@ %a@]"
        (pp_print_valty true) cond pp_print_expty then_ pp_print_expty else_
  | Ret val_ -> fprintf ppf "halt %a" (pp_print_valty true) val_

and pp_print_expty ppf (exp, _) = pp_print_exp ppf exp

let pp_print_prog ppf exp = pp_print_exp ppf exp; print_newline ()

let print_prog exp = pp_print_prog std_formatter exp
