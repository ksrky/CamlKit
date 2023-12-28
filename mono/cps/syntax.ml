module C = Lambda.Syntax

type id = Id.t

type oper = C.oper

type const = C.const

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
  | Glb of id
  | Lam of {vars: var list; body: exp}
  | Tuple of valty list
  | Pack of {ty: ty; val_: valty; exty: ty}

and valty = value * ty

and exp =
  | Let of {dec: dec; body: exp}
  | Letrec of {fundefs: fundef list; body: exp}
  | App of {fcn: valty; args: valty list}
  | If of {cond: valty; then_: exp; else_: exp}
  | Halt of valty

and fundef = {var: var; params: var list; body: exp}

and dec =
  | ValDec of {var: var; val_: valty}
  | PrimDec of {var: var; left: valty; oper: oper; right: valty}
  | ProjDec of {var: var; val_: valty; idx: int}
  | UnpackDec of {tyvar: var; var: var; val_: valty}

type prog = exp

let mk_lam var body = Lam {vars= [var]; body}

let mk_lams vars body = Lam {vars; body}

let mk_app fcn arg = App {fcn; args= [arg]}

let mk_apps fcn args = App {fcn; args}

let mk_let (decs : dec list) (body : exp) : exp =
  List.fold_right (fun d e -> Let {dec= d; body= e}) decs body

let mk_projs val_ (vars : var list) : dec list =
  List.mapi (fun i var -> ProjDec {var; val_; idx= i + 1}) vars

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
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") pp_print_ty)
        tys
  | TupleTy tys ->
      fprintf ppf "(%a)"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") pp_print_ty)
        tys
  | ExistsTy (id, ty) ->
      fprintf ppf "exists %a. %a]" pp_print_id id pp_print_ty ty

let pp_print_var ppf (id, ty) =
  fprintf ppf "%a : %a" pp_print_id id pp_print_ty ty

let rec pp_print_val paren ppf = function
  | Const c -> pp_print_const ppf c
  | Var x -> pp_print_id ppf x
  | Glb f -> fprintf ppf "$%s" (Id.unique_name f)
  | Lam {vars; body} ->
      Utils.with_paren ?b:paren
        (fun ppf ->
          fprintf ppf "@[<1>fun %a ->@ %a@]"
            (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_var)
            vars pp_print_exp body )
        ppf
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

and pp_print_val0 ppf : value -> unit = pp_print_val None ppf

and pp_print_valty paren ppf (val_, _) =
  fprintf ppf "%a" (pp_print_val (Some paren)) val_

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
      fprintf ppf "@[<2>%a@ %a@]" (pp_print_valty true) fcn
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ", ")
           (pp_print_valty true) )
        args
  | If {cond; then_; else_} ->
      fprintf ppf "@[<v 2>if %a then@ %a@]@;@[<v 2>else@ %a@]"
        (pp_print_valty true) cond pp_print_exp then_ pp_print_exp else_
  | Halt val_ -> fprintf ppf "halt %a" (pp_print_valty true) val_

and pp_print_fundef ppf {var; params; body} =
  fprintf ppf "@[<2>%a %a =@ %a@]" pp_print_id (fst var)
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf " ")
       (fun ppf x -> Utils.with_paren (fun ppf -> pp_print_var ppf x) ppf) )
    params pp_print_exp body

and pp_print_dec ppf : dec -> unit = function
  | ValDec {var; val_} ->
      fprintf ppf "%a =@ %a" pp_print_var var (pp_print_valty false) val_
  | PrimDec {var; left; oper; right} ->
      fprintf ppf "%a =@ %a %s %a" pp_print_var var (pp_print_valty true) left
        (List.assoc oper
           [ (C.Add, "+"); (C.Sub, "-"); (C.Mul, "*"); (C.Div, "/"); (C.Eq, "=")
           ; (C.Ne, "<>"); (C.Lt, "<"); (C.Le, "<="); (C.Gt, ">"); (C.Ge, ">=")
           ] )
        (pp_print_valty true) right
  | ProjDec {var; val_; idx} ->
      fprintf ppf "%a =@ %a.%i" pp_print_var var (pp_print_valty true) val_ idx
  | UnpackDec {tyvar; var; val_} ->
      fprintf ppf "[%a, %a] =@ unpack %a" pp_print_var tyvar pp_print_var var
        (pp_print_valty false) val_

let pp_print_prog ppf exp = pp_print_exp ppf exp; print_newline ()

let print_prog exp = pp_print_prog std_formatter exp
