type id = Id.t

type oper = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Bool of bool

type ty = IntTy | BoolTy | FunTy of ty * ty | TupleTy of ty list

type var = id * ty

type exp =
  | Const of const
  | Var of var
  | App of {fcn: expty; arg: expty}
  | Lam of {var: var; body: expty}
  | Fix of {name: var; var: var; body: expty}
  | Prim of {left: expty; oper: oper; right: expty}
  | If of {cond: expty; then_: expty; else_: expty}
  | Let of {var: var; bnd: exp; body: expty}
  | Tuple of expty list
  | Proj of {tup: expty; idx: int}

and expty = exp * ty

type prog = expty

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

let mk_let (decs : (var * exp) list) (body : expty) : expty =
  List.fold_right
    (fun (var, bnd) body -> (Let {var; bnd; body}, snd body))
    decs body

let rec subst (s : (id * exp) list) (exp, ty) : expty = (subst' s exp, ty)

and subst' (s : (id * exp) list) : exp -> exp = function
  | Const c -> Const c
  | Var (id, ty) -> (
    match List.assoc_opt id s with Some exp -> exp | None -> Var (id, ty) )
  | App {fcn; arg} -> App {fcn= subst s fcn; arg= subst s arg}
  | Lam {var; body} -> Lam {var; body= subst s body}
  | Fix {name; var; body} -> Fix {name; var; body= subst s body}
  | Prim {left; oper; right} ->
      Prim {left= subst s left; oper; right= subst s right}
  | If {cond; then_; else_} ->
      If {cond= subst s cond; then_= subst s then_; else_}
  | Let {var; bnd; body} -> Let {var; bnd= subst' s bnd; body= subst s body}
  | Tuple exps -> Tuple (List.map (subst s) exps)
  | Proj {tup; idx} -> Proj {tup= subst s tup; idx}

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
  | TupleTy tys ->
      fprintf ppf "(%a)"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
           (pp_print_ty 0) )
        tys

let pp_print_ty0 : Format.formatter -> ty -> unit = pp_print_ty 0

let pp_print_var ppf (id, ty) =
  fprintf ppf "%a: %a" Id.pp_print_id id pp_print_ty0 ty

let rec pp_print_exp outer ppf = function
  | Const c -> pp_print_const ppf c
  | Var (id, _) -> Id.pp_print_id ppf id
  | App {fcn; arg} ->
      Utils.with_paren ~b:(outer > 8)
        (fun ppf ->
          fprintf ppf "@[<1>%a %a@]" (pp_print_expty 8) fcn (pp_print_expty 9)
            arg )
        ppf
  | Lam {var; body} ->
      Utils.with_paren ~b:(outer > 0)
        (fun ppf ->
          fprintf ppf "fun %a ->@;<1 2>%a" pp_print_var var (pp_print_expty 0)
            body )
        ppf
  | Fix {name; var; body} ->
      Utils.with_paren ~b:(outer > 0)
        (fun ppf ->
          fprintf ppf "fix %a %a ->@;<1 2>%a" pp_print_var name pp_print_var var
            (pp_print_expty 0) body )
        ppf
  | Prim {left; oper= (Add | Sub) as oper; right} ->
      Utils.with_paren ~b:(outer > 6)
        (fun ppf ->
          fprintf ppf "%a %a %a" (pp_print_expty 6) left pp_print_oper oper
            (pp_print_expty 7) right )
        ppf
  | Prim {left; oper= (Mul | Div) as oper; right} ->
      Utils.with_paren ~b:(outer > 7)
        (fun ppf ->
          fprintf ppf "%a %a %a" (pp_print_expty 7) left pp_print_oper oper
            (pp_print_expty 8) right )
        ppf
  | Prim {left; oper= (Eq | Ne | Lt | Le | Gt | Ge) as oper; right} ->
      Utils.with_paren ~b:(outer > 4)
        (fun ppf ->
          fprintf ppf "%a %a %a" (pp_print_expty 4) left pp_print_oper oper
            (pp_print_expty 4) right )
        ppf
  | If {cond; then_; else_} ->
      Utils.with_paren ~b:(outer > 2)
        (fun ppf ->
          fprintf ppf "if %a@;<1 0>then@;<1 2>@[%a@]@;<1 0>else@;<1 2>@[%a@]@]"
            (pp_print_expty 0) cond (pp_print_expty 0) then_ (pp_print_expty 0)
            else_ )
        ppf
  | Let {var; bnd; body} ->
      fprintf ppf "let@ %a = %a@ in@;%a" pp_print_var var (pp_print_exp 0) bnd
        (pp_print_expty 0) body
  | Tuple exps ->
      fprintf ppf "(%a)"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
           (pp_print_expty 0) )
        exps
  | Proj {tup; idx} -> fprintf ppf "%a.%i" (pp_print_expty 10) tup idx

and pp_print_exp0 ppf : exp -> unit = pp_print_exp 0 ppf

and pp_print_expty outer ppf (exp, _) = pp_print_exp outer ppf exp

let print_prog (exp, _) =
  pp_print_exp 0 std_formatter exp;
  print_newline ()
