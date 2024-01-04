open Syntax
open Format

let pp_print_tyvar ppf tv = fprintf ppf "'%i" tv.uniq

let rec pp_print_ty outer ppf : ty -> unit = function
  | NilTy -> pp_print_string ppf "nil"
  | IntTy -> pp_print_string ppf "int"
  | BoolTy -> pp_print_string ppf "bool"
  | FunTy (ty1, ty2) ->
      Utils.with_paren ~b:(outer > 1)
        (fun ppf ->
          fprintf ppf "%a ->@ %a" (pp_print_ty 2) ty1 (pp_print_ty 1) ty2 )
        ppf
  | MetaTy tv -> pp_print_tyvar ppf tv

let pp_print_ty0 = pp_print_ty 0

let pp_print_op ppf op =
  pp_print_string ppf
    (List.assoc op
       [ (PlusOp, "+"); (MinusOp, "-"); (TimesOp, "*"); (DivideOp, "/")
       ; (EqOp, "="); (NeqOp, "<>"); (LtOp, "<"); (LeOp, "<="); (GtOp, ">")
       ; (GeOp, ">=") ] )

let rec pp_print_exp ppf : exp -> unit = function
  | VarExp id -> Id.pp_print_id ppf id
  | NilExp -> pp_print_string ppf "nil"
  | BoolExp b -> pp_print_bool ppf b
  | IntExp i -> pp_print_int ppf i
  | AppExp {fcn; arg} ->
      fprintf ppf "(@[<v 2>%a@;%a@])" pp_print_exp fcn pp_print_exp arg
  | OpExp {left; op; right} ->
      fprintf ppf "(@[<v 2>%a@;%a@;%a@])" pp_print_exp left pp_print_op op
        pp_print_exp right
  | LamExp {vars; body} ->
      fprintf ppf "(@[<v 2>fun %a ->@;%a@])"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") Id.pp_print_id)
        vars pp_print_exp body
  | IfExp {cond; then_; else_} ->
      fprintf ppf "(@[<v 2>if %a then@;%a@;else@;%a@])" pp_print_exp cond
        pp_print_exp then_ pp_print_exp else_
  | LetExp {bnds; body} ->
      fprintf ppf "(@[<v 2>let %a in@;%a@])"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf "@;and ")
           pp_print_bnd )
        bnds pp_print_exp body
  | LetrecExp {bnds; body} ->
      fprintf ppf "(@[<v 2>let rec %a in@;%a@])"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf "@;and ")
           pp_print_bnd )
        bnds pp_print_exp body

and pp_print_bnd ppf (Bind {name; params; body}) =
  fprintf ppf "%a %a =@;%a" Id.pp_print_id name
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") Id.pp_print_id)
    params pp_print_exp body

let pp_print_prog = pp_print_exp

let print_prog exp =
  pp_print_prog std_formatter exp;
  print_newline ()

let pp_print_var ppf (id, ty) =
  fprintf ppf "(%a : %a)" Id.pp_print_id id pp_print_ty0 ty

let rec pp_print_aexp ppf : aexp -> unit = function
  | VarAExp var -> pp_print_var ppf var
  | NilAExp -> pp_print_string ppf "nil"
  | BoolAExp b -> pp_print_bool ppf b
  | IntAExp i -> pp_print_int ppf i
  | AppAExp {fcn; arg} ->
      fprintf ppf "(@[<v 2>%a@;%a@])" pp_print_expty fcn pp_print_expty arg
  | OpAExp {left; op; right} ->
      fprintf ppf "(@[<v 2>%a@;%a@;%a@])" pp_print_expty left pp_print_op op
        pp_print_expty right
  | LamAExp {params; body} ->
      fprintf ppf "(@[<v 2>fun %a ->@;%a@])"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_var)
        params pp_print_expty body
  | IfAExp {cond; then_; else_} ->
      fprintf ppf "(@[<v 2>if %a then@;%a@;else@;%a@])" pp_print_expty cond
        pp_print_expty then_ pp_print_expty else_
  | LetAExp {bnds; body} ->
      fprintf ppf "(@[<v 2>let %a in@;%a@])"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf "@;and ")
           pp_print_abnd )
        bnds pp_print_expty body
  | LetrecAExp {bnds; body} ->
      fprintf ppf "(@[<v 2>let rec %a in@;%a@])"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf "@;and ")
           pp_print_abnd )
        bnds pp_print_expty body

and pp_print_expty ppf (exp, _) = pp_print_aexp ppf exp

and pp_print_abnd ppf (ABind {name; params; body}) =
  fprintf ppf "@[<v 2>%a %a =@;%a@]" Id.pp_print_id name
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_print_var)
    params pp_print_expty body

let pp_print_aprog = pp_print_expty

let print_aprog aexp =
  pp_print_aprog std_formatter aexp;
  print_newline ()
