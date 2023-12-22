type id = Id.t

type ty =
  | I1Ty
  | I32Ty
  | PtrTy of ty option
  | FunTy of ty * ty list
  | StrctTy of ty list

type const = I1 of int | I32 of int

type var = id * ty

type value = Const of const | Var of var | Glb of var

and exp =
  | Let of {dec: dec; body: exp}
  | If of {oper: relop; left: value; right: value; then_: exp; else_: exp}
  | Return of value

and dec =
  | ValDec of {var: var; val_: value}
  | PrimDec of {var: var; left: value; oper: arithop; right: value}
  | CallDec of {var: var; fcn: value; args: value list}
  | SubscrDec of {var: var; val_: value; idx: int}
  | MallocDec of {var: var; len: int}
  | UpdateDec of {var: var; strct: value; idx: int; val_: value}

and arithop = Add | Sub | Mul | Div

and relop = Eq | Ne | Lt | Le | Gt | Ge

type heap =
  | Code of {var: var; params: var list; body: exp}
  | Tuple of {name: id; vals: value list}

type prog = heap list * exp

let mk_let decs body =
  List.fold_right (fun dec body -> Let {dec; body}) decs body

let return_type = I32Ty

let deref_type = function
  | PtrTy (Some ty) -> ty
  | PtrTy None -> failwith "actual type is unknown"
  | _ -> failwith "not a pointer"

open Format

let pp_print_id ppf id = fprintf ppf "%s" (Id.unique_name id)

let pp_print_ty ppf : ty -> unit =
  let rec pp_print_ty ppf : ty -> unit = function
    | I1Ty -> fprintf ppf "i1"
    | I32Ty -> fprintf ppf "i32"
    | PtrTy (Some ty) -> fprintf ppf "%a*" pp_print_ty ty
    | PtrTy None -> fprintf ppf "ptr"
    | FunTy (res_ty, arg_tys) ->
        fprintf ppf "%a(%a)" pp_print_ty res_ty
          (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_ty)
          arg_tys
    | StrctTy tys ->
        fprintf ppf "{%a}"
          (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_ty)
          tys
  in
  pp_print_ty ppf

let pp_print_var ppf (id, ty) =
  fprintf ppf "%a : %a" pp_print_id id pp_print_ty ty

let pp_print_const ppf : const -> unit = function
  | I1 i -> fprintf ppf "%i" i
  | I32 i -> fprintf ppf "%i" i

let rec pp_print_val ppf : value -> unit = function
  | Const c -> fprintf ppf "%a" pp_print_const c
  | Var (x, _) -> pp_print_id ppf x
  | Glb (x, _) -> fprintf ppf "$%s" (Id.unique_name x)

and pp_print_exp ppf : exp -> unit = function
  | Let {dec; body} ->
      fprintf ppf "let %a in@ %a" pp_print_dec dec pp_print_exp body
  | If {left; oper; right; then_; else_} ->
      fprintf ppf "@[<v 2>if %a %s %a then@ %a@]@;@[<v 2>else@ %a@]"
        pp_print_val left
        ( match oper with
        | Eq -> "="
        | Ne -> "<>"
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">=" )
        pp_print_val right pp_print_exp then_ pp_print_exp else_
  | Return val_ -> fprintf ppf "ret %a" pp_print_val val_

and pp_print_dec ppf : dec -> unit = function
  | ValDec {var; val_} ->
      fprintf ppf "%a = %a" pp_print_var var pp_print_val val_
  | PrimDec {var; left; oper; right} ->
      fprintf ppf "%a = %a %s %a" pp_print_var var pp_print_val left
        (match oper with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/")
        pp_print_val right
  | CallDec {var; fcn; args} ->
      fprintf ppf "%a = %a(%a)" pp_print_var var pp_print_val fcn
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_val)
        args
  | SubscrDec {var; val_; idx} ->
      fprintf ppf "%a = %a[%i]" pp_print_var var pp_print_val val_ idx
  | MallocDec {var; len} -> fprintf ppf "%a = malloc(%i)" pp_print_var var len
  | UpdateDec {var; strct; idx; val_} ->
      fprintf ppf "%a = %a[%i] <- %a" pp_print_var var pp_print_val strct idx
        pp_print_val val_

let pp_print_heap ppf : heap -> unit = function
  | Code {var; params; body} ->
      fprintf ppf "%a = " pp_print_var var;
      open_box 0;
      fprintf ppf "code(%a).@;<1 2>"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_var)
        params;
      fprintf ppf "@[<v 0>%a@]" pp_print_exp body;
      close_box ()
  | Tuple {name; vals} ->
      fprintf ppf "%a = " pp_print_id name;
      open_box 0;
      fprintf ppf "tuple(%a)"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ",@;<1 1>")
           pp_print_val )
        vals;
      close_box ()

let pp_print_heaps : formatter -> heap list -> unit =
  pp_print_list pp_print_heap

let pp_print_prog ppf (heaps, exp) : unit =
  fprintf ppf "letrec@;<1 2>@[<v 0>%a@]@.in@;<1 2>@[<v 0>%a@]@." pp_print_heaps
    heaps pp_print_exp exp

let print_prog = pp_print_prog std_formatter
