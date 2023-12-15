type id = Id.t

type ty =
  | I1Ty
  | I32Ty
  | PtrTy of ty
  | FunTy of ty * ty list
  | StrctTy of ty list

type const = I1 of int | I32 of int

type var = id * ty

type value = Const of const | Var of id | Glb of id

and exp =
  | Let of {dec: dec; body: exp}
  | App of {fcn: value; args: value list}
  | If of {oper: relop; left: value; right: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: id; val_: value}
  | PrimDec of {name: id; left: value; oper: arithop; right: value}
  | SubscrDec of {name: id; val_: value; idx: int}
  | MallocDec of {name: id; len: int}
  | UpdateDec of {name: id; var: id; idx: int; val_: value}

and arithop = Add | Sub | Mul | Div

and relop = Eq | Ne | Lt | Le | Gt | Ge

type heap =
  | Code of {name: id; params: var list; ret_ty: ty; body: exp}
  | Tuple of {name: id; vals: value list}

type prog = heap list * exp

let mk_let decs body =
  List.fold_right (fun dec body -> Let {dec; body}) decs body

let return_ty : ty -> ty = function
  | FunTy (ty, _) -> ty
  | _ -> failwith "not a function type"

open Format

let pp_print_id ppf id = fprintf ppf "%s" (Id.unique_name id)

let pp_print_ty ppf : ty -> unit =
  let rec pp_print_ty ppf : ty -> unit = function
    | I1Ty -> fprintf ppf "i1"
    | I32Ty -> fprintf ppf "i32"
    | PtrTy ty -> fprintf ppf "%a*" pp_print_ty ty
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
  fprintf ppf "%a: %a" pp_print_id id pp_print_ty ty

let pp_print_const ppf : const -> unit = function
  | I1 i -> fprintf ppf "%i" i
  | I32 i -> fprintf ppf "%i" i

let rec pp_print_val ppf : value -> unit = function
  | Const c -> fprintf ppf "%a" pp_print_const c
  | Var x -> pp_print_id ppf x
  | Glb x -> fprintf ppf "$%s" (Id.unique_name x)

and pp_print_exp ppf : exp -> unit = function
  | Let {dec; body} ->
      fprintf ppf "let %a in@ %a" pp_print_dec dec pp_print_exp body
  | App {fcn; args} ->
      fprintf ppf "%a(%a)" pp_print_val fcn
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_val)
        args
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
  | Halt val_ -> fprintf ppf "halt %a" pp_print_val val_

and pp_print_dec ppf : dec -> unit = function
  | ValDec {name; val_} ->
      fprintf ppf "%a = %a" pp_print_id name pp_print_val val_
  | PrimDec {name; left; oper; right} ->
      fprintf ppf "%a = %a %s %a" pp_print_id name pp_print_val left
        (match oper with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/")
        pp_print_val right
  | SubscrDec {name; val_; idx} ->
      fprintf ppf "%a = %a[%i]" pp_print_id name pp_print_val val_ idx
  | MallocDec {name; len} -> fprintf ppf "%a = malloc(%i)" pp_print_id name len
  | UpdateDec {name; var; idx; val_} ->
      fprintf ppf "%a = %a[%i] <- %a" pp_print_id name pp_print_id var idx
        pp_print_val val_

let pp_print_heap ppf : heap -> unit = function
  | Code {name; params; ret_ty; body} ->
      fprintf ppf "%a = " pp_print_id name;
      open_box 0;
      fprintf ppf "code(%a): %a.@;<1 2>"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_var)
        params pp_print_ty ret_ty;
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
