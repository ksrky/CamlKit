type id = Id.t

type value = Const of int | Var of id | Glb of id

and exp =
  | Let of {dec: dec; body: exp}
  | App of {fcn: value; args: value list}
  | If of {oper: relop; left: value; right: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: id; val_: value}
  | PrimDec of {name: id; left: value; oper: arithop; right: value}
  | ProjDec of {name: id; val_: value; idx: int}
  | MallocDec of {name: id; len: int}
  | UpdateDec of {name: id; var: id; idx: int; val_: value}

and arithop = Add | Sub | Mul | Div

and relop = Eq | Ne | Lt | Le | Gt | Ge

type heap =
  | Code of {name: id; vars: id list; body: exp}
  | Tuple of {name: id; vals: value list}

type prog = heap list * exp

let mk_let decs body =
  List.fold_right (fun dec body -> Let {dec; body}) decs body

open Format

let pp_print_id ppf id = fprintf ppf "%s" (Id.unique_name id)

let rec pp_print_val ppf : value -> unit = function
  | Const i -> fprintf ppf "%i" i
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
  | ProjDec {name; val_; idx} ->
      fprintf ppf "%a = %a.%i" pp_print_id name pp_print_val val_ idx
  | MallocDec {name; len} -> fprintf ppf "%a = malloc(%i)" pp_print_id name len
  | UpdateDec {name; var; idx; val_} ->
      fprintf ppf "%a = %a.%i <- %a" pp_print_id name pp_print_id var idx
        pp_print_val val_

let pp_print_heap ppf : heap -> unit = function
  | Code {name; vars; body} ->
      fprintf ppf "%a = " pp_print_id name;
      open_box 0;
      fprintf ppf "code(%a).@;<1 2>"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_id)
        vars;
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
