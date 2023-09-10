type id = Ident.t

type exp =
  | VarExp of id
  | NilExp
  | BoolExp of bool
  | IntExp of int
  | AppExp of {fcn: exp; arg: exp}
  | LamExp of {vars: id list; body: exp}
  | OpExp of {left: exp; op: op; right: exp}
  | IfExp of {test: exp; then_: exp; else_: exp}
  | LetExp of {bnds: bnd list; body: exp}
  | LetrecExp of {bnds: bnd list; body: exp}
  | SubscExp of {arr: exp; idx: exp}
  | AssignExp of {arr: exp; idx: exp; rhs: exp}
  | SeqExp of exp list

and bnd = {name: id; params: id list; body: exp}

and op = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

type ty = NIL | TyconTy of {con: id; args: ty list} | FunTy of ty * ty | MetaTy of tyvar

and tyvar = {uniq: int; mutable repres: ty option}

let rec ppr_exp exp =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pretty ctx = function
    | VarExp id -> Ident.name id
    | NilExp -> "nil"
    | BoolExp b -> string_of_bool b
    | IntExp i -> string_of_int i
    | AppExp {fcn; arg} -> parens ctx 2 (pretty 2 fcn ^ " " ^ pretty 1 arg)
    | OpExp {left; op; right} ->
        parens ctx 1 (pretty 1 left ^ " " ^ ppr_oper op ^ " " ^ pretty 1 right)
    | LamExp {vars; body} ->
        parens ctx 0 ("fun " ^ String.concat " " (List.map Ident.name vars) ^ " -> " ^ pretty 0 body)
    | IfExp {test; then_; else_} ->
        parens ctx 0 ("if " ^ pretty 0 test ^ " then " ^ pretty 0 then_ ^ " else " ^ pretty 0 else_)
    | LetExp {bnds; body} ->
        parens ctx 0
          ("let " ^ String.concat " and " (List.map ppr_bnd bnds) ^ " in " ^ pretty 0 body)
    | LetrecExp {bnds; body} ->
        parens ctx 0
          ("let " ^ String.concat " and " (List.map ppr_bnd bnds) ^ " in " ^ pretty 0 body)
  and ppr_oper : op -> string = function
    | PlusOp -> "+"
    | MinusOp -> "-"
    | TimesOp -> "*"
    | DivideOp -> "/"
    | EqOp -> "="
    | NeqOp -> "<>"
    | LtOp -> "<"
    | LeOp -> "<="
    | GtOp -> ">"
    | GeOp -> ">="
  in
  pretty 0 exp

and ppr_ty (ty : ty) : string =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pretty ctx = function
    | NIL -> "nil"
    | TyconTy {con; args} ->
        if args = [] then Ident.name con
        else parens ctx 1 (String.concat " " (Ident.name con :: List.map (pretty 2) args))
    | FunTy (fcn, arg) -> parens ctx 0 (pretty 1 fcn ^ " -> " ^ pretty 0 arg)
    | MetaTy tv -> "$" ^ string_of_int tv.uniq
  in
  pretty 0 ty

and ppr_bnd ({name; params; body} : bnd) : string =
  String.concat " " (List.map Ident.name (name :: params)) ^ " = " ^ ppr_exp body
