type id = Id.t

type exp =
  | VarExp of id
  | NilExp
  | BoolExp of bool
  | IntExp of int
  | AppExp of {fcn: exp; arg: exp}
  | LamExp of {vars: id list; body: exp}
  | OpExp of {left: exp; op: op; right: exp}
  | IfExp of {cond: exp; then_: exp; else_: exp}
  | LetExp of {bnds: bnd list; body: exp}
  | LetrecExp of {bnds: bnd list; body: exp}

and bnd = {name: id; params: id list; body: exp}

and op = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

let rec ppr_exp exp =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pretty ctx = function
    | VarExp id -> Id.name id
    | NilExp -> "nil"
    | BoolExp b -> string_of_bool b
    | IntExp i -> string_of_int i
    | AppExp {fcn; arg} -> parens ctx 2 (pretty 2 fcn ^ " " ^ pretty 1 arg)
    | OpExp {left; op; right} ->
        parens ctx 1 (pretty 1 left ^ " " ^ ppr_oper op ^ " " ^ pretty 1 right)
    | LamExp {vars; body} ->
        parens ctx 0 ("fun " ^ String.concat " " (List.map Id.name vars) ^ " -> " ^ pretty 0 body)
    | IfExp {cond; then_; else_} ->
        parens ctx 0 ("if " ^ pretty 0 cond ^ " then " ^ pretty 0 then_ ^ " else " ^ pretty 0 else_)
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

and ppr_bnd ({name; params; body} : bnd) : string =
  String.concat " " (List.map Id.name (name :: params)) ^ " = " ^ ppr_exp body
