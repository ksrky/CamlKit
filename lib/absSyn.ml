type id = Ident.t

type exp =
  | VarExp of id
  | NilExp
  | IntExp of int
  | AppExp of {fcn: exp; arg: exp}
  | LamExp of {vars: id list; body: exp}
  | OpExp of {left: exp; oper: oper; right: exp}
  | IfExp of {test: exp; then_: exp; else_: exp}
  | LetExp of {decs: dec list; body: exp}
  | LetrecExp of {decs: dec list; body: exp}

and dec = {name: id; params: id list; body: exp}

and oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

let ppr_exp exp =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pretty ctx exp =
    match exp with
    | VarExp id -> Ident.name id
    | NilExp -> "nil"
    | IntExp i -> string_of_int i
    | AppExp {fcn; arg} -> parens ctx 2 (pretty 2 fcn ^ " " ^ pretty 1 arg)
    | OpExp {left; oper; right} ->
        parens ctx 1 (pretty 1 left ^ " " ^ ppr_oper oper ^ " " ^ pretty 1 right)
    | LamExp {vars; body} ->
        parens ctx 0 ("fun " ^ String.concat " " (List.map Ident.name vars) ^ " -> " ^ pretty 0 body)
    | IfExp {test; then_; else_} ->
        parens ctx 0 ("if " ^ pretty 0 test ^ " then " ^ pretty 0 then_ ^ " else " ^ pretty 0 else_)
    | LetExp {decs; body} ->
        parens ctx 0
          ("let " ^ String.concat " and " (List.map ppr_dec decs) ^ " in " ^ pretty 0 body)
    | LetrecExp {decs; body} ->
        parens ctx 0
          ("let " ^ String.concat " and " (List.map ppr_dec decs) ^ " in " ^ pretty 0 body)
  and ppr_dec ({name; params; body} : dec) : string =
    String.concat " " (List.map Ident.name (name :: params)) ^ " = " ^ pretty 0 body
  and ppr_oper : oper -> string = function
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
