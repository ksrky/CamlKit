type id = string

type exp =
  | VarExp of id
  | NilExp
  | IntExp of int
  | AppExp of {fcn: exp; arg: exp}
  | LamExp of {vars: id list; body: exp}
  | OpExp of {left: exp; oper: oper; right: exp}
  | IfExp of {test: exp; then': exp; else': exp}
  | LetExp of {decs: dec list; body: exp}
  | LetrecExp of {decs: dec list; body: exp}

and dec = {name: id; params: id list; body: exp}

and oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

let pretty_exp exp =
  let prec_of = function
    | VarExp _ -> 3
    | NilExp -> 3
    | IntExp _ -> 3
    | AppExp _ -> 2
    | OpExp _ -> 1
    | LamExp _ -> 0
    | IfExp _ -> 0
    | LetExp _ -> 0
    | LetrecExp _ -> 0
  in
  let rec pretty prec exp =
    let s =
      match exp with
      | VarExp x -> x
      | NilExp -> "nil"
      | IntExp i -> string_of_int i
      | AppExp {fcn; arg} -> pretty 2 fcn ^ " " ^ pretty 1 arg
      | OpExp {left; oper; right} -> pretty 1 left ^ " " ^ pretty_oper oper ^ " " ^ pretty 1 right
      | LamExp {vars; body} ->
          "fun " ^ String.concat " " (List.map (fun x -> x ^ " ") vars) ^ "-> " ^ pretty 0 body
      | IfExp {test; then'; else'} ->
          "if " ^ pretty 0 test ^ " then " ^ pretty 0 then' ^ " else " ^ pretty 0 else'
      | LetExp {decs; body} ->
          "let " ^ String.concat " and " (List.map pretty_dec decs) ^ " in " ^ pretty (-1) body
      | LetrecExp {decs; body} ->
          "let " ^ String.concat " and " (List.map pretty_dec decs) ^ " in " ^ pretty (-1) body
    in
    if prec >= prec_of exp then "(" ^ s ^ ")" else s
  and pretty_dec ({name; params; body} : dec) : string =
    String.concat " " (name :: params) ^ " = " ^ pretty (-1) body
  and pretty_oper : oper -> string = function
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
  pretty (-1) exp
