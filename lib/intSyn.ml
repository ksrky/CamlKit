type id = Ident.t

type ids = id list

type exps = exp list

and exp =
  | Int of int
  | Nil
  | Var of id
  | App of exp * exps
  | Lam of ids * exp
  | Builtin of string * exps
  | Let of ids * exps * exp
  | Letrec of ids * exps * exp
  | If of exp * exp * exp

type def = {name: id; params: ids; body: exp}

type defs = def list

let arith = ["ADD"; "SUB"; "MUL"; "DIV"]

let rel = ["EQ"; "NE"; "LT"; "LE"; "GT"; "GE"]

let io = ["WRITEC"; "READC"]

let ppr_exp exp =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pretty ctx exp =
    match exp with
    | Var id -> Ident.to_string id
    | Nil -> "nil"
    | Int i -> string_of_int i
    | App (fcn, args) ->
        parens ctx 2 (pretty 2 fcn ^ "(" ^ String.concat ", " (List.map (pretty 0) args) ^ ")")
    | Lam (vars, body) ->
        parens ctx 0
          ( "fun "
          ^ String.concat " " (List.map (fun id -> Ident.to_string id ^ " ") vars)
          ^ "-> " ^ pretty 0 body )
    | Builtin (fcn, args) -> fcn ^ "(" ^ String.concat ", " (List.map (pretty 0) args) ^ ")"
    | If (cond, then_, else_) ->
        parens ctx 0 ("if " ^ pretty 0 cond ^ " then " ^ pretty 0 then_ ^ " else " ^ pretty 0 else_)
    | Let (vars, exps, body) ->
        parens ctx 0
          ( "let "
          ^ String.concat "; "
              (List.map2 (fun v e -> Ident.to_string v ^ " = " ^ pretty 0 e) vars exps)
          ^ " in " ^ pretty 0 body )
    | Letrec (vars, exps, body) ->
        parens ctx 0
          ( "let rec "
          ^ String.concat "; "
              (List.map2 (fun v e -> Ident.to_string v ^ " = " ^ pretty 0 e) vars exps)
          ^ " in " ^ pretty 0 body )
  in
  pretty 0 exp

let ppr_def {name; params; body} =
  Ident.to_string name ^ "("
  ^ String.concat ", " (List.map Ident.to_string params)
  ^ ") = " ^ ppr_exp body

let ppr_defs defs = String.concat "\n" (List.map ppr_def defs)
