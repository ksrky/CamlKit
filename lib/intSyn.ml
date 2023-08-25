type id = Ident.t

type ids = id list

type exps = exp list

and exp =
  | Int of int
  | Nil
  | Var of id
  | App of exp * exps
  | Lam of ids * exp
  | Prim of string * exps
  | Let of bool * ids * exps * exp
  | If of exp * exp * exp
  | Seq of exp * exp
  | Select of exp * exp
  | Store of exp * exp

type def = {name: string; params: ids; body: exp}

type defs = def list

let arith = ["add"; "sub"; "mul"; "div"]

let rel = ["eq"; "ne"; "lt"; "le"; "gt"; "ge"]

let io = ["printi"; "readi"]

let mem = ["load"; "store"; "gep"; "array_alloca"]

let ppr_exp (pprid : id -> string) (exp : exp) =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pretty ctx exp =
    match exp with
    | Var id -> pprid id
    | Nil -> "nil"
    | Int i -> string_of_int i
    | App (fcn, args) ->
        parens ctx 2 (pretty 2 fcn ^ "(" ^ String.concat ", " (List.map (pretty 0) args) ^ ")")
    | Lam (vars, body) ->
        parens ctx 0
          ( "fun "
          ^ String.concat " " (List.map (fun id -> pprid id ^ " ") vars)
          ^ "-> " ^ pretty 0 body )
    | Prim (fcn, args) -> fcn ^ "(" ^ String.concat ", " (List.map (pretty 0) args) ^ ")"
    | If (cond, then_, else_) ->
        parens ctx 0 ("if " ^ pretty 0 cond ^ " then " ^ pretty 0 then_ ^ " else " ^ pretty 0 else_)
    | Let (isrec, vars, exps, body) ->
        parens ctx 0
          ( "let "
          ^ (if isrec then "rec " else "")
          ^ String.concat "; " (List.map2 (fun v e -> pprid v ^ " = " ^ pretty 0 e) vars exps)
          ^ " in " ^ pretty 0 body )
    | Seq (exp, rest) -> "(" ^ pretty 0 exp ^ "; " ^ pretty 0 rest ^ ")"
    | Select (exp, idx) -> pretty 3 exp ^ ".(" ^ pretty 0 idx ^ ")"
    | Store (lhs, rhs) -> pretty 0 lhs ^ "<-" ^ pretty 0 rhs
  in
  pretty 0 exp

let ppr_def {name; params; body} =
  name ^ "("
  ^ String.concat ", " (List.map Ident.unique_name params)
  ^ ") = " ^ ppr_exp Ident.unique_name body

let ppr_defs defs = String.concat "\n" (List.map ppr_def defs)
