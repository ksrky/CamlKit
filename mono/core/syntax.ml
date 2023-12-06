type id = Id.t

type oper = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Nil

type exp =
  | Const of const
  | Var of id
  | App of {fcn: exp; arg: exp}
  | Lam of {var: id; body: exp}
  | Prim of {left: exp; oper: oper; right: exp}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {isrec: bool; vars: id list; bnds: exp list; body: exp}

let lams (ids : id list) (exp : exp) : exp =
  List.fold_right (fun id exp -> Lam {var= id; body= exp}) ids exp

let unlam : exp -> id list * exp = function
  | Lam {var; body} -> ([var], body)
  | exp -> ([], exp)

let ppr_oper : oper -> string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "="
  | Ne -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

let ppr_const : const -> string = function
  | Int i -> string_of_int i
  | Nil -> "nil"

let ppr_exp (pprid : id -> string) (exp : exp) =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pexp ctx = function
    | Var var -> pprid var
    | Const c -> ppr_const c
    | App {fcn; arg} -> parens ctx 1 (pexp 1 fcn ^ "(" ^ pexp 0 arg ^ ")")
    | Lam {var; body} -> parens ctx 0 ("fun " ^ pprid var ^ " -> " ^ pexp 0 body)
    | Prim {left; oper; right} ->
        parens ctx 1 (pexp 1 left ^ " " ^ ppr_oper oper ^ pexp 1 right)
    | If {cond; then_; else_} ->
        parens ctx 0
          ( "if " ^ pexp 0 cond ^ " then " ^ pexp 0 then_ ^ " else "
          ^ pexp 0 else_ )
    | Let {isrec; vars; bnds; body} ->
        parens ctx 0
          ( "let "
          ^ (if isrec then "rec " else "")
          ^ String.concat "; "
              (List.map2 (fun v e -> pprid v ^ " = " ^ pexp 0 e) vars bnds)
          ^ " in " ^ pexp 0 body )
  in
  pexp 0 exp
