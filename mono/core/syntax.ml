type id = Id.t

type oper = Add | Sub | Mul | Div | Eq | Ne | Lt | Le | Gt | Ge

type const = Int of int | Nil

type exp =
  | Const of const
  | Var of id
  | App of {fcn: exp; args: exp list}
  | Lam of {vars: id list; body: exp}
  | Prim of {oper: oper; args: exp list}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {isrec: bool; vars: id list; bnds: exp list; body: exp}
  | Tuple of exp list
  | Split of {inp: exp; vars: id list; body: exp}

let lams (ids : id list) (exp : exp) : exp =
  List.fold_right (fun id exp -> Lam {vars= [id]; body= exp}) ids exp

let unlam : exp -> id list * exp = function
  | Lam {vars; body} -> (vars, body)
  | exp -> ([], exp)

let ppr_oper : oper -> string = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt -> "lt"
  | Le -> "le"
  | Gt -> "gt"
  | Ge -> "ge"

let ppr_const : const -> string = function
  | Int i -> string_of_int i
  | Nil -> "nil"

let ppr_exp (pprid : id -> string) (exp : exp) =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pexp ctx = function
    | Var var -> pprid var
    | Const c -> ppr_const c
    | App {fcn; args} ->
        parens ctx 1
          (pexp 1 fcn ^ "(" ^ String.concat ", " (List.map (pexp 0) args) ^ ")")
    | Lam {vars; body} ->
        parens ctx 0
          ( "fun "
          ^ String.concat " " (List.map (fun id -> pprid id) vars)
          ^ " -> " ^ pexp 0 body )
    | Prim {oper; args} ->
        ppr_oper oper ^ "(" ^ String.concat ", " (List.map (pexp 0) args) ^ ")"
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
    | Tuple exps -> "(" ^ String.concat ", " (List.map (pexp 0) exps) ^ ")"
    | Split {inp; vars; body} ->
        parens ctx 0
          ( "split " ^ pexp 0 inp ^ " as ("
          ^ String.concat ", " (List.map (fun id -> pprid id) vars)
          ^ ") in " ^ pexp 0 body )
  in
  pexp 0 exp
