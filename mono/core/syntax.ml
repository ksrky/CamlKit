type id = Id.t

and exp =
  | Int of int
  | Nil
  | Var of id
  | App of {fcn: exp; args: exp list}
  | Lam of {vars: id list; body: exp}
  | Prim of {oper: string; args: exp list}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {isrec: bool; vars: id list; bnds: exp list; body: exp}

let arith = ["add"; "sub"; "mul"; "div"]

let rel = ["eq"; "ne"; "lt"; "le"; "gt"; "ge"]

let lams (ids : id list) (exp : exp) : exp =
  List.fold_right (fun id exp -> Lam {vars= [id]; body= exp}) ids exp

let ppr_exp (pprid : id -> string) (exp : exp) =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pexp ctx exp =
    match exp with
    | Var var -> pprid var
    | Nil -> "nil"
    | Int i -> string_of_int i
    | App {fcn; args} ->
        parens ctx 1
          (pexp 1 fcn ^ "(" ^ String.concat ", " (List.map (pexp 0) args) ^ ")")
    | Lam {vars; body} ->
        parens ctx 0
          ( "fun "
          ^ String.concat " " (List.map (fun id -> pprid id) vars)
          ^ " -> " ^ pexp 0 body )
    | Prim {oper; args} ->
        oper ^ "(" ^ String.concat ", " (List.map (pexp 0) args) ^ ")"
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
