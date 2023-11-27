type id = Id.t

type oper = Core.Syntax.oper

type const = Core.Syntax.const

type exp =
  | Const of const
  | Var of id
  | App of {fcn: exp; args: exp list}
  | Prim of {oper: oper; args: exp list}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {decs: dec list; body: exp}

and dec =
  | ValDec of {var: id; exp: exp}
  | ClosDec of {var: id; env: id list; code: exp}

type code = {name: string; params: id list; body: exp}

type codes = code list

let ppr_oper = Core.Syntax.ppr_oper

let ppr_const = Core.Syntax.ppr_const

let rec ppr_exp (pprid : id -> string) (exp : exp) =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pexp ctx exp =
    match exp with
    | Var var -> pprid var
    | Const c -> ppr_const c
    | App {fcn; args} ->
        parens ctx 1
          (pexp 1 fcn ^ "(" ^ String.concat ", " (List.map (pexp 0) args) ^ ")")
    | Prim {oper; args} ->
        ppr_oper oper ^ "(" ^ String.concat ", " (List.map (pexp 0) args) ^ ")"
    | If {cond; then_; else_} ->
        parens ctx 0
          ( "if " ^ pexp 0 cond ^ " then " ^ pexp 0 then_ ^ " else "
          ^ pexp 0 else_ )
    | Let {decs; body} ->
        parens ctx 0
          ( "let "
          ^ String.concat "; " (List.map (ppr_dec pprid) decs)
          ^ " in " ^ pexp 0 body )
  in
  pexp 0 exp

and ppr_dec (pprid : id -> string) : dec -> string = function
  | ValDec {var; exp} -> pprid var ^ " = " ^ ppr_exp pprid exp
  | ClosDec {var; env; code} ->
      pprid var ^ "=" ^ "<{"
      ^ String.concat ", " (List.map pprid env)
      ^ "}, " ^ ppr_exp pprid code ^ ">"

let ppr_code {name; params; body} =
  name ^ "("
  ^ String.concat ", " (List.map Id.unique_name params)
  ^ ") = "
  ^ ppr_exp Id.unique_name body

let ppr_codes (codes : codes) = String.concat "\n" (List.map ppr_code codes)
