type id = Id.t

type oper = Core.Syntax.oper

type const = Core.Syntax.const

type exp =
  | Const of const
  | Var of id
  | App of {fcn: exp; args: exp list}
  | Prim of {oper: oper; args: exp list}
  | If of {cond: exp; then_: exp; else_: exp}
  | Let of {isrec: bool; vars: id list; bnds: exp list; body: exp}

type frag = {name: string; params: id list; body: exp}

type frags = frag list

let ppr_oper = Core.Syntax.ppr_oper

let ppr_const = Core.Syntax.ppr_const

let ppr_exp (pprid : id -> string) (exp : exp) =
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
    | Let {isrec; vars; bnds; body} ->
        parens ctx 0
          ( "let "
          ^ (if isrec then "rec " else "")
          ^ String.concat "; "
              (List.map2 (fun v e -> pprid v ^ " = " ^ pexp 0 e) vars bnds)
          ^ " in " ^ pexp 0 body )
  in
  pexp 0 exp

let ppr_frag {name; params; body} =
  name ^ "("
  ^ String.concat ", " (List.map Id.unique_name params)
  ^ ") = "
  ^ ppr_exp Id.unique_name body

let ppr_frags frags = String.concat "\n" (List.map ppr_frag frags)
