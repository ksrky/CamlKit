type id = Id.t

type oper = Core.Syntax.oper

type const = Core.Syntax.const

type value = Const of const | Var of id | Lam of {vars: id list; body: exp}

and fundef = {name: id; vars: id list; body: exp}

and exp =
  | Let of {dec: dec; body: exp}
  | Letrec of {fundefs: fundef list; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: id; val_: value}
  | PrimDec of {name: id; oper: oper; args: value list}

let lam var body = Lam {vars= [var]; body}

let lams vars body = Lam {vars; body}

let app fcn arg = App {fcn; args= [arg]}

let apps fcn args = App {fcn; args}

let parens (outer : int) (prec : int) s =
  if outer > prec then "(" ^ s ^ ")" else s

let rec ppr_val prec : value -> string = function
  | Const c -> Core.Syntax.ppr_const c
  | Var x -> Id.unique_name x
  | Lam {vars; body} ->
      let vars = String.concat " " (List.map Id.unique_name vars) in
      let body = ppr_exp 0 body in
      parens prec 0 (Printf.sprintf "fun %s -> %s" vars body)

and ppr_fundef {name; vars; body} =
  let vars = String.concat " " (List.map Id.unique_name vars) in
  let body = ppr_exp 0 body in
  if vars = "" then Printf.sprintf "%s = %s" (Id.unique_name name) body
  else Printf.sprintf "%s %s = %s" (Id.unique_name name) vars body

and ppr_exp prec : exp -> string = function
  | Let {dec; body} ->
      let dec = ppr_dec dec in
      let body = ppr_exp 0 body in
      parens prec 0 (Printf.sprintf "let %s in %s" dec body)
  | Letrec {fundefs; body} ->
      let fundefs = String.concat " and " (List.map ppr_fundef fundefs) in
      parens prec 0 (Printf.sprintf "let rec %s" fundefs)
  | App {fcn; args} ->
      let fcn = ppr_val 1 fcn in
      let args = String.concat " " (List.map (ppr_val 2) args) in
      parens prec 1 (Printf.sprintf "%s %s" fcn args)
  | If {cond; then_; else_} ->
      let cond = ppr_val 0 cond in
      let then_ = ppr_exp 0 then_ in
      let else_ = ppr_exp 0 else_ in
      parens prec 0 (Printf.sprintf "if %s then %s else %s" cond then_ else_)
  | Halt v -> parens prec 0 (Printf.sprintf "halt %s" (ppr_val 0 v))

and ppr_dec : dec -> string = function
  | ValDec {name; val_} ->
      let name = Id.unique_name name in
      let value = ppr_val 0 val_ in
      Printf.sprintf "%s = %s" name value
  | PrimDec {name; oper; args} ->
      let name = Id.unique_name name in
      let oper = Core.Syntax.ppr_oper oper in
      let args = String.concat " " (List.map (ppr_val 0) args) in
      Printf.sprintf "%s = %s %s" name oper args

and ppr_prog exp = ppr_exp 0 exp
