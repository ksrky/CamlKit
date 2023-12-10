type id = Id.t

type oper = Core.Syntax.oper

type const = Core.Syntax.const

type value =
  | Const of const
  | Var of id
  | Glb of id
  | Lam of {vars: id list; body: exp}
  | Tuple of value list

and fundef = {name: id; vars: id list; body: exp}

and exp =
  | Let of {dec: dec; body: exp}
  | Letrec of {fundefs: fundef list; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: id; val_: value}
  | PrimDec of {name: id; left: value; oper: oper; right: value}
  | ProjDec of {name: id; val_: value; idx: int}

type prog = exp

let mk_vars xs = List.map (fun x -> Var x) xs

let mk_lam var body = Lam {vars= [var]; body}

let mk_lams vars body = Lam {vars; body}

let mk_app fcn arg = App {fcn; args= [arg]}

let mk_apps fcn args = App {fcn; args}

let mk_let (decs : dec list) (body : exp) : exp =
  List.fold_right (fun d e -> Let {dec= d; body= e}) decs body

let mk_projs val_ (names : id list) : dec list =
  List.mapi (fun i name -> ProjDec {name; val_; idx= i + 1}) names

let parens (outer : int) (prec : int) s =
  if outer > prec then "(" ^ s ^ ")" else s

let rec ppr_val prec : value -> string = function
  | Const c -> Core.Syntax.ppr_const c
  | Var x -> Id.unique_name x
  | Glb x -> Id.unique_name x
  | Lam {vars; body} ->
      let vars = String.concat " " (List.map Id.unique_name vars) in
      let body = ppr_exp 0 body in
      parens prec 0 (Printf.sprintf "fun %s -> %s" vars body)
  | Tuple vals ->
      let vals = String.concat ", " (List.map (ppr_val 0) vals) in
      parens prec 0 (Printf.sprintf "(%s)" vals)

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
  | Halt val_ -> parens prec 0 (Printf.sprintf "halt %s" (ppr_val 0 val_))

and ppr_dec : dec -> string = function
  | ValDec {name; val_} ->
      let name = Id.unique_name name in
      let value = ppr_val 0 val_ in
      Printf.sprintf "%s = %s" name value
  | PrimDec {name; left; oper; right} ->
      let name = Id.unique_name name in
      let oper = Core.Syntax.ppr_oper oper in
      let left = ppr_val 0 left in
      let right = ppr_val 0 right in
      Printf.sprintf "%s = %s %s %s" name left oper right
  | ProjDec {name; val_; idx} ->
      let name = Id.unique_name name in
      let val_ = ppr_val 0 val_ in
      Printf.sprintf "%s = #%d %s" name idx val_

and ppr_prog exp = ppr_exp 0 exp
