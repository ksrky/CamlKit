module K = Syntax
module CC = ClosConv

type value = Const of K.const | Var of K.id | Tuple of value list

and exp =
  | Let of {dec: dec; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: K.id; val_: value}
  | PrimDec of {name: K.id; left: value; oper: K.oper; right: value}
  | ProjDec of {name: K.id; val_: value; idx: int}

and escapes = K.id list

type code = {name: K.id; vars: K.id list; body: exp}

type prog = code list * exp

let code_list : code list ref = ref []

let append_code (code : code) : unit = code_list := code :: !code_list

let rec hoist_val : CC.value -> value = function
  | Const c -> Const c
  | Var x -> Var x
  | Lam {vars; body} ->
      let name = Id.from_string "lamtmp" in
      append_code {name; vars; body= hoist_exp body};
      Var name
  | Tuple vals -> Tuple (List.map hoist_val vals)

and hoist_exp : CC.exp -> exp = function
  | Let {dec; body} -> Let {dec= hoist_dec dec; body= hoist_exp body}
  | Letrec {fundefs; body} ->
      List.iter
        (fun ({name; vars; body; env} : CC.fundef) ->
          append_code {name; vars; body= hoist_exp body} )
        fundefs;
      hoist_exp body
  | App {fcn; args} -> App {fcn= hoist_val fcn; args= List.map hoist_val args}
  | If {cond; then_; else_} ->
      If {cond= hoist_val cond; then_= hoist_exp then_; else_= hoist_exp else_}
  | Halt v -> Halt (hoist_val v)

and hoist_dec : CC.dec -> dec = function
  | ValDec {name; val_} -> ValDec {name; val_= hoist_val val_}
  | PrimDec {name; left; oper; right} ->
      PrimDec {name; left= hoist_val left; oper; right= hoist_val right}
  | ProjDec {name; val_; idx} -> ProjDec {name; val_= hoist_val val_; idx}

let hoist_prog (prog : CC.exp) : prog =
  code_list := [];
  let exp = hoist_exp prog in
  (!code_list, exp)

let parens (outer : int) (prec : int) s =
  if outer > prec then "(" ^ s ^ ")" else s

let rec ppr_val prec : value -> string = function
  | Const c -> Core.Syntax.ppr_const c
  | Var x -> Id.unique_name x
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

and ppr_prog (heaps, exp) =
  let heaps = String.concat "\n" (List.map ppr_fundef heaps) in
  let exp = ppr_exp 0 exp in
  Printf.sprintf "%s\n%s" heaps exp
