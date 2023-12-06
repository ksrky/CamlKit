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
