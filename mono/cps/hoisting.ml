module K = Syntax
module CC = ClosConv

type value =
  | Const of K.const
  | Var of K.id
  | Clos of clos
  | Select of {clos: clos; idx: int}
  | Pair of {fst: value; snd: value}

and exp =
  | Let of {dec: dec; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: K.id; val_: value}
  | PrimDec of {name: K.id; oper: K.oper; args: value list}

and clos = CVar of K.id | CPair of {code: K.id; env: escapes}

and escapes = K.id list

type code = {name: K.id; cvar: K.id; vars: K.id list; body: exp}

type prog = code list * exp

let code_list : code list ref = ref []

let append_code (code : code) : unit = code_list := code :: !code_list

let rec hoist_val : CC.value -> value = function
  | Const c -> Const c
  | Var x -> Var x
  | Clos clos -> Clos (hoist_clos clos)
  | Select {clos; idx} -> Select {clos= hoist_clos clos; idx}

and hoist_exp : CC.exp -> exp = function
  | Let {dec; body} -> Let {dec= hoist_dec dec; body= hoist_exp body}
  | Letrec {fundefs; body} ->
      List.iter
        (fun ({name; cvar; vars; body; env} : CC.fundef) ->
          append_code {name; cvar; vars; body= hoist_exp body} )
        fundefs;
      hoist_exp body
  | ClosApp {fcn; args} ->
      App {fcn= hoist_val fcn; args= List.map hoist_val args}
  | If {cond; then_; else_} ->
      If {cond= hoist_val cond; then_= hoist_exp then_; else_= hoist_exp else_}
  | Halt v -> Halt (hoist_val v)

and hoist_dec : CC.dec -> dec = function
  | ValDec {name; val_} -> ValDec {name; val_= hoist_val val_}
  | PrimDec {name; oper; args} ->
      PrimDec {name; oper; args= List.map hoist_val args}

and hoist_clos : CC.clos -> clos = function
  | CVar x -> CVar x
  | CLam {cvar; vars; body; env} ->
      let name = Id.from_string "clam" in
      append_code {name; cvar; vars; body= hoist_exp body};
      CPair {code= name; env}
(* | CFix fundefs ->
     let cc =
       List.map
         (fun ({name; cvar; vars; body; env} : CC.fundef) ->
           append_code {name; cvar; vars; body= hoist_exp body};
           CPair {code= name; env} )
         fundefs
     in
     failwith ""*)
