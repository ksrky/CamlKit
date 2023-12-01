module K = Syntax

type value =
  | Const of K.const
  | Var of K.id
  | Tuple of value list
  | Clos of clos
  | Select of {clos: clos; idx: int}

and exp =
  | Let of {dec: dec; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Split of {inp: value; vars: K.id list; body: exp}
  | Halt of value

and dec =
  | ValDec of {name: K.id; val_: value}
  | PrimDec of {name: K.id; oper: K.oper; args: value list}

and clos =
  | CVar of K.id
  | CLam of {cvar: K.id; vars: K.id list; body: exp; env: escapes}
  | CFix of fundef list

and fundef = {name: K.id; cvar: K.id; vars: K.id list; body: exp; env: escapes}

and escapes = K.id list

let remove_dup xs =
  List.fold_right (fun x xs -> if List.mem x xs then xs else x :: xs) xs []

let lookup_env (escs : escapes) (x : K.id) : int * escapes =
  let rec find_idx i = function
    | [] -> (i, escs @ [x])
    | y :: ys -> if x = y then (i, escs) else find_idx (i + 1) ys
  in
  find_idx 0 escs

let locals : K.id list ref = ref []

let cvar : K.id ref = ref (Id.fresh ())

let rec cc_val (escs : escapes) : K.value -> value * escapes = function
  | Const c -> (Const c, escs)
  | Var x when List.mem x !locals -> (Var x, escs)
  | Var x ->
      let idx, env' = lookup_env escs x in
      (Select {clos= CVar !cvar; idx}, env')
  | Lam {vars; body} ->
      let cvar' = Id.from_string "clos" in
      cvar := cvar';
      locals := vars;
      let body', escs' = cc_exp [] body in
      ( Clos (CLam {cvar= cvar'; vars; body= body'; env= escs'})
      , escs @ escs' |> remove_dup )
  | Fix fundefs ->
      let names = List.map (fun {K.name} -> name) fundefs in
      let cc_fundef ({name; vars; body} : K.fundef) : fundef * escapes =
        let cvar' = Id.from_string "clos" in
        cvar := cvar';
        locals := names @ vars;
        let body', escs' = cc_exp [] body in
        ({name; cvar= cvar'; vars; body= body'; env= escs'}, escs')
      in
      let fundefs', escss = List.split (List.map cc_fundef fundefs) in
      (Clos (CFix fundefs'), List.concat (escs :: escss) |> remove_dup)

and cc_val_seq (escs : escapes) (vals : K.value list) : value list * escapes =
  let loop (acc : value list) (escs : escapes) :
      K.value list -> value list * escapes = function
    | [] -> (List.rev acc, escs)
    | val_ :: vals ->
        let val', escs' = cc_val escs val_ in
        (val' :: acc, escs')
  in
  loop [] escs vals

and cc_exp (escs : escapes) : K.exp -> exp * escapes = function
  | Let {dec; body} ->
      let dec', escs1 = cc_dec escs dec in
      let body', escs2 = cc_exp escs1 body in
      (Let {dec= dec'; body= body'}, escs2)
  | App {fcn; args} ->
      let fcn', escs1 = cc_val escs fcn in
      let args', escs2 = cc_val_seq escs1 args in
      (App {fcn= fcn'; args= args'}, escs2)
  | If {cond; then_; else_} ->
      let cond', escs1 = cc_val escs cond in
      let then', escs2 = cc_exp escs1 then_ in
      let else', escs3 = cc_exp escs2 else_ in
      (If {cond= cond'; then_= then'; else_= else'}, escs3)
  | Halt val_ ->
      let val', escs' = cc_val escs val_ in
      (Halt val', escs')

and cc_dec (escs : escapes) : K.dec -> dec * escapes = function
  | ValDec {name; val_} ->
      let val', escs' = cc_val escs val_ in
      (ValDec {name; val_= val'}, escs')
  | PrimDec {name; oper; args} ->
      let args', escs' = cc_val_seq escs args in
      (PrimDec {name; oper; args= args'}, escs')

let cc_prog (exp : K.exp) : exp = cc_exp [] exp |> fst
