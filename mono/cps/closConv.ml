module K = Syntax

type value =
  | Const of K.const
  | Var of K.id
  | Lam of {vars: K.id list; body: exp}
  | Tuple of value list

and exp =
  | Let of {dec: dec; body: exp}
  | Letrec of {fundefs: fundef list; body: exp}
  | App of {fcn: value; args: value list}
  | If of {cond: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: K.id; val_: value}
  | PrimDec of {name: K.id; left: value; oper: K.oper; right: value}
  | ProjDec of {name: K.id; val_: value; idx: int}

(* no function has zero argument because of cps conversion *)
and fundef = {name: K.id; vars: K.id list; body: exp; env: escapes}

and escapes = K.id list

let mk_vars = List.map (fun x -> Var x)

let proj_decs val_ (names : K.id list) : dec list =
  List.mapi (fun i name -> ProjDec {name; val_; idx= i + 1}) names

let mk_let (decs : dec list) (body : exp) : exp =
  List.fold_right (fun d e -> Let {dec= d; body= e}) decs body

let remove_dup xs =
  List.fold_right (fun x xs -> if List.mem x xs then xs else x :: xs) xs []

let lookup_env (escs : escapes) (x : K.id) : int * escapes =
  let rec find_idx i = function
    | [] -> (i, escs @ [x])
    | y :: ys -> if x = y then (i, escs) else find_idx (i + 1) ys
  in
  find_idx 1 escs

let locals : K.id list ref = ref []

let globals : K.id list ref = ref []

let rec cc_val (escs : escapes) : K.value -> value * escapes = function
  | Const c -> (Const c, escs)
  | Var x when List.mem x !locals || List.mem x !globals -> (Var x, escs)
  | Var x ->
      let _, escs' = lookup_env escs x in
      (Var x, escs')
  | Lam {vars; body} ->
      let cvar = Id.from_string "clos" in
      locals := vars;
      let body', escs' = cc_exp [] body in
      ( Tuple
          [ Lam
              { vars= cvar :: vars
              ; body= mk_let (proj_decs (Var cvar) escs') body' }
          ; Tuple (mk_vars escs') ]
      , escs @ escs' |> remove_dup )

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
  | Letrec {fundefs; body} ->
      let names = List.map (fun {K.name} -> name) fundefs in
      let cc_fundef ({name; vars; body} : K.fundef) : fundef * escapes =
        let cvar = Id.from_string "letrec" in
        locals := vars;
        globals := names;
        let body', escs' = cc_exp [] body in
        ( { name
          ; vars= cvar :: vars
          ; body= mk_let (proj_decs (Var cvar) escs') body'
          ; env= escs' }
        , escs' )
      in
      let fundefs', escss = List.split (List.map cc_fundef fundefs) in
      let escs' = List.concat (escs :: escss) |> remove_dup in
      let body', escs'' = cc_exp escs' body in
      (Letrec {fundefs= fundefs'; body= body'}, escs'')
  | App {fcn; args} ->
      let code_var = Id.from_string "code" in
      let env_var = Id.from_string "env" in
      let fcn', escs1 = cc_val escs fcn in
      let args', escs2 = cc_val_seq escs1 args in
      ( mk_let
          (proj_decs fcn' [code_var; env_var])
          (App {fcn= Var code_var; args= Var env_var :: args'})
      , escs2 )
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
      locals := name :: !locals;
      let val', escs' = cc_val escs val_ in
      (ValDec {name; val_= val'}, escs')
  | PrimDec {name; left; oper; right} ->
      let left', escs1 = cc_val escs left in
      let right', escs2 = cc_val escs1 right in
      (PrimDec {name; left= left'; oper; right= right'}, escs2)

let cc_prog (exp : K.exp) : exp = cc_exp [] exp |> fst
