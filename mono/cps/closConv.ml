module K = Syntax

type value =
  | Const of K.const
  | Var of K.id
  | Glb of K.id
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

type locals = K.id list

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

let globals : K.id list ref = ref []

let rec cc_val (escs : escapes) (lcls : locals) : K.value -> value * escapes =
  function
  | Const c -> (Const c, escs)
  | Var x when List.mem x lcls -> (Var x, escs)
  | Var f when List.mem f !globals -> (Glb f, escs)
  | Var x ->
      let _, escs' = lookup_env escs x in
      (Var x, escs')
  | Lam {vars; body} ->
      let cvar = Id.from_string "clos" in
      let body', escs' = cc_exp [] vars body in
      if escs' = [] then
        (Lam {vars; body= mk_let (proj_decs (Var cvar) escs') body'}, escs)
      else
        ( Tuple
            [ Lam
                { vars= cvar :: vars
                ; body= mk_let (proj_decs (Var cvar) escs') body' }
            ; Tuple (mk_vars escs') ]
        , escs @ escs' |> remove_dup )

and cc_val_seq (escs : escapes) (lcls : locals) (vals : K.value list) :
    value list * escapes =
  let loop (acc : value list) (escs : escapes) :
      K.value list -> value list * escapes = function
    | [] -> (List.rev acc, escs)
    | val_ :: vals ->
        let val', escs' = cc_val escs lcls val_ in
        (val' :: acc, escs')
  in
  loop [] escs vals

and cc_exp (escs : escapes) (lcls : locals) : K.exp -> exp * escapes = function
  | Let {dec; body} ->
      let dec', escs1, name = cc_dec escs lcls dec in
      let body', escs2 = cc_exp escs1 (name :: lcls) body in
      (Let {dec= dec'; body= body'}, escs2)
  | Letrec {fundefs; body} ->
      let names = List.map (fun {K.name} -> name) fundefs in
      let cc_fundef ({name; vars; body} : K.fundef) : fundef * escapes =
        let cvar = Id.from_string "letrec" in
        globals := names;
        let body', escs' = cc_exp [] vars body in
        ( { name
          ; vars= cvar :: vars
          ; body= mk_let (proj_decs (Var cvar) escs') body'
          ; env= escs' }
        , escs' )
      in
      let fundefs', escss = List.split (List.map cc_fundef fundefs) in
      let escs' = List.concat (escs :: escss) |> remove_dup in
      let body', escs'' = cc_exp escs' lcls body in
      (Letrec {fundefs= fundefs'; body= body'}, escs'')
  | App {fcn; args} -> (
      let fcn', escs1 = cc_val escs lcls fcn in
      let args', escs2 = cc_val_seq escs1 lcls args in
      match fcn' with
      | Lam _ -> (App {fcn= fcn'; args= args'}, escs2)
      | _ ->
          let clos_var = Id.from_string "clos" in
          let code_var = Id.from_string "code" in
          let env_var = Id.from_string "env" in
          ( mk_let
              ( ValDec {name= clos_var; val_= fcn'}
              :: proj_decs (Var clos_var) [code_var; env_var] )
              (App {fcn= Var code_var; args= Var env_var :: args'})
          , escs2 ) )
  | If {cond; then_; else_} ->
      let cond', escs1 = cc_val escs lcls cond in
      let then', escs2 = cc_exp escs1 lcls then_ in
      let else', escs3 = cc_exp escs2 lcls else_ in
      (If {cond= cond'; then_= then'; else_= else'}, escs3)
  | Halt val_ ->
      let val', escs' = cc_val escs lcls val_ in
      (Halt val', escs')

and cc_dec (escs : escapes) (lcls : locals) : K.dec -> dec * escapes * K.id =
  function
  | ValDec {name; val_} ->
      let val', escs' = cc_val escs lcls val_ in
      (ValDec {name; val_= val'}, escs', name)
  | PrimDec {name; left; oper; right} ->
      let left', escs1 = cc_val escs lcls left in
      let right', escs2 = cc_val escs1 lcls right in
      (PrimDec {name; left= left'; oper; right= right'}, escs2, name)

let cc_prog (exp : K.exp) : exp = cc_exp [] [] exp |> fst

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
