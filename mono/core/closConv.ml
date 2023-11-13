open Syntax

let notin xs ys = List.filter (fun y -> not (List.mem y ys)) xs

let rec conv_exp (sc : id list) : exp -> id list * exp = function
  | Const c -> ([], Const c)
  | Var x when List.mem x sc -> ([], Var x)
  | Var x -> ([x], Var x)
  | App {fcn; args} ->
      let fvs, fcn' = conv_exp sc fcn in
      let code_var = Id.from_string "code" and env_var = Id.from_string "env" in
      let fvss, args' = List.split (List.map (conv_exp sc) args) in
      ( fvs @ List.concat fvss
      , Split
          { inp= fcn'
          ; vars= [code_var; env_var]
          ; body= App {fcn= Var code_var; args= Var env_var :: args'} } )
  | Lam {vars; body} ->
      let fvs, body' = conv_exp (vars @ sc) body in
      let env_var = Id.from_string "env" in
      let v_env = Tuple (List.map (fun v -> Var v) fvs) in
      let v_code =
        Lam
          { vars= env_var :: vars
          ; body= Split {inp= Var env_var; vars= fvs; body= body'} }
      in
      ([], Tuple [v_code; v_env])
  | Prim {oper; args} ->
      let vss, args' = List.split (List.map (conv_exp sc) args) in
      (List.concat vss, Prim {oper; args= args'})
  | Let {isrec; vars; bnds; body} ->
      let fvss, bnds' = List.split (List.map (conv_exp (vars @ sc)) bnds) in
      let fvs, body' = conv_exp (vars @ sc) body in
      (List.concat fvss @ fvs, Let {isrec; vars; bnds= bnds'; body= body'})
  | If {cond; then_; else_} ->
      let vs1, cond' = conv_exp sc cond in
      let vs2, then' = conv_exp sc then_ in
      let vs3, else' = conv_exp sc else_ in
      (vs1 @ vs2 @ vs3, If {cond= cond'; then_= then'; else_= else'})
  | Tuple exps ->
      let vss, exps' = List.split (List.map (conv_exp sc) exps) in
      (List.concat vss, Tuple exps')
  | Split {inp; vars; body} ->
      let fvs1, inp' = conv_exp sc inp in
      let fvs2, body' = conv_exp (vars @ sc) body in
      (fvs1 @ fvs2, Split {inp= inp'; vars; body= body'})

let conv_prog (exp : exp) : exp = conv_exp [] exp |> snd
