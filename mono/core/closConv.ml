open Syntax

let notin xs ys = List.filter (fun y -> not (List.mem y ys)) xs

let rec conv_exp  : exp -> id list * exp = function
  | Const c -> ([], Const c) 
  | Var x -> ([x], Var x)
  | App {fcn; args} ->
      let vs, fcn' = conv_exp fcn in
      let code_var = Id.from_string "code" and env_var = Id.from_string "env" in
      let vss, args' = List.split (List.map (conv_exp) args) in
      ( vs @ List.concat vss
      , Split
          { inp= fcn'
          ; vars= [code_var; env_var]
          ; body= App {fcn= Var code_var; args= Var env_var :: args'} } )
  | Lam {vars; body} ->
      let vs, body' = conv_exp body in 
      let env_var = Id.from_string "env" in
      let fvs = notin vs vars in
      let v_code =
        Lam
          { vars= env_var :: vars
          ; body= Split {inp= Var env_var; vars= fvs; body= body'} }
      in
      let v_env = Tuple (List.map (fun v -> Var v) vs) in
      (fvs , Tuple [v_code; v_env])
  | Prim {oper; args} ->
      let vss, args' = List.split (List.map conv_exp args) in
      (List.concat vss, Prim {oper; args= args'})
  | Let {isrec; vars; bnds; body} ->
      let vss, bnds' = List.split (List.map conv_exp bnds) in
      let vs, body' = conv_exp  body in
      let fvs = notin (List.concat vss @ vs) vars in
      (fvs, Let {isrec; vars; bnds= bnds'; body= body'})
  | If {cond; then_; else_} ->
      let fvs1, cond' = conv_exp cond in
      let fvs2, then' = conv_exp then_ in
      let fvs3, else' = conv_exp else_ in
      (fvs1 @ fvs2 @ fvs3, If {cond= cond'; then_= then'; else_= else'})
  | Tuple exps ->
      let fvss, exps' = List.split (List.map (conv_exp) exps) in
      (List.concat fvss, Tuple exps')
  | Split {inp; vars; body} ->
      let fvs1, inp' = conv_exp inp in
      let fvs2, body' = conv_exp body in
      (fvs1 @ fvs2, Split {inp= inp'; vars; body= body'})

let conv_prog (exp : exp) : exp = conv_exp  exp |> snd
