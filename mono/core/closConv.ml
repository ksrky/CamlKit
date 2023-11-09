open Syntax

let notin xs ys = List.filter (fun y -> not (List.mem y ys)) xs

let rec conv_exp : exp -> id list * exp = function
  | Const c -> ([], Const c)
  | Var x -> ([x], Var x)
  | App {fcn; args} ->
      let vs, fcn' = conv_exp fcn in
      let code_var = Id.from_string "code" and env_var = Id.from_string "env" in
      let vss, args' = List.split (List.map conv_exp args) in
      ( vs @ List.concat vss
      , Split
          { inp= fcn'
          ; vars= [code_var; env_var]
          ; body= App {fcn= Var code_var; args= Var env_var :: args'} } )
  | Lam {vars; body} ->
      let vs, body' = conv_exp body in
      let fvs = notin vs vars in
      let env_var = Id.from_string "env" in
      let v_env = Tuple (List.map (fun v -> Var v) fvs) in
      let v_code =
        Lam
          { vars= env_var :: vars
          ; body= Split {inp= Var env_var; vars= fvs; body= body'} }
      in
      (fvs, Tuple [v_code; v_env])
  | Prim {oper; args} ->
      let vss, args' = List.split (List.map conv_exp args) in
      (List.concat vss, Prim {oper; args})
  | Let {isrec; vars; bnds; body} ->
      let vss, bnds' = List.split (List.map conv_exp bnds) in
      let vs, body' = conv_exp body in
      let fvs = notin (List.concat vss @ vs) vars in
      (fvs, Let {isrec; vars; bnds= bnds'; body= body'})
  | If {cond; then_; else_} ->
      let vs1, cond' = conv_exp cond in
      let vs2, then' = conv_exp then_ in
      let vs3, else' = conv_exp else_ in
      (vs1 @ vs2 @ vs3, If {cond= cond'; then_= then'; else_= else'})
  | Tuple exps ->
      List.split (List.map conv_exp exps)
      |> fun (vss, exps') -> (List.concat vss, Tuple exps')
  | Split {inp; vars; body} ->
      let vs, inp' = conv_exp inp in
      let fvs = notin vs vars in
      (fvs, Split {inp= inp'; vars; body})

let conv_prog (exp : exp) : exp = conv_exp exp |> snd
