open Syntax

let notin xs ys = List.filter (fun y -> not (List.mem y ys)) xs

let rec conv_exp : exp -> id list * exp = function
  | Const c -> ([], Const c)
  | Var x -> ([x], Var x)
  | App {fcn; args} ->
      let vs, fcn' = conv_exp fcn in
      let vss, args' = List.split (List.map conv_exp args) in
      (vs @ List.concat vss, App {fcn= fcn'; args})
  | Lam {vars; body} ->
      let vs, body' = conv_exp body in
      let fvs = notin vs vars in
      let env_var = Id.from_string "env" in
      let v_env = App {fcn= Const Tuple; args= List.map (fun v -> Var v) fvs} in
      let v_code = Lam {vars= env_var :: vars; body= body'} in
      (fvs, App {fcn= Const Tuple; args= [v_code; v_env]})
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
