open Syntax

let notin xs ys = List.filter (fun y -> not (List.mem y ys)) xs

let rec conv_val : value -> id list * value = function
  | Const c -> ([], Const c)
  | Var x -> ([x], Var x)
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
  | Tuple vals ->
      let vs', vals' = List.split (List.map conv_val vals) in
      (List.concat vs', Tuple vals')

and conv_exp : exp -> id list * exp = function
  | Let {decs; body} ->
      let vss, decs' = List.split (List.map conv_dec decs) in
      let vs, body' = conv_exp body in
      (List.concat vss @ vs, Let {decs= decs'; body= body'})
  | App {fcn; args} ->
      let vs, fcn' = conv_val fcn in
      let code_var = Id.from_string "code" and env_var = Id.from_string "env" in
      let vss, args' = List.split (List.map conv_val args) in
      ( vs @ List.concat vss
      , Split
          { inp= fcn'
          ; vars= [code_var; env_var]
          ; body= App {fcn= Var code_var; args= Var env_var :: args'} } )
  | If {cond; then_; else_} ->
      let vs1, cond' = conv_val cond in
      let vs2, then' = conv_exp then_ in
      let vs3, else' = conv_exp else_ in
      (vs1 @ vs2 @ vs3, If {cond= cond'; then_= then'; else_= else'})
  | Split {inp; vars; body} -> failwith "not implemented"
  | Halt value ->
      let vs, value' = conv_val value in
      (vs, Halt value')

and conv_dec : dec -> id list * dec = function
  | ValDec {name; value} ->
      let vs, value' = conv_val value in
      (notin vs [name], ValDec {name; value= value'})
  | ProjDec {name; index; tuple} ->
      let vs, tuple' = conv_val tuple in
      (notin vs [name], ProjDec {name; index; tuple= tuple'})
  | PrimDec {name; oper; args} ->
      let vss, body' = List.split (List.map conv_val args) in
      (notin (List.concat vss) [name], PrimDec {name; oper; args})
