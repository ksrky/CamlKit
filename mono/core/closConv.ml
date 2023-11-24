open Syntax

let notin xs ys = List.filter (fun y -> not (List.mem y ys)) xs

let remove_dup xs =
  List.fold_right (fun x xs -> if List.mem x xs then xs else x :: xs) xs []

type exp_clos = E of exp | C of clos

let unE : exp_clos -> exp = function E exp -> exp | C clos -> Clos clos

let rec conv_exp : exp -> id list * exp_clos = function
  | Const c -> ([], E (Const c))
  | Var x -> ([x], E (Var x))
  | App {fcn; args} ->
      let vs, fcn' = conv_exp fcn in
      let vss, args' = List.split (List.map conv_exp args) in
      let res =
        match fcn' with
        | E exp -> E (App {fcn= exp; args= List.map unE args'})
        | C clos -> C (ClosApp {clos; args= List.map unE args'})
      in
      (vs @ List.concat vss, res)
  | Lam {vars; body} ->
      let vs, body' = conv_exp body in
      let fvs = remove_dup (notin vs vars) in
      let env_var = Id.from_string "env" in
      ( fvs
      , C (Clos {env= fvs; code= Lam {vars= env_var :: vars; body= unE body'}})
      )
  | Prim {oper; args} ->
      let vss, args' = List.split (List.map conv_exp args) in
      (List.concat vss, E (Prim {oper; args= List.map unE args'}))
  | Let {isrec; vars; bnds; body} ->
      let vss, bnds' = List.split (List.map conv_exp bnds) in
      let vs, body' = conv_exp body in
      let fvs = notin (List.concat vss @ vs) vars in
      (fvs, E (Let {isrec; vars; bnds= List.map unE bnds'; body= unE body'}))
  | If {cond; then_; else_} ->
      let fvs1, cond' = conv_exp cond in
      let fvs2, then' = conv_exp then_ in
      let fvs3, else' = conv_exp else_ in
      ( fvs1 @ fvs2 @ fvs3
      , E (If {cond= unE cond'; then_= unE then'; else_= unE else'}) )
  | Clos clos -> ([], C clos)

let conv_prog (exp : exp) : exp = conv_exp exp |> snd |> unE
