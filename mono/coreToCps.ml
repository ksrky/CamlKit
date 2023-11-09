module C = Core.Syntax
module K = Cps.Syntax

let rec c2k_exp (exp : C.exp) (k : K.value) : K.exp =
  match exp with
  | Const c -> K.App {fcn= k; args= [K.Const c]}
  | Var id -> K.App {fcn= k; args= [K.Var id]}
  | App {fcn; args} ->
      let fcn_var = Id.from_string "fcn" in
      let arg_vars =
        List.mapi (fun i _ -> Id.from_string ("arg" ^ string_of_int i)) args
      in
      failwith ""
      (* List.fold_right
         (fun arg exp -> K.App {fcn= c2k_exp k arg; args= exp})
         (fcn :: args) (c2k_exp k fcn)*)
  | Lam {vars; body} ->
      let c_var = Id.from_string "c" in
      K.App
        { fcn= k
        ; args= [K.Lam {vars= vars @ [c_var]; body= c2k_exp body (Var c_var)}]
        }
  | Prim {oper; args} ->
      failwith ""
      (*
      List.fold_right
        (fun arg exp -> K.App {fcn= c2k_exp k arg; args= exp})
        args (K.Prim oper*)
  | If {cond; then_; else_} ->
      let cond_var = Id.from_string "cond" in
      c2k_exp cond
        (K.Lam
           { vars= [cond_var]
           ; body=
               K.If
                 { cond= Var cond_var
                 ; then_= c2k_exp then_ k
                 ; else_= c2k_exp else_ k } } )
  | Let {vars; bnds; body} -> failwith "not implemented"
  | Tuple exps ->
      let tuple_vars =
        List.mapi (fun i _ -> Id.from_string ("tuple" ^ string_of_int i)) exps
      in
      List.fold_right2
        (fun var elm body -> c2k_exp elm (Lam {vars= [var]; body}))
        tuple_vars exps
        (K.App {fcn= k; args= List.map (fun v -> K.Var v) tuple_vars})
  | Split {inp; vars; body} ->
      let split_var = Id.from_string "split" in
      c2k_exp inp
        (Lam
           { vars= [split_var]
           ; body= Split {inp= Var split_var; vars; body= c2k_exp body k} } )
