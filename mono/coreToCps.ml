module C = Core.Syntax
module K = Cps.Syntax

let rec c2k_exp (exp : C.exp) (k : K.value) : K.exp = failwith ""
(*
  match exp with
  | Const c -> K.apps k [K.Const c]
  | Var id -> K.apps k [K.Var id]
  | App {fcn; arg} ->
      let fcn_var = Id.from_string "fcn" in
      let arg_vars =
        List.mapi (fun i _ -> Id.from_string ("arg" ^ string_of_int i)) args
      in
      c2k_exp fcn
        (K.lam fcn_var
           (List.fold_right2
              (fun var arg body -> c2k_exp arg (K.lam var body))
              arg_vars arg
              (K.apps (Var fcn_var)
                 (List.map (fun v -> K.Var v) arg_vars @ [k]) ) ) )
  | Lam {var; body} ->
      let c_var = Id.from_string "c" in
      K.apps k [K.lams (var :: [c_var]) (c2k_exp body (Var c_var))]
  | Prim {oper; args} ->
      let name = Id.from_string "prim" in
      let arg_vars =
        List.mapi (fun i _ -> Id.from_string ("arg" ^ string_of_int i)) args
      in
      List.fold_right2
        (fun var arg body -> c2k_exp arg (K.lam var body))
        arg_vars args
        (K.Let
           { decs=
               [ K.PrimDec
                   {name; oper; args= List.map (fun v -> K.Var v) arg_vars} ]
           ; body= K.app k (K.Var name) } )
  | If {cond; then_; else_} ->
      let cond_var = Id.from_string "cond" in
      c2k_exp cond
        (K.lam cond_var
           (K.If
              { cond= Var cond_var
              ; then_= c2k_exp then_ k
              ; else_= c2k_exp else_ k } ) )
  | Let {isrec= false; vars; bnds; body} ->
      List.fold_right2
        (fun var bnd body -> c2k_exp bnd (K.lam var body))
        vars bnds (c2k_exp body k)
  | Let {isrec= true; vars; bnds; body} -> failwith "TODO" *)
