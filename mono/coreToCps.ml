module C = Core.Syntax
module K = Cps.Syntax

let rec c2k_exp (exp : C.exp) (k : K.value) : K.exp =
  match exp with
  | Const c -> K.apps k [K.Const c]
  | Var id -> K.apps k [K.Var id]
  | App {fcn; arg} ->
      let fcn_var = Id.from_string "fcn" in
      let arg_var = Id.from_string "arg" in
      c2k_exp fcn
        (K.lam fcn_var
           (c2k_exp arg
              (K.lam arg_var (K.apps (Var fcn_var) [K.Var arg_var; k])) ) )
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
           { dec=
               K.PrimDec {name; oper; args= List.map (fun v -> K.Var v) arg_vars}
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
  | Let {isrec= true; vars; bnds; body} ->
      let fundefs = List.map2 c2k_fundef vars bnds in
      let body' = c2k_exp body k in
      Letrec {fundefs; body= body'}

and c2k_fundef (name : C.id) : C.exp -> K.fundef = function
  | C.Lam {var; body} ->
      let c_var = Id.from_string "c" in
      {name; vars= [var; c_var]; body= c2k_exp body (Var c_var)}
  | exp ->
      let c_var = Id.from_string "c" in
      {name; vars= [c_var]; body= c2k_exp exp (Var c_var)}

let c2k_prog (exp : C.exp) : K.exp =
  let var = Id.from_string "x" in
  c2k_exp exp (K.lam var (K.Halt (Var var)))
