module C = Core.Syntax
module K = Cps.Syntax

let rec c2k_exp (exp : C.exp) (k : K.value) : K.exp =
  match exp with
  | Const c -> K.mk_app k (K.Const c)
  | Var id -> K.mk_app k (K.Var id)
  | App {fcn= fcn, _; arg= arg, _} ->
      let fcn_var = Id.from_string "fcn" in
      let arg_var = Id.from_string "arg" in
      c2k_exp fcn
        (K.mk_lam fcn_var
           (c2k_exp arg
              (K.mk_lam arg_var (K.mk_apps (Var fcn_var) [K.Var arg_var; k])) ) )
  | Lam {var= var, _; body= body, _} ->
      let c_var = Id.from_string "c" in
      K.mk_apps k [K.mk_lams [var; c_var] (c2k_exp body (Var c_var))]
  | Prim {left= left, _; oper; right= right, _} ->
      let name = Id.from_string "prim" in
      let larg_var = Id.from_string "a0" in
      let rarg_var = Id.from_string "a1" in
      c2k_exp left
        (K.mk_lam larg_var
           (c2k_exp right
              (K.mk_lam rarg_var
                 (K.Let
                    { dec=
                        K.PrimDec
                          {name; left= Var larg_var; oper; right= Var rarg_var}
                    ; body= K.mk_app k (K.Var name) } ) ) ) )
  | If {cond= cond, _; then_= then_, _; else_= else_, _} ->
      let cond_var = Id.from_string "cond" in
      c2k_exp cond
        (K.mk_lam cond_var
           (K.If
              { cond= Var cond_var
              ; then_= c2k_exp then_ k
              ; else_= c2k_exp else_ k } ) )
  | Let {isrec= false; vars; bnds; body= body, _} ->
      List.fold_right2
        (fun (var, _) (bnd, _) body -> c2k_exp bnd (K.mk_lam var body))
        vars bnds (c2k_exp body k)
  | Let {isrec= true; vars; bnds; body= body, _} ->
      let fundefs =
        List.map2 c2k_fundef (List.map fst vars) (List.map fst bnds)
      in
      let body' = c2k_exp body k in
      Letrec {fundefs; body= body'}

and c2k_fundef (name : C.id) : C.exp -> K.fundef = function
  | C.Lam {var= var, _; body= body, _} ->
      let c_var = Id.from_string "c" in
      {name; vars= [var; c_var]; body= c2k_exp body (Var c_var)}
  | exp ->
      let c_var = Id.from_string "c" in
      {name; vars= [c_var]; body= c2k_exp exp (Var c_var)}

let c2k_prog (exp : C.exp) : K.exp =
  let var = Id.from_string "x" in
  c2k_exp exp (K.mk_lam var (K.Halt (Var var)))
