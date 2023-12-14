module C = Core.Syntax
module K = Cps.Syntax

let rec c2k_ty : C.ty -> K.ty = function
  | IntTy -> IntTy
  | BoolTy -> BoolTy
  | FunTy (ty1, ty2) -> ContTy [c2k_ty ty1; c2k_ty ty2]

let c2k_cont ty : K.ty = ContTy [c2k_ty ty]

let c2k_var (id, ty) : K.var = (id, c2k_ty ty)

let varlist : C.var list ref = ref [] (* TODO: Hashtbl? *)

let rec c2k_exp (exp : C.exp) (k : K.value) : K.exp =
  match exp with
  | Const c -> K.mk_app k (K.Const c)
  | Var id ->
      let ty = c2k_ty (List.assoc id !varlist) in
      K.mk_app k (K.Var (id, ty))
  | App {fcn= fcn, fcn_ty; arg= arg, arg_ty} ->
      let fcn_var = (Id.from_string "fcn", c2k_ty fcn_ty) in
      let arg_var = (Id.from_string "arg", c2k_ty arg_ty) in
      c2k_exp fcn
        (K.mk_lam fcn_var
           (c2k_exp arg
              (K.mk_lam arg_var (K.mk_apps (Var fcn_var) [K.Var arg_var; k])) ) )
  | Lam {var; body= body, body_ty} ->
      varlist := var :: !varlist;
      let c_var = (Id.from_string "c", c2k_cont body_ty) in
      K.mk_apps k [K.mk_lams [c2k_var var; c_var] (c2k_exp body (Var c_var))]
  | Prim {left= left, left_ty; oper; right= right, right_ty} ->
      let var_ty =
        match oper with
        | Add | Sub | Mul | Div -> K.IntTy
        | Eq | Ne | Lt | Le | Gt | Ge -> K.BoolTy
      in
      let var = (Id.from_string "prim", var_ty) in
      let left_var = (Id.from_string "left", c2k_ty left_ty) in
      let right_var = (Id.from_string "right", c2k_ty right_ty) in
      c2k_exp left
        (K.mk_lam left_var
           (c2k_exp right
              (K.mk_lam right_var
                 (K.Let
                    { dec=
                        K.PrimDec
                          {var; left= Var left_var; oper; right= Var right_var}
                    ; body= K.mk_app k (K.Var var) } ) ) ) )
  | If {cond= cond, cond_ty; then_= then_, _; else_= else_, _} ->
      let cond_var = (Id.from_string "cond", c2k_ty cond_ty) in
      c2k_exp cond
        (K.mk_lam cond_var
           (K.If
              { cond= Var cond_var
              ; then_= c2k_exp then_ k
              ; else_= c2k_exp else_ k } ) )
  | Let {isrec= false; vars; bnds; body= body, _} ->
      varlist := vars @ !varlist;
      List.fold_right2
        (fun var (bnd, _) body -> c2k_exp bnd (K.mk_lam (c2k_var var) body))
        vars bnds (c2k_exp body k)
  | Let {isrec= true; vars; bnds; body= body, _} ->
      varlist := vars @ !varlist;
      let fundefs = List.map2 c2k_fundef vars bnds in
      let body' = c2k_exp body k in
      Letrec {fundefs; body= body'}

and c2k_fundef (name : C.var) : C.expty -> K.fundef = function
  | C.Lam {var; body= body, body_ty}, _ ->
      varlist := var :: !varlist;
      let c_var = (Id.from_string "c", c2k_cont body_ty) in
      { var= c2k_var name
      ; params= [c2k_var var; c_var]
      ; body= c2k_exp body (Var c_var) }
  | exp, exp_ty ->
      let c_var = (Id.from_string "c", c2k_cont exp_ty) in
      {var= c2k_var name; params= [c_var]; body= c2k_exp exp (Var c_var)}

let c2k_prog ((exp, ty) : C.prog) : K.prog =
  let var = (Id.from_string "x", c2k_ty ty) in
  c2k_exp exp (K.mk_lam var (K.Halt (Var var)))
