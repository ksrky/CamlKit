module A = Abstract.Syntax
module E = Env
module T = Types
module U = Unification

let rec check (env : E.env) (exp : A.exp) (exp_ty : A.ty) : A.expty =
  match exp with
  | VarExp id ->
      let ty = E.lookup_type id env in
      U.unify ty exp_ty;
      (VarAExp (id, ty), ty)
  | NilExp -> (NilAExp, NilTy)
  | BoolExp b -> U.unify A.BoolTy exp_ty; (BoolAExp b, BoolTy)
  | IntExp i -> U.unify A.IntTy exp_ty; (IntAExp i, IntTy)
  | AppExp {fcn; arg} ->
      let arg_ty = T.new_tyvar () in
      let fcn' = check env fcn (A.FunTy (arg_ty, exp_ty)) in
      let arg' = check env arg arg_ty in
      (AppAExp {fcn= fcn'; arg= arg'}, exp_ty)
  | LamExp {vars; body} ->
      let var_tys, body_ty = U.unify_funs vars exp_ty in
      let env' = E.extend_vals vars var_tys env in
      let body' = check env' body body_ty in
      (LamAExp {params= List.combine vars var_tys; body= body'}, exp_ty)
  | OpExp {left; op= (PlusOp | MinusOp | TimesOp | DivideOp) as op; right} ->
      let left' = check env left A.IntTy and right' = check env right A.IntTy in
      U.unify A.IntTy exp_ty;
      (OpAExp {left= left'; op; right= right'}, IntTy)
  | OpExp {left; op= (LtOp | LeOp | GtOp | GeOp) as op; right} ->
      U.unify A.BoolTy exp_ty;
      let left' = check env left A.IntTy and right' = check env right A.IntTy in
      (OpAExp {left= left'; op; right= right'}, BoolTy)
  | OpExp {left; op= (EqOp | NeqOp) as op; right} -> (
      U.unify A.BoolTy exp_ty;
      try
        let left' = check env left A.IntTy
        and right' = check env right A.IntTy in
        (OpAExp {left= left'; op; right= right'}, BoolTy)
      with _ -> (
        try
          let left' = check env left A.BoolTy
          and right' = check env right A.BoolTy in
          (OpAExp {left= left'; op; right= right'}, BoolTy)
        with _ ->
          let left_ty = typeof env left and right_ty = typeof env right in
          Error.binop_error op left_ty right_ty;
          (NilAExp, NilTy) ) )
  | IfExp {cond; then_; else_} ->
      let cond' = check env cond A.BoolTy
      and then_' = check env then_ exp_ty
      and else_' = check env else_ exp_ty in
      (IfAExp {cond= cond'; then_= then_'; else_= else_'}, exp_ty)
  | LetExp {bnds; body} ->
      let bnd_tys = List.map (fun _ -> T.new_tyvar ()) bnds in
      let bnds' = check_bnds env bnds bnd_tys in
      let env' =
        E.extend_vals
          (List.map (fun (A.Bind {name; _}) -> name) bnds)
          bnd_tys env
      in
      (LetAExp {bnds= bnds'; body= check env' body exp_ty}, exp_ty)
  | LetrecExp {bnds; body} ->
      let bnd_tys = List.map (fun _ -> T.new_tyvar ()) bnds in
      let env' =
        E.extend_vals
          (List.map (fun (A.Bind {name; _}) -> name) bnds)
          bnd_tys env
      in
      let bnds' = check_bnds ~isrec:true env' bnds bnd_tys in
      (LetrecAExp {bnds= bnds'; body= check env' body exp_ty}, exp_ty)

and typeof (env : E.env) (exp : A.exp) : A.ty =
  let _, ty = check env exp (Types.new_tyvar ()) in
  Types.zonk_ty ty

and check_bnds ?(isrec = false) (env : E.env) (bnds : A.bnd list)
    (exp_tys : A.ty list) : A.abnd list =
  List.map2
    (fun (A.Bind {name; params; body}) exp_ty ->
      if isrec && params = [] then Error.letrec_error name;
      let param_tys, body_ty = U.unify_funs params exp_ty in
      let env' = E.extend_vals params param_tys env in
      A.ABind
        { name
        ; params= List.combine params param_tys
        ; body= check env' body body_ty } )
    bnds exp_tys

let check_prog (env : E.env) (prog : A.exp) : A.aprog =
  let exp, ty = check env prog (T.new_tyvar ()) in
  (Types.zonk_aexp exp, Types.zonk_ty ty)
