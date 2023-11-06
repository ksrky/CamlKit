module L = Language.Syntax
module E = Env
module T = Types
module U = Unification

let rec check (env : E.env) (exp : L.exp) (exp_ty : L.ty) : L.expty =
  match exp with
  | VarExp id ->
      let ty = E.lookup_type id env in
      U.unify ty exp_ty; (VarAExp id, ty)
  | NilExp -> (NilAExp, NilTy)
  | BoolExp b -> U.unify L.BoolTy exp_ty; (BoolAExp b, BoolTy)
  | IntExp i -> U.unify L.IntTy exp_ty; (IntAExp i, IntTy)
  | AppExp {fcn; arg} ->
      let arg_ty = T.new_tyvar () in
      let fcn' = check env fcn (L.FunTy (arg_ty, exp_ty)) in
      let arg' = check env arg arg_ty in
      (AppAExp {fcn= fcn'; arg= arg'}, exp_ty)
  | LamExp {vars; body} ->
      let var_tys, body_ty = U.unify_funs vars exp_ty in
      let env' = E.extend_vals vars var_tys env in
      check env' body body_ty
  | OpExp {left; op= (PlusOp | MinusOp | TimesOp | DivideOp) as op; right} ->
      let left' = check env left L.IntTy and right' = check env right L.IntTy in
      U.unify L.IntTy exp_ty;
      (OpAExp {left= left'; op; right= right'}, IntTy)
  | OpExp {left; op= (LtOp | LeOp | GtOp | GeOp) as op; right} ->
      U.unify L.BoolTy exp_ty;
      let left' = check env left L.IntTy and right' = check env right L.IntTy in
      (OpAExp {left= left'; op; right= right'}, BoolTy)
  | OpExp {left; op= (EqOp | NeqOp) as op; right} -> (
      U.unify L.BoolTy exp_ty;
      try
        let left' = check env left L.IntTy
        and right' = check env right L.IntTy in
        (OpAExp {left= left'; op; right= right'}, BoolTy)
      with _ -> (
        try
          let left' = check env left L.BoolTy
          and right' = check env right L.BoolTy in
          (OpAExp {left= left'; op; right= right'}, BoolTy)
        with _ ->
          Error.error "type mismatched";
          (NilAExp, NilTy) ) )
  | IfExp {cond; then_; else_} ->
      let cond' = check env cond L.BoolTy
      and then_' = check env then_ exp_ty
      and else_' = check env else_ exp_ty in
      (IfAExp {cond= cond'; then_= then_'; else_= else_'}, exp_ty)
  | LetExp {bnds; body} ->
      let bnd_tys = List.map (fun _ -> T.new_tyvar ()) bnds in
      let bnds' = check_bnds env bnds bnd_tys in
      let env' =
        E.extend_vals
          (List.map (fun (L.Bind {name; _}) -> name) bnds)
          bnd_tys env
      in
      (LetAExp {bnds= bnds'; body= check env' body exp_ty}, exp_ty)
  | LetrecExp {bnds; body} ->
      let bnd_tys = List.map (fun _ -> T.new_tyvar ()) bnds in
      let env' =
        E.extend_vals
          (List.map (fun (L.Bind {name; _}) -> name) bnds)
          bnd_tys env
      in
      let bnds' = check_bnds env' bnds bnd_tys in
      (LetrecAExp {bnds= bnds'; body= check env' body exp_ty}, exp_ty)

and check_bnds (env : E.env) (bnds : L.bnd list) (exp_tys : L.ty list) :
    L.abnd list =
  List.map2
    (fun (L.Bind {name; params; body}) exp_ty ->
      let param_tys, body_ty = U.unify_funs params exp_ty in
      let env' = E.extend_vals params param_tys env in
      L.ABind
        { name
        ; params= List.combine params param_tys
        ; body= check env' body body_ty } )
    bnds exp_tys

let check_prog (env : E.env) (prog : L.exp) : L.aexp =
  check env prog (T.new_tyvar ()) |> fst |> Types.zonk_aexp
