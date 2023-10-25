module L = Language.Syntax
module E = Env
module T = Types
module U = Unification

let rec check (env : E.env) (exp : L.exp) (exp_ty : L.ty) : L.aexp =
  match exp with
  | VarExp id ->
      let ty = E.lookup_type id env in
      U.unify ty exp_ty;
      VarAExp (id, ty)
  | NilExp -> NilAExp
  | BoolExp b -> U.unify L.BoolTy exp_ty; BoolAExp b
  | IntExp i -> U.unify L.IntTy exp_ty; IntAExp i
  | AppExp {fcn; arg} ->
      let arg_ty = T.new_tyvar () in
      let fcn' = check env fcn (L.FunTy (arg_ty, exp_ty)) in
      let arg' = check env arg arg_ty in
      AppAExp {fcn= fcn'; arg= arg'}
  | LamExp {vars; body} ->
      let arg_tys, body_ty = U.unify_funs vars exp_ty in
      let env' = List.fold_right2 (fun id ty -> E.extend id (ValBind ty)) vars arg_tys env in
      check env' body body_ty
  | OpExp {left; op= PlusOp | MinusOp | TimesOp | DivideOp; right} ->
      let left' = check env left L.IntTy and right' = check env right L.IntTy in
      U.unify L.IntTy exp_ty;
      OpAExp {left= left'; op= PlusOp; right= right'}
  | OpExp {left; op= LtOp | LeOp | GtOp | GeOp; right} ->
      U.unify L.BoolTy exp_ty;
      let left' = check env left L.IntTy and right' = check env right L.IntTy in
      OpAExp {left= left'; op= LtOp; right= right'}
  | OpExp {left; op= EqOp | NeqOp; right} -> (
      U.unify L.BoolTy exp_ty;
      try
        let left' = check env left L.IntTy and right' = check env right L.IntTy in
        OpAExp {left= left'; op= EqOp; right= right'}
      with _ -> (
        try
          let left' = check env left L.BoolTy and right' = check env right L.BoolTy in
          OpAExp {left= left'; op= EqOp; right= right'}
        with _ -> Error.error "type mismatched"; NilAExp ) )
  | IfExp {cond; then_; else_} ->
      let cond' = check env cond L.BoolTy
      and then_' = check env then_ exp_ty
      and else_' = check env else_ exp_ty in
      IfAExp {cond= cond'; then_= then_'; else_= else_'}
  | LetExp {bnds; body} ->
      let tvs = List.map (fun _ -> T.new_tyvar ()) bnds in
      let env' =
        List.fold_right2 (fun (L.Bind {name; _}) tv -> E.extend name (ValBind tv)) bnds tvs env
      in
      let bnds' = check_bnds env bnds tvs in
      LetAExp {bnds= bnds'; body= check env' body exp_ty}
  | LetrecExp {bnds; body} ->
      let tvs = List.map (fun _ -> T.new_tyvar ()) bnds in
      let env' =
        List.fold_right2 (fun (L.Bind {name; _}) tv -> E.extend name (ValBind tv)) bnds tvs env
      in
      let bnds' = check_bnds env' bnds tvs in
      LetrecAExp {bnds= bnds'; body= check env' body exp_ty}

and check_bnds (env : E.env) (bnds : L.bnd list) (exp_tys : L.ty list) : L.abnd list =
  List.map2
    (fun (L.Bind {name; params; body}) exp_ty ->
      let params' = List.map (fun id -> (id, T.new_tyvar ())) params in
      let env' = List.fold_right (fun id -> E.extend id (ValBind (T.new_tyvar ()))) params env in
      L.ABind {name; params= params'; body= check env' body exp_ty} )
    bnds exp_tys

let check_prog (env : E.env) (prog : L.exp) : L.aexp = check env prog (T.new_tyvar ())
