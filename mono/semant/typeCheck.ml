module L = Language.Syntax
module E = Env
module T = Types
module U = Unification

let rec check (env : E.env) (exp : L.exp) (exp_ty : L.ty) : unit =
  match exp with
  | VarExp id ->
      let ty = E.lookup_type id env in
      U.unify ty exp_ty
  | NilExp -> ()
  | BoolExp _ -> U.unify L.BoolTy exp_ty
  | IntExp _ -> U.unify L.IntTy exp_ty
  | AppExp {fcn; arg} ->
      let arg_ty = T.new_tyvar () in
      check env fcn (L.FunTy (arg_ty, exp_ty));
      check env arg arg_ty
  | LamExp {vars; body} ->
      let arg_tys, body_ty = U.unify_funs vars exp_ty in
      let env' = List.fold_right2 (fun id ty -> E.extend id (ValBind ty)) vars arg_tys env in
      check env' body body_ty
  | OpExp {left; op= PlusOp | MinusOp | TimesOp | DivideOp; right} ->
      check env left L.IntTy; check env right L.IntTy; U.unify L.IntTy exp_ty
  | OpExp {left; op= LtOp | LeOp | GtOp | GeOp; right} ->
      U.unify L.BoolTy exp_ty; check env left L.IntTy; check env right L.IntTy
  | OpExp {left; op= EqOp | NeqOp; right} -> (
      U.unify L.BoolTy exp_ty;
      try check env left L.IntTy; check env right L.IntTy
      with _ -> (
        try check env left L.BoolTy; check env right L.BoolTy
        with _ -> Error.error "type mismatched" ) )
  | IfExp {cond; then_; else_} ->
      check env cond L.BoolTy; check env then_ exp_ty; check env else_ exp_ty
  | LetExp {bnds; body} ->
      let tvs = List.map (fun _ -> T.new_tyvar ()) bnds in
      let env' =
        List.fold_right2 (fun (L.Bind {name; _}) tv -> E.extend name (ValBind tv)) bnds tvs env
      in
      check_bnds env bnds tvs; check env' body exp_ty
  | LetrecExp {bnds; body} ->
      let tvs = List.map (fun _ -> T.new_tyvar ()) bnds in
      let env' =
        List.fold_right2 (fun (L.Bind {name; _}) tv -> E.extend name (ValBind tv)) bnds tvs env
      in
      check_bnds env' bnds tvs; check env' body exp_ty

and check_bnds (env : E.env) (bnds : L.bnd list) (exp_tys : L.ty list) : unit =
  List.iter2
    (fun (L.Bind {params; body; _}) exp_ty ->
      let env' = List.fold_right (fun id -> E.extend id (ValBind (T.new_tyvar ()))) params env in
      check env' body exp_ty )
    bnds exp_tys

let check_prog (env : E.env) (prog : L.exp) : unit = check env prog (T.new_tyvar ())
