module A = Language.Syntax
module E = Env
module T = Types
module U = Unification

type expected = Infer of T.ty option ref | Check of T.ty

let is_arith_op : A.op -> bool = function
  | PlusOp | MinusOp | TimesOp | DivideOp -> true
  | _ -> false

let is_comp_op : A.op -> bool = function LtOp | LeOp | GtOp | GeOp -> true | _ -> false

let is_equal_op : A.op -> bool = function EqOp | NeqOp -> true | _ -> false

let rec check (env : E.env) (exp : A.exp) (exp_ty : T.ty) : unit =
  match exp with
  | VarExp id ->
      let ty = E.lookup_type id env in
      U.unify ty exp_ty
  | NilExp -> ()
  | BoolExp _ -> U.unify T.tBOOL exp_ty
  | IntExp _ -> U.unify T.tINT exp_ty
  | AppExp {fcn; arg} ->
      let arg_ty = T.new_tyvar () in
      check env fcn (T.FunTy (arg_ty, exp_ty));
      check env arg arg_ty
  | LamExp {vars; body} ->
      let arg_tys, body_ty = U.unify_funs vars exp_ty in
      let env' = List.fold_right2 (fun id ty -> E.extend id (ValBind ty)) vars arg_tys env in
      check env' body body_ty
  | OpExp {left; op= PlusOp | MinusOp | TimesOp | DivideOp; right} ->
      check env left T.tINT; check env right T.tINT; U.unify T.tINT exp_ty
  | OpExp {left; op= LtOp | LeOp | GtOp | GeOp; right} ->
      U.unify T.tBOOL exp_ty; check env left T.tINT; check env right T.tINT
  | OpExp {left; op= EqOp | NeqOp; right} -> (
      U.unify T.tBOOL exp_ty;
      try check env left T.tINT; check env right T.tINT
      with _ -> (
        try check env left T.tBOOL; check env right T.tBOOL
        with _ -> Error.error "type mismatched" ) )
  | IfExp {cond; then_; else_} ->
      check env cond T.tBOOL; check env then_ exp_ty; check env else_ exp_ty
  | LetExp {bnds; body} ->
      let tvs = List.map (fun _ -> T.new_tyvar ()) bnds in
      let env' = List.fold_right2 (fun {A.name; _} tv -> E.extend name (ValBind tv)) bnds tvs env in
      check_bnds env bnds tvs; check env' body exp_ty
  | LetrecExp {bnds; body} ->
      let tvs = List.map (fun _ -> T.new_tyvar ()) bnds in
      let env' = List.fold_right2 (fun {A.name; _} tv -> E.extend name (ValBind tv)) bnds tvs env in
      check_bnds env' bnds tvs; check env' body exp_ty

and check_bnds (env : E.env) (bnds : A.bnd list) (exp_tys : T.ty list) : unit =
  List.iter2
    (fun {A.params; A.body; _} exp_ty ->
      let env' = List.fold_right (fun id -> E.extend id (ValBind (T.new_tyvar ()))) params env in
      check env' body exp_ty )
    bnds exp_tys

let check_prog (env : E.env) (prog : A.exp) : unit = check env prog (T.new_tyvar ())
