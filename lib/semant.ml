module A = AbsSyn
module I = IntSyn
module E = Env
module T = Types
module Tc = TypeCheck

type expected = Infer of A.ty option ref | Check of A.ty

let check_type (ty : A.ty) : expected -> unit = function
  | Check ty' -> Tc.unify ty ty'
  | Infer ref -> ref := Some ty

let rec check_exp env exp ty : I.exp = trans_exp env exp (Check ty)

and infer_exp env exp : I.exp * A.ty =
  let ref = ref None in
  let exp' = trans_exp env exp (Infer ref) in
  (exp', Option.get !ref)

and trans_exp env (exp : A.exp) (exp_ty : expected) : I.exp =
  let rec trexp : A.exp * expected -> I.exp = function
    | VarExp id, exp_ty ->
        check_type (E.lookup_type id env) exp_ty;
        Var id
    | NilExp, _ -> Nil
    | BoolExp b, exp_ty ->
        check_type T.tBOOL exp_ty;
        if b then Int 1 else Int 0
    | IntExp x, exp_ty -> check_type T.tINT exp_ty; Int x
    | (AppExp _ as exp), exp_ty ->
        let rec loop acc exp_ty : A.exp -> I.exp = function
          | AppExp {fcn; arg} ->
              let arg_ty = T.new_tyvar () and res_ty = T.new_tyvar () in
              check_type res_ty exp_ty;
              loop (check_exp env arg arg_ty :: acc) exp_ty fcn
          | VarExp id when Ident.name id = "read_int" ->
              check_type (E.lookup_type id env) exp_ty;
              Prim ("readi", acc)
          | VarExp id when Ident.name id = "print_int" ->
              check_type (E.lookup_type id env) exp_ty;
              Prim ("printi", acc)
          | VarExp id when Ident.name id = "array_make" ->
              check_type (E.lookup_type id env) exp_ty;
              Prim ("init_array", acc)
          | fcn -> App (trexp (fcn, exp_ty), acc)
        in
        loop [] exp_ty exp
    | LamExp {vars; body}, Check ty ->
        let arg_tys, body_ty = Tc.unify_funs vars ty in
        let env' = List.fold_right2 (fun id ty -> E.extend id (ValBind ty)) vars arg_tys env in
        let body' = check_exp env' body body_ty in
        Lam (vars, body')
    | LamExp {vars; body}, Infer ref ->
        let var_tys = List.map (fun _ -> T.new_tyvar ()) vars in
        let env' = List.fold_right2 (fun id ty -> E.extend id (ValBind ty)) vars var_tys env in
        let body', body_ty = infer_exp env' body in
        ref := Some (List.fold_right (fun l r -> A.FunTy (l, r)) var_tys body_ty);
        Lam (vars, body')
    | OpExp {left; op= PlusOp; right}, exp_ty ->
        check_type T.tINT exp_ty;
        Prim ("add", [check_exp env left T.tINT; check_exp env right T.tINT])
    | OpExp {left; op= MinusOp; right}, exp_ty ->
        check_type T.tINT exp_ty;
        Prim ("sub", [check_exp env left T.tINT; check_exp env right T.tINT])
    | OpExp {left; op= TimesOp; right}, exp_ty ->
        check_type T.tINT exp_ty;
        Prim ("mul", [check_exp env left T.tINT; check_exp env right T.tINT])
    | OpExp {left; op= DivideOp; right}, exp_ty ->
        check_type T.tINT exp_ty;
        Prim ("div", [check_exp env left T.tINT; check_exp env right T.tINT])
    | OpExp {left; op= EqOp; right}, exp_ty ->
        check_type T.tBOOL exp_ty;
        Prim ("eq", [check_exp env left T.tINT; check_exp env right T.tINT])
    | OpExp {left; op= NeqOp; right}, exp_ty ->
        check_type T.tBOOL exp_ty;
        Prim ("ne", [check_exp env left T.tINT; check_exp env right T.tINT])
    | OpExp {left; op= LtOp; right}, exp_ty ->
        check_type T.tBOOL exp_ty;
        Prim ("lt", [check_exp env left T.tINT; check_exp env right T.tINT])
    | OpExp {left; op= LeOp; right}, exp_ty ->
        check_type T.tBOOL exp_ty;
        Prim ("le", [check_exp env left T.tINT; check_exp env right T.tINT])
    | OpExp {left; op= GtOp; right}, exp_ty ->
        check_type T.tBOOL exp_ty;
        Prim ("lt", [check_exp env right T.tINT; check_exp env left T.tINT])
    | OpExp {left; op= GeOp; right}, exp_ty ->
        check_type T.tBOOL exp_ty;
        Prim ("le", [check_exp env right T.tINT; check_exp env left T.tINT])
    | IfExp {test; then_; else_}, exp_ty ->
        If (check_exp env test T.tBOOL, trexp (then_, exp_ty), trexp (else_, exp_ty))
    | LetExp {bnds; body}, exp_ty ->
        let env' =
          List.fold_right
            (fun {A.name; _} ->
              let tv = T.new_tyvar () in
              E.extend name (ValBind tv) )
            bnds env
        in
        Let
          ( false
          , List.map (fun {A.name; _} -> name) bnds
          , trans_bnds env bnds
          , trans_exp env' body exp_ty )
    | LetrecExp {bnds; body}, exp_ty ->
        let env' =
          List.fold_right
            (fun {A.name; _} ->
              let tv = T.new_tyvar () in
              E.extend name (ValBind tv) )
            bnds env
        in
        Let
          ( true
          , List.map (fun {A.name; _} -> name) bnds
          , trans_bnds env' bnds
          , trans_exp env' body exp_ty )
    | SubscExp {arr; idx}, exp_ty ->
        check_type T.tINT exp_ty;
        (* tmp : array polymorphism *)
        Select (check_exp env idx T.tINT, check_exp env arr T.tARRAY)
    | AssignExp {arr; idx; rhs}, exp_ty ->
        check_type T.tUNIT exp_ty;
        Rewrite
          (Select (check_exp env idx T.tINT, check_exp env arr T.tARRAY), check_exp env rhs T.tINT)
  in
  try trexp (exp, exp_ty)
  with E.Out_of_scope id ->
    ErrorMsg.error ("Unbound value " ^ Ident.name id);
    Nil

and trans_bnds env bnds =
  let trbnds =
    List.map (fun {A.params; A.body; _} ->
        let tv = T.new_tyvar () in
        match params with
        | [] -> trans_exp env body (Check tv)
        | _ ->
            let env' = List.fold_right (fun id -> E.extend id (ValBind tv)) params env in
            Lam (params, trans_exp env' body (Check tv)) )
  in
  trbnds bnds
