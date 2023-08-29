module A = AbsSyn
module I = IntSyn
module E = Env
module T = Types
module U = Unification

type expected = Infer of A.ty option ref | Check of A.ty

let check_type (ty : A.ty) : expected -> unit = function
  | Check ty' -> U.unify ty ty'
  | Infer ref -> ref := Some ty

let zonk_bndrs : I.binder list -> I.binder list = List.map (fun (id, ty) -> (id, T.zonk_type ty))

let rec zonk_expr : I.exp -> I.exp = function
  | Var (id, ty) -> Var (id, T.zonk_type ty)
  | App (fcn, args) -> App (zonk_expr fcn, List.map zonk_expr args)
  | Lam (bndrs, body) -> Lam (zonk_bndrs bndrs, zonk_expr body)
  | Prim (op, args) -> Prim (op, List.map zonk_expr args)
  | If (test, then_, else_) -> If (zonk_expr test, zonk_expr then_, zonk_expr else_)
  | Let (isrec, bndrs, defs, body) ->
      Let (isrec, zonk_bndrs bndrs, List.map zonk_expr defs, zonk_expr body)
  | Seq (e1, e2) -> Seq (zonk_expr e1, zonk_expr e2)
  | e -> e

let mkbndrs : Ident.t list -> A.ty list -> I.binders = List.map2 (fun x y -> (x, y))

let rec check_exp env exp ty : I.exp = trans_exp env exp (Check ty)

and infer_exp env exp : I.exp * A.ty =
  let ref = ref None in
  let exp' = trans_exp env exp (Infer ref) in
  (exp', T.zonk_type (Option.get !ref))

and trans_exp env (exp : A.exp) (exp_ty : expected) : I.exp =
  let rec trexp : A.exp * expected -> I.exp = function
    | VarExp id, exp_ty ->
        let ty = E.lookup_type id env in
        check_type ty exp_ty;
        Var (id, ty)
    | NilExp, _ -> Nil
    | BoolExp b, exp_ty ->
        check_type T.tBOOL exp_ty;
        if b then Int 1 else Int 0
    | IntExp x, exp_ty -> check_type T.tINT exp_ty; Int x
    | (AppExp _ as exp), exp_ty ->
        let rec loop acc (exp_ty' : expected) : A.exp -> I.exp = function
          | AppExp {fcn; arg} ->
              let arg_ty = T.new_tyvar () and res_ty = T.new_tyvar () in
              check_type res_ty exp_ty';
              loop (check_exp env arg arg_ty :: acc) (Check T.([arg_ty] --> res_ty)) fcn
          | VarExp id when Ident.name id = "read_int" ->
              check_type (E.lookup_type id env) exp_ty';
              Prim ("readi", acc)
          | VarExp id when Ident.name id = "print_int" ->
              check_type (E.lookup_type id env) exp_ty';
              Prim ("printi", acc)
          | VarExp id when Ident.name id = "array_make" ->
              check_type (E.lookup_type id env) exp_ty';
              Prim ("array_alloca", acc)
          | fcn ->  App (trexp (fcn, exp_ty'), acc)
        in
        loop [] exp_ty exp
    | LamExp {vars; body}, Check ty ->
        let arg_tys, body_ty = U.unify_funs vars ty in
        let env' = List.fold_right2 (fun id ty -> E.extend id (ValBind ty)) vars arg_tys env in
        let bndrs = mkbndrs vars arg_tys in
        let body' = check_exp env' body body_ty in
        Lam (bndrs, body')
    | LamExp {vars; body}, Infer ref ->
        let var_tys = List.map (fun _ -> T.new_tyvar ()) vars in
        let env' = List.fold_right2 (fun id ty -> E.extend id (ValBind ty)) vars var_tys env in
        let bndrs = mkbndrs vars var_tys in
        let body', body_ty = infer_exp env' body in
        ref := Some (List.fold_right (fun l r -> A.FunTy (l, r)) var_tys body_ty);
        Lam (bndrs, body')
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
        let bndrs, defs = List.split (trans_bnds env bnds) in
        Let (false, bndrs, defs, trans_exp env' body exp_ty)
    | LetrecExp {bnds; body}, exp_ty ->
        let env' =
          List.fold_right
            (fun {A.name; _} ->
              let tv = T.new_tyvar () in
              E.extend name (ValBind tv) )
            bnds env
        in
        let bndrs, defs = List.split (trans_bnds env' bnds) in
        Let (true, bndrs, defs, trans_exp env' body exp_ty)
    | SubscExp {arr; idx}, exp_ty ->
        check_type T.tINT exp_ty;
        (* tmp : array polymorphism *)
        Prim ("load", [Prim ("gep", [check_exp env arr T.tARRAY; check_exp env idx T.tINT])])
    | AssignExp {arr; idx; rhs}, exp_ty ->
        check_type T.tUNIT exp_ty;
        Prim
          ( "store"
          , [ Prim ("gep", [check_exp env arr T.tARRAY; check_exp env idx T.tINT])
            ; check_exp env rhs T.tINT ] )
    | SeqExp exps, exp_ty ->
        let rec loop = function
          | [] -> check_type T.tUNIT exp_ty; I.Int 0
          | [e] -> trans_exp env e exp_ty
          | e :: es ->
              let e' = check_exp env e T.tUNIT in
              Seq (e', loop es)
        in
        loop exps
  in
  try zonk_expr (trexp (exp, exp_ty))
  with E.Out_of_scope id ->
    ErrorMsg.error ("Unbound value " ^ Ident.name id);
    Nil

and trans_bnds env bnds : (I.binder * I.exp) list =
  List.map
    (fun {A.name; A.params; A.body} ->
      let body', ty =
        match params with
        | [] -> infer_exp env body
        | _ ->
            let bndrs = ref [] in
            let env' =
              List.fold_right
                (fun id ->
                  let tv = T.new_tyvar () in
                  bndrs := (id, tv) :: !bndrs;
                  E.extend id (ValBind tv) )
                params env
            in
            let body', ty = infer_exp env' body in
            (Lam (!bndrs, body'), ty)
      in
      ((name, ty), body') )
    bnds
