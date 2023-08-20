module A = AbsSyn
module I = IntSyn
module E = Env
module Tc = TypeCheck

type expected = Infer of A.ty option ref | Check of A.ty

let check_type (ty : A.ty) : expected -> unit = function
  | Check ty' -> Tc.unify ty ty'
  | Infer ref -> ref := Some ty

let rec trans_exp env exp exp_ty =
  let rec check_exp exp ty : I.exp = trexp (exp, Check ty)
  and infer_exp exp : I.exp * A.ty =
    let ref = ref None in
    let exp' = trexp (exp, Infer ref) in
    (exp', Option.get !ref)
  and trexp : A.exp * expected -> I.exp = function
    | VarExp id, exp_ty ->
        check_type (E.lookup_type id env) exp_ty;
        Var id
    | NilExp, exp_ty -> Nil
    | BoolExp b, exp_ty -> if b then Int 1 else Int 0
    | IntExp x, exp_ty -> Int x
    | (AppExp _ as exp), exp_ty ->
        let rec loop acc : A.exp -> I.exp = function
          | AppExp {fcn; arg} -> loop (trexp arg :: acc) fcn
          | VarExp id when Ident.name id = "read_int" -> Builtin ("readi", acc)
          | VarExp id when Ident.name id = "print_int" -> Builtin ("printi", acc)
          | fcn -> App (trexp fcn, acc)
        in
        loop [] exp
    | LamExp {vars; body}, Check ty ->
        let arg_tys, body_ty = Tc.unifyFun vars ty in
        let env' = List.fold_right2 (fun id ty -> E.extend id (ValBind ty)) vars arg_tys env in
        let body' = trans_exp env' body body_ty in
        Lam (vars, body')
    | LamExp {vars; body}, Infer ref ->
        let var_tys = List.map (fun _ -> A.MetaTy {uniq= 0; repres= None}) vars in
        let env' = List.fold_right2 (fun id ty -> E.extend id (ValBind ty)) vars var_tys env in
        let body', body_ty = infer_exp body in
        ref := Some body_ty;
        Lam (vars, body')
    | OpExp {left; op= PlusOp; right}, exp_ty ->
        check_type INT exp_ty;
        Builtin ("add", [check_exp left INT; check_exp right INT])
    | OpExp {left; op= MinusOp; right}, exp_ty ->
        check_type INT exp_ty;
        Builtin ("sub", [check_exp left INT; check_exp right INT])
    | OpExp {left; op= TimesOp; right}, exp_ty ->
        check_type INT exp_ty;
        Builtin ("mul", [check_exp left INT; check_exp right INT])
    | OpExp {left; op= DivideOp; right}, exp_ty ->
        check_type INT exp_ty;
        Builtin ("div", [check_exp left INT; check_exp right INT])
    | OpExp {left; op= EqOp; right}, exp_ty ->
        check_type BOOL exp_ty;
        Builtin ("eq", [check_exp left INT; check_exp right INT])
    | OpExp {left; op= NeqOp; right}, exp_ty ->
        check_type BOOL exp_ty;
        Builtin ("ne", [check_exp left INT; check_exp right INT])
    | OpExp {left; op= LtOp; right}, exp_ty ->
        check_type BOOL exp_ty;
        Builtin ("lt", [check_exp left INT; check_exp right INT])
    | OpExp {left; op= LeOp; right}, exp_ty ->
        check_type BOOL exp_ty;
        Builtin ("le", [check_exp left INT; check_exp right INT])
    | OpExp {left; op= GtOp; right}, exp_ty ->
        check_type BOOL exp_ty;
        Builtin ("lt", [check_exp right INT; check_exp left INT])
    | OpExp {left; op= GeOp; right}, exp_ty ->
        check_type BOOL exp_ty;
        Builtin ("le", [check_exp right INT; check_exp left INT])
    | IfExp {test; then_; else_}, exp_ty ->
        If (check_exp test INT, trexp (then_, exp_ty), trexp (else_, exp_ty))
    | LetExp {bnds; body}, exp_ty ->
        let env' = List.fold_right (fun {A.name; _} -> E.extend name ValBind) bnds env in
        Let
          ( false
          , List.map (fun {A.name; _} -> name) bnds
          , trans_bnds env bnds
          , trans_exp env' body exp_ty )
    | LetrecExp {bnds; body}, exp_ty ->
        let env' = List.fold_right (fun {A.name; _} -> E.extend name ValBind) bnds env in
        Let
          ( true
          , List.map (fun {A.name; _} -> name) bnds
          , trans_bnds env' bnds
          , trans_exp env' body exp_ty )
  in
  try trexp (exp, exp_ty)
  with E.Out_of_scope id ->
    ErrorMsg.error ("Unbound value " ^ Ident.name id);
    Nil

and trans_bnds env bnds =
  let trbnds =
    List.map (fun {A.params; A.body; _} ->
        match params with
        | [] -> trans_exp env body
        | _ ->
            let env' = List.fold_right (fun id -> E.extend id ValBind) params env in
            Lam (params, trans_exp env' body) )
  in
  trbnds bnds
