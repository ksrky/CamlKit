module I = IntSyn
module T = Types

let openv : (string * I.ty) list =
  [ ("add", T.([tINT; tINT] --> tINT)); ("sub", T.([tINT; tINT] --> tINT))
  ; ("mul", T.([tINT; tINT] --> tINT)); ("div", T.([tINT; tINT] --> tINT))
  ; ("eq", T.([tINT; tINT] --> tBOOL)); ("ne", T.([tINT; tINT] --> tBOOL))
  ; ("lt", T.([tINT; tINT] --> tBOOL)); ("le", T.([tINT; tINT] --> tBOOL))
  ; ("gt", T.([tINT; tINT] --> tBOOL)); ("ge", T.([tINT; tINT] --> tBOOL))
  ; ("printi", T.([tINT] --> tUNIT)); ("readi", T.([tUNIT] --> tINT)) ]

let rec tyeqv ty1 ty2 =
  match (ty1, ty2) with
  | AbsSyn.NIL, _ -> ()
  | _, AbsSyn.NIL -> ()
  | AbsSyn.TyconTy {con= c1; args= a1}, AbsSyn.TyconTy {con= c2; args= a2} when c1 = c2 ->
      List.iter2 tyeqv a1 a2
  | AbsSyn.FunTy (a1, b1), AbsSyn.FunTy (a2, b2) -> tyeqv a1 a2; tyeqv b1 b2
  | _ -> ErrorMsg.impossible ("Types not match: " ^ I.ppr_ty ty1 ^ " with " ^ I.ppr_ty ty2)

let rec check_app fcn_ty = function
  | [] -> fcn_ty
  | arg_ty :: rest -> (
    match fcn_ty with
    | AbsSyn.FunTy (arg_ty', res_ty) -> tyeqv arg_ty arg_ty'; check_app res_ty rest
    | _ -> ErrorMsg.impossible "Not function type" )

let rec type_of (env : (Ident.t * I.ty) list) : I.exp -> I.ty = function
  | Int _ -> T.tINT
  | Nil -> T.tNIL
  | Var (id, ty) ->
      (try tyeqv ty (List.assoc id env) with Not_found -> ());
      ty
  | App (fcn, args) ->
      let fcn_ty = type_of env fcn in
      let arg_tys = List.map (type_of env) args in
      check_app fcn_ty arg_tys
  | Lam (vars, body) ->
      let var_tys = List.map snd vars in
      let res_ty = type_of env body in
      T.(var_tys --> res_ty)
  | Prim (op, args) ->
      let op_ty = List.assoc op openv in
      let arg_tys = List.map (type_of env) args in
      check_app op_ty arg_tys
  | If (test, then_, else_) ->
      let test_ty = type_of env test in
      let then_ty = type_of env then_ in
      let else_ty = type_of env else_ in
      tyeqv test_ty T.tINT; tyeqv then_ty else_ty; then_ty
  | Let (_, bndrs, defs, body) ->
      let var_tys = List.map snd bndrs in
      List.iter2 (fun (_, ty) e -> tyeqv ty (type_of env e)) bndrs defs;
      let env' = List.map2 (fun (id, _) ty -> (id, ty)) bndrs var_tys @ env in
      type_of env' body
  | Seq (exp, rest) ->
      tyeqv T.tUNIT (type_of env exp);
      type_of env rest
