module A = AbsSyn

let rec unify ty1 ty2 : unit =
  match (ty1, ty2) with
  | A.NIL, _ | _, A.NIL -> ()
  | A.TyconTy {con= c1; args= a1}, A.TyconTy {con= c2; args= a2} when c1 = c2 ->
      List.iter2 unify a1 a2
  | A.FunTy (arg1, res1), A.FunTy (arg2, res2) -> unify arg1 arg2; unify res1 res2
  | A.MetaTy tv1, A.MetaTy tv2 when tv1.uniq = tv2.uniq -> ()
  | A.MetaTy tv1, _ -> unify_var tv1 ty2
  | _, A.MetaTy tv2 -> unify_var tv2 ty1
  | _, _ -> ErrorMsg.error ("Cannot unify types: " ^ A.ppr_ty ty1 ^ " with " ^ A.ppr_ty ty2)

and unify_var (tv1 : A.tyvar) (ty2 : A.ty) : unit =
  match (tv1, ty2) with
  | _, A.MetaTy tv2 -> (
    match (tv1.repres, tv2.repres) with
    | Some ty1, _ -> unify ty1 ty2
    | None, Some ty2' -> unify (A.MetaTy tv1) ty2'
    | None, None -> tv1.repres <- Some ty2 )
  | _ ->
      occurs_check tv1 ty2;
      tv1.repres <- Some ty2

and occurs_check tv1 ty2 : unit =
  let tvs2 = Types.get_tyvars ty2 in
  if List.mem tv1 tvs2 then ErrorMsg.error ("Infinite type: " ^ A.ppr_ty ty2) else ()

let unify_fun = function
  | A.FunTy (arg_ty, res_ty) -> (arg_ty, res_ty)
  | ty ->
      let arg_ty = Types.new_tyvar () and res_ty = Types.new_tyvar () in
      unify ty (A.FunTy (arg_ty, res_ty));
      (arg_ty, res_ty)

let unify_funs vars ty =
  let rec loop acc = function
    | [], ty -> (acc, ty)
    | _ :: rest, ty ->
        let arg_ty, res_ty = unify_fun ty in
        loop (arg_ty :: acc) (rest, res_ty)
  in
  loop [] (vars, ty)
