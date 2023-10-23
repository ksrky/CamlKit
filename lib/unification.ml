module T = Types

let rec unify ty1 ty2 : unit =
  match (ty1, ty2) with
  | T.NIL, _ | _, T.NIL -> ()
  | T.TyconTy {con= c1; args= a1}, T.TyconTy {con= c2; args= a2} when c1 = c2 ->
      List.iter2 unify a1 a2
  | T.FunTy (arg1, res1), T.FunTy (arg2, res2) -> unify arg1 arg2; unify res1 res2
  | T.MetaTy tv1, T.MetaTy tv2 when tv1.uniq = tv2.uniq -> ()
  | T.MetaTy tv1, _ -> unify_var tv1 ty2
  | _, T.MetaTy tv2 -> unify_var tv2 ty1
  | _, _ -> ErrorMsg.error ("Cannot unify types: " ^ T.ppr_ty ty1 ^ " with " ^ T.ppr_ty ty2)

and unify_var (tv1 : T.tyvar) (ty2 : T.ty) : unit =
  match (tv1.repres, ty2) with
  | Some ty1, _ -> unify ty1 ty2
  | None, T.MetaTy tv2 -> (
    match tv2.repres with Some ty2 -> unify (T.MetaTy tv1) ty2 | None -> tv1.repres <- Some ty2 )
  | _ ->
      occurs_check tv1 ty2;
      tv1.repres <- Some ty2

and occurs_check tv1 ty2 : unit =
  let tvs2 = Types.get_tyvars ty2 in
  if List.mem tv1 tvs2 then ErrorMsg.error ("Infinite type: " ^ T.ppr_ty ty2) else ()

let unify_fun = function
  | T.FunTy (arg_ty, res_ty) -> (arg_ty, res_ty)
  | ty ->
      let arg_ty = Types.new_tyvar () and res_ty = Types.new_tyvar () in
      unify ty (T.FunTy (arg_ty, res_ty));
      (arg_ty, res_ty)

let unify_funs vars ty =
  let rec loop acc = function
    | [], ty -> (acc, ty)
    | _ :: rest, ty ->
        let arg_ty, res_ty = unify_fun ty in
        loop (arg_ty :: acc) (rest, res_ty)
  in
  loop [] (vars, ty)
