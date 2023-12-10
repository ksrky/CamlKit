module A = Abstract.Syntax
module T = Types

let rec unify (ty1 : A.ty) (ty2 : A.ty) : unit =
  match (ty1, ty2) with
  | NilTy, _ | _, NilTy -> ()
  | IntTy, IntTy -> ()
  | BoolTy, BoolTy -> ()
  | FunTy (arg1, res1), FunTy (arg2, res2) -> unify arg1 arg2; unify res1 res2
  | MetaTy tv1, MetaTy tv2 when tv1.uniq = tv2.uniq -> ()
  | MetaTy tv1, _ -> unify_var tv1 ty2
  | _, MetaTy tv2 -> unify_var tv2 ty1
  | _, _ ->
      Error.error
        ("Cannot unify types: " ^ A.ppr_ty ty1 ^ " with " ^ A.ppr_ty ty2)

and unify_var (tv1 : A.tyvar) (ty2 : A.ty) : unit =
  match (tv1.repres, ty2) with
  | Some ty1, _ -> unify ty1 ty2
  | None, MetaTy tv2 -> (
    match tv2.repres with
    | Some ty2 -> unify (MetaTy tv1) ty2
    | None -> tv1.repres <- Some ty2 )
  | _ ->
      occurs_check tv1 ty2;
      tv1.repres <- Some ty2

and occurs_check (tv1 : A.tyvar) (ty2 : A.ty) : unit =
  let tvs2 = T.get_tyvars ty2 in
  if List.mem tv1 tvs2 then Error.error ("Infinite type: " ^ A.ppr_ty ty2)
  else ()

let unify_fun : A.ty -> A.ty * A.ty = function
  | FunTy (arg_ty, res_ty) -> (arg_ty, res_ty)
  | ty ->
      let arg_ty = T.new_tyvar () and res_ty = T.new_tyvar () in
      unify ty (FunTy (arg_ty, res_ty));
      (arg_ty, res_ty)

let unify_funs vars ty : A.ty list * A.ty =
  let rec loop acc = function
    | [], ty -> (List.rev acc, ty)
    | _ :: rest, ty ->
        let arg_ty, res_ty = unify_fun ty in
        loop (arg_ty :: acc) (rest, res_ty)
  in
  loop [] (vars, ty)
