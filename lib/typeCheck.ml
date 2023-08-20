module A = AbsSyn

let rec unify ty1 ty2 =
  match (ty1, ty2) with
  | A.INT, A.INT -> ()
  | A.BOOL, A.BOOL -> ()
  | A.FunTy (arg1, res1), A.FunTy (arg2, res2) -> unify arg1 arg2; unify res1 res2
  | A.MetaTy tv1, A.MetaTy tv2 when tv1.uniq = tv2.uniq -> ()
  | _, _ -> ErrorMsg.error "Cannot unify types"

let unifyFun vars ty =
  let rec loop acc = function
    | [], ty -> (acc, ty)
    | _ :: rest, A.FunTy (arg_ty, res_ty) -> loop (arg_ty :: acc) (rest, res_ty)
  in
  loop [] (vars, ty)
