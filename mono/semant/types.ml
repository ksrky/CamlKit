open Abstract.Syntax

let ( --> ) : ty list -> ty -> ty =
  List.fold_right (fun ty1 ty2 -> FunTy (ty1, ty2))

let rec zonk_ty : ty -> ty = function
  | NilTy -> NilTy
  | IntTy -> IntTy
  | BoolTy -> BoolTy
  | FunTy (fcn, arg) -> FunTy (zonk_ty fcn, zonk_ty arg)
  | MetaTy tv -> (
    match tv.repres with
    | None -> MetaTy tv
    | Some ty ->
        let ty' = zonk_ty ty in
        tv.repres <- Some ty';
        ty' )

let rec zonk_aexp : aexp -> aexp = function
  | VarAExp x -> VarAExp x
  | IntAExp i -> IntAExp i
  | BoolAExp b -> BoolAExp b
  | NilAExp -> NilAExp
  | AppAExp {fcn= fcn, fcn_ty; arg= arg, arg_ty} ->
      AppAExp
        { fcn= (zonk_aexp fcn, zonk_ty fcn_ty)
        ; arg= (zonk_aexp arg, zonk_ty arg_ty) }
  | LamAExp {vars; body= body, body_ty} ->
      LamAExp {vars; body= (zonk_aexp body, zonk_ty body_ty)}
  | OpAExp {left= left, left_ty; op; right= right, right_ty} ->
      OpAExp
        { left= (zonk_aexp left, zonk_ty left_ty)
        ; op
        ; right= (zonk_aexp right, zonk_ty right_ty) }
  | IfAExp {cond= cond, cond_ty; then_= then_, then_ty; else_= else_, else_ty}
    ->
      IfAExp
        { cond= (zonk_aexp cond, zonk_ty cond_ty)
        ; then_= (zonk_aexp then_, zonk_ty then_ty)
        ; else_= (zonk_aexp else_, zonk_ty else_ty) }
  | LetAExp {bnds; body= body, body_ty} ->
      LetAExp
        {bnds= List.map zonk_abnd bnds; body= (zonk_aexp body, zonk_ty body_ty)}
  | LetrecAExp {bnds; body= body, body_ty} ->
      LetrecAExp
        {bnds= List.map zonk_abnd bnds; body= (zonk_aexp body, zonk_ty body_ty)}

and zonk_abnd (ABind {name; params; body= body, body_ty}) : abnd =
  let params' = List.map (fun (x, ty) -> (x, zonk_ty ty)) params in
  ABind {name; params= params'; body= (zonk_aexp body, zonk_ty body_ty)}

let unique = ref (-1)

let new_tyvar () : ty =
  incr unique;
  MetaTy {uniq= !unique; repres= None}

let rec metatvs : ty -> tyvar list = function
  | NilTy -> []
  | IntTy -> []
  | BoolTy -> []
  | FunTy (fcn, arg) -> metatvs fcn @ metatvs arg
  | MetaTy tv -> [tv]

let rec get_tyvars ty : tyvar list = metatvs (zonk_ty ty)
