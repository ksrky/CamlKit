open Language.Syntax

let ( --> ) : ty list -> ty -> ty = List.fold_right (fun ty1 ty2 -> FunTy (ty1, ty2))

let rec zonk_type : ty -> ty = function
  | NilTy -> NilTy
  | IntTy -> IntTy
  | BoolTy -> BoolTy
  | FunTy (fcn, arg) -> FunTy (zonk_type fcn, zonk_type arg)
  | MetaTy tv -> (
    match tv.repres with
    | None -> MetaTy tv
    | Some ty ->
        let ty' = zonk_type ty in
        tv.repres <- Some ty';
        ty' )

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

let rec get_tyvars ty : tyvar list = metatvs (zonk_type ty)
