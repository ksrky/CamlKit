let tINT = AbsSyn.TyconTy {con= Scoping.get_reservedid "int"; args= []}

let tBOOL = AbsSyn.TyconTy {con= Scoping.get_reservedid "bool"; args= []}

let tUNIT = AbsSyn.TyconTy {con= Scoping.get_reservedid "unit"; args= []}

let rec zonk_type : AbsSyn.ty -> AbsSyn.ty = function
  | TyconTy {con; args} -> TyconTy {con; args= List.map zonk_type args}
  | FunTy (fcn, arg) -> FunTy (zonk_type fcn, zonk_type arg)
  | MetaTy tv -> (
    match tv.repres with
    | None -> MetaTy tv
    | Some ty ->
        let ty' = zonk_type ty in
        tv.repres <- Some ty';
        ty' )

let unique = ref (-1)

let new_tyvar () =
  incr unique;
  AbsSyn.MetaTy {uniq= !unique; repres= None}

let rec metatvs : AbsSyn.ty -> AbsSyn.tyvar list = function
  | TyconTy {args; _} -> List.concat_map metatvs args
  | FunTy (fcn, arg) -> metatvs fcn @ metatvs arg
  | MetaTy tv -> [tv]

let rec get_tyvars ty = metatvs (zonk_type ty)
