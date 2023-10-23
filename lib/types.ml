type ty = NIL | TyconTy of {con: Ident.t; args: ty list} | FunTy of ty * ty | MetaTy of tyvar

and tyvar = {uniq: int; mutable repres: ty option}

let ppr_ty (ty : ty) : string =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pretty ctx = function
    | NIL -> "nil"
    | TyconTy {con; args} ->
        if args = [] then Ident.name con
        else parens ctx 1 (String.concat " " (Ident.name con :: List.map (pretty 2) args))
    | FunTy (fcn, arg) -> parens ctx 0 (pretty 1 fcn ^ " -> " ^ pretty 0 arg)
    | MetaTy tv -> "$" ^ string_of_int tv.uniq
  in
  pretty 0 ty

let tNIL = NIL

let tINT = TyconTy {con= Scoping.get_reservedid "int"; args= []}

let tBOOL = TyconTy {con= Scoping.get_reservedid "bool"; args= []}

let tUNIT = TyconTy {con= Scoping.get_reservedid "unit"; args= []}

let ( --> ) = List.fold_right (fun ty1 ty2 -> FunTy (ty1, ty2))

let rec zonk_type : ty -> ty = function
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
  MetaTy {uniq= !unique; repres= None}

let rec metatvs : ty -> tyvar list = function
  | TyconTy {args; _} -> List.concat_map metatvs args
  | FunTy (fcn, arg) -> metatvs fcn @ metatvs arg
  | MetaTy tv -> [tv]

let rec get_tyvars ty = metatvs (zonk_type ty)
