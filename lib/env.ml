type binding = ValBind of AbsSyn.ty

type env = binding Ident.Table.t

exception Out_of_scope of Ident.t

let empty : env = Ident.Table.empty

let entry : env =
  List.fold_right
    (fun (k, b) -> Ident.Table.add k b)
    [ (Scoping.get_reservedid "print_int", ValBind (AbsSyn.FunTy (Types.tINT, Types.tUNIT)))
    ; (Scoping.get_reservedid "read_int", ValBind (AbsSyn.FunTy (Types.tUNIT, Types.tINT)))
    ; ( Scoping.get_reservedid "array_make"
      , ValBind (AbsSyn.FunTy (Types.tINT, AbsSyn.FunTy (Types.tINT, Types.tARRAY))) ) ]
    empty

let extend (id : Ident.t) (bind : binding) (env : env) = Ident.Table.add id bind env

let extend_list (binds : (Ident.t * binding) list) (env : env) =
  List.fold_right (fun (id, bind) -> extend id bind) binds env

let lookup (id : Ident.t) (env : env) =
  match Ident.Table.find_opt id env with Some bind -> bind | None -> raise (Out_of_scope id)

let lookup_type id env : AbsSyn.ty = match lookup id env with ValBind ty -> ty
