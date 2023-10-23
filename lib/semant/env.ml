type binding = ValBind of Types.ty

type env = binding Id.Table.t

exception Out_of_scope of Id.t

let empty : env = Id.Table.empty

let entry : env =
  List.fold_right
    (fun (k, b) -> Id.Table.add k b)
    [ (Scoping.get_reservedid "print_int", ValBind Types.([tINT] --> tUNIT))
    ; (Scoping.get_reservedid "read_int", ValBind Types.([tUNIT] --> tINT)) ]
    empty

let extend (id : Id.t) (bind : binding) (env : env) = Id.Table.add id bind env

let extend_list (binds : (Id.t * binding) list) (env : env) =
  List.fold_right (fun (id, bind) -> extend id bind) binds env

let lookup (id : Id.t) (env : env) =
  match Id.Table.find_opt id env with Some bind -> bind | None -> raise (Out_of_scope id)

let lookup_type id env : Types.ty = match lookup id env with ValBind ty -> ty
