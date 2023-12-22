type binding = ValBind of Abstract.Syntax.ty

type env = binding Id.table

exception Out_of_scope of Id.t

let empty : env = Id.Table.empty

let extend : Id.t -> binding -> env -> env = Id.Table.add

let extend_list = List.fold_right (fun (id, bind) -> extend id bind)

let extend_vals = List.fold_right2 (fun id ty -> extend id (ValBind ty))

let lookup (id : Id.t) (env : env) : binding =
  match Id.Table.find_opt id env with
  | Some bind -> bind
  | None -> raise (Out_of_scope id)

let lookup_type id env : Abstract.Syntax.ty =
  match lookup id env with ValBind ty -> ty
