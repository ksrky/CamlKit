type binding = ValBind

type env = binding Ident.Table.t

exception Out_of_scope of Ident.t

let empty : env = Ident.Table.empty

let extend (id : Ident.t) (bind : binding) (env : env) = Ident.Table.add id bind env

let lookup (id : Ident.t) (env : env) =
  match Ident.Table.find_opt id env with Some bind -> bind | None -> raise (Out_of_scope id)

let extend_list (binds : (Ident.t * binding) list) (env : env) =
  List.fold_right (fun (id, bind) -> extend id bind) binds env
