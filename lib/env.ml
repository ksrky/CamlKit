type binding = ValBind

type env = (Ident.t * binding) list

exception Out_of_scope of Ident.t

let empty : env = []

let extend id bind env = (id, bind) :: env

let lookup id env =
  match List.assoc_opt id env with Some bind -> bind | None -> raise (Out_of_scope id)

let extend_list binds env = binds @ env
