type binding = ValBind
type env

exception Out_of_scope of Ident.t

val empty : env
val extend : Ident.t -> binding ->  env -> env
val lookup : Ident.t ->  env -> binding
val extend_list : (Ident.t * binding) list ->  env -> env