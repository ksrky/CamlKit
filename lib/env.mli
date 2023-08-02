type binding = ValBind
type env

exception ScopeError of Ident.t

val empty : env
val extend : Ident.t -> binding ->  env -> env
val lookup : Ident.t ->  env -> binding
val extend_list : (Ident.t * binding) list ->  env -> env