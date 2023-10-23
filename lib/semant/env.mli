type binding = ValBind of Types.ty

type env

exception Out_of_scope of Id.t

val empty : env

val entry : env

val extend : Id.t -> binding -> env -> env

val lookup : Id.t -> env -> binding

val lookup_type : Id.t -> env -> Types.ty

val extend_list : (Id.t * binding) list -> env -> env
