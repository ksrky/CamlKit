type binding = ValBind of Language.Syntax.ty

type env

exception Out_of_scope of Id.t

val empty : env

val extend : Id.t -> binding -> env -> env

val lookup : Id.t -> env -> binding

val lookup_type : Id.t -> env -> Language.Syntax.ty

val extend_list : (Id.t * binding) list -> env -> env
