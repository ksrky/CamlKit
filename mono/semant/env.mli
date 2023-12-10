type binding = ValBind of Abstract.Syntax.ty

type env

exception Out_of_scope of Id.t

val empty : env

val extend : Id.t -> binding -> env -> env

val extend_list : (Id.t * binding) list -> env -> env

val extend_vals : Id.t list -> Abstract.Syntax.ty list -> env -> env

val lookup : Id.t -> env -> binding

val lookup_type : Id.t -> env -> Abstract.Syntax.ty
