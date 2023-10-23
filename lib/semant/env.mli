module Ident = Language.Ident

type binding = ValBind of Types.ty
type env

exception Out_of_scope of Ident.t

val empty : env
val entry : env
val extend : Ident.t -> binding ->  env -> env
val lookup : Ident.t ->  env -> binding
val lookup_type : Ident.t ->  env -> Types.ty
val extend_list : (Ident.t * binding) list ->  env -> env