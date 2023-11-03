type value = Int of int | Closure of Code.t * env | Empty

and stack = value Stack.t

and env = value list

val code : Code.t ref

val env : env ref

val arg_stack : stack

val ret_stack : stack

val pop_code : unit -> Code.instr

val extend : value -> unit

val access : int -> value

val push_arg : value -> unit

val pop_arg : unit -> value

val push_ret : value -> unit

val pop_ret : unit -> value

val get_int : value -> int

val get_closure : value -> Code.t * env
