val s : int ref
val e : int ref
val c : int ref
val d : int ref

val init : unit -> unit
val make_int : int -> int
val get_int : int -> int
val make_cons : int -> int -> int
val get_cons : int -> int * int
val push : int -> int ref -> unit
val pop : int ref -> int
val atom : int -> int
val car : int -> int
val cdr : int -> int
val locate : int -> int -> int
val binop : int ref -> (int -> int -> int) -> unit
val rplaca : int -> int -> int
val store : int -> int -> unit