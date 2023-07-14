val s : int ref

val e : int ref

val c : int ref

val d : int ref

val init : unit -> unit

val makeInt : int -> int

val getInt : int -> int

val makeCons : int -> int -> int

val getCons : int -> int * int

val push : int -> int ref -> unit

val pop : int ref -> int

val atom : int -> int

val car : int -> int

val cdr : int -> int

val locate : int -> int -> int

val binOp : int ref -> (int -> int -> int) -> unit

val rplaca : int -> int -> int

val alloc : int -> int -> int
