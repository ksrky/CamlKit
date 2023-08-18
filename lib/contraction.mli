val max_steps : int
val thresh_ratio : float

type varinfo =
  { mutable side_effect: bool
  ; mutable usecount: int
  ; mutable repres: IntSyn.exp
  ; mutable simple: bool
  ; mutable isrec: bool }

val hashtbl_find : Ident.t -> varinfo

val step : IntSyn.exp -> IntSyn.exp
val steps : int -> IntSyn.exp -> IntSyn.exp