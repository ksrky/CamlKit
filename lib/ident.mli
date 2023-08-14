type t
val from_string : string -> t
val name  : t -> string
val unique : t -> int
val fresh : unit -> t

module Table : sig
  type key = t
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
end