type t

val from_string : string -> t

val name : t -> string

val unique : t -> int

val unique_name : t -> string

val reassign_unique : t -> t

val fresh : unit -> t

module Table : sig
  type key = t

  type 'a t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
end

type 'a table = 'a Table.t

val pp_print_id : Format.formatter -> t -> unit
