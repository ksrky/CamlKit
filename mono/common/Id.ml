type t = string * int

let uniq = ref (-1)

let from_string (name : string) : t = incr uniq; (name, !uniq)

let name : t -> string = fst

let unique : t -> int = snd

let unique_name (id : t) : string = fst id ^ "_" ^ string_of_int (snd id)

let fresh () = from_string "a"

module Table = Map.Make (struct
  type nonrec t = t

  let compare (_, u1) (_, u2) = compare u1 u2
end)
