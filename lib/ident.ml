type t = string * int

let unique = ref (-1)

let h_size = 128

let hashtable : (string, int) Hashtbl.t = Hashtbl.create ~random:true h_size

let from_string (name : string) : t =
  match Hashtbl.find_opt hashtable name with
  | Some i -> (name, i)
  | None ->
      incr unique;
      Hashtbl.add hashtable name !unique;
      (name, !unique)

let to_string : t -> string = fst
