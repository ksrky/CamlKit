(** Abstract types of identifier *)
type t = string * int

(** Unique id of identifier *)
let uniq = ref (-1)

(** Construct an identifier from string *)
let from_string (name : string) : t = incr uniq; (name, !uniq)

(** Get actual string of an identifier *)
let name : t -> string = fst

(** Get unique id of an identifier *)
let unique : t -> int = snd

(** Generate unique string of an identifier *)
let unique_name (id : t) : string = fst id ^ "_" ^ string_of_int (snd id)

module Table = struct
  module M = Map.Make (struct
    type nonrec t = t

    let compare (_, u1) (_, u2) = compare u1 u2
  end)

  include M

  let find id t =
    try M.find id t
    with Not_found ->
      Format.eprintf "Not found: %s@." (unique_name id);
      ErrorMsg.bug ()

  let add_list l t = M.add_seq (List.to_seq l) t
end

(** Map type using identifiers as the keys *)
type 'a table = 'a Table.t

(** Pretty print name of an identifier *)
let pp_print_id ppf id = Format.fprintf ppf "%s" (name id)
