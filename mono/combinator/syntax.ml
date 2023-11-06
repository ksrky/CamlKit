include Core.Syntax

type frag = {name: string; params: id list; body: exp}

type frags = frag list

let ppr_frag {name; params; body} =
  name ^ "("
  ^ String.concat ", " (List.map Id.unique_name params)
  ^ ") = "
  ^ ppr_exp Id.unique_name body

let ppr_frags frags = String.concat "\n" (List.map ppr_frag frags)
