type scope = (string * Ident.t) list

module A = AbsSyn

let empty : scope = []

let initial : scope = List.map (fun s -> (s, Ident.from_string s)) ["read_int"; "print_int"]

let extend (id : Ident.t) (sc : scope) : scope = (Ident.name id, id) :: sc

let extend_list : Ident.t list -> scope -> scope = List.fold_right extend

let scoping id sc =
  match List.assoc_opt (Ident.name id) sc with
  | Some id' -> id'
  | None ->
      ErrorMsg.error ("Not in scope " ^ Ident.name id);
      id

let rec scoping_exp (sc : scope) : A.exp -> A.exp =
  let rec scexp : A.exp -> A.exp = function
    | VarExp id -> VarExp (scoping id sc)
    | NilExp -> NilExp
    | IntExp i -> IntExp i
    | AppExp {fcn; arg} -> AppExp {fcn= scexp fcn; arg= scexp arg}
    | LamExp {vars; body} -> LamExp {vars; body= scexp body}
    | OpExp {left; oper; right} -> OpExp {left= scexp left; oper; right= scexp right}
    | IfExp {test; then_; else_} -> IfExp {test= scexp test; then_= scexp then_; else_= scexp else_}
    | LetExp {decs; body} ->
        let sc' = extend_list (List.map (fun (d : A.dec) -> d.name) decs) sc in
        LetExp {decs= scoping_decs sc decs; body= scoping_exp sc' body}
    | LetrecExp {decs; body} ->
        let sc' = extend_list (List.map (fun (d : A.dec) -> d.name) decs) sc in
        LetrecExp {decs= scoping_decs sc' decs; body= scoping_exp sc' body}
  in
  scexp

and scoping_decs (sc : scope) (decs : A.dec list) : A.dec list =
  let scdec ({name; params; body} : A.dec) : A.dec =
    let sc' = extend_list params sc in
    {name; params; body= scoping_exp sc' body}
  in
  List.map scdec decs
