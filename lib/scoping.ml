type scope = (string * Ident.t) list

module A = AbsSyn

let empty : scope = []

let initial : scope =
  List.map (fun s -> (s, Ident.from_string s)) ["read_int"; "print_int"; "int"; "bool"; "unit"; "Array.create"]

let get_reservedid (name : string) : Ident.t =
  match List.assoc_opt name initial with
  | Some id -> id
  | None -> ErrorMsg.impossible ("Not found " ^ name)

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
    | BoolExp b -> BoolExp b
    | IntExp i -> IntExp i
    | AppExp {fcn; arg} -> AppExp {fcn= scexp fcn; arg= scexp arg}
    | LamExp {vars; body} ->
        let sc' = extend_list vars sc in
        LamExp {vars; body= scoping_exp sc' body}
    | OpExp {left; op; right} -> OpExp {left= scexp left; op; right= scexp right}
    | IfExp {test; then_; else_} -> IfExp {test= scexp test; then_= scexp then_; else_= scexp else_}
    | LetExp {bnds; body} ->
        let sc' = extend_list (List.map (fun (d : A.bnd) -> d.name) bnds) sc in
        LetExp {bnds= scoping_bnds sc bnds; body= scoping_exp sc' body}
    | LetrecExp {bnds; body} ->
        let sc' = extend_list (List.map (fun (d : A.bnd) -> d.name) bnds) sc in
        LetrecExp {bnds= scoping_bnds sc' bnds; body= scoping_exp sc' body}
  in
  scexp

and scoping_bnds (sc : scope) (bnds : A.bnd list) : A.bnd list =
  let scbnd ({name; params; body} : A.bnd) : A.bnd =
    let sc' = extend_list params sc in
    {name; params; body= scoping_exp sc' body}
  in
  List.map scbnd bnds
