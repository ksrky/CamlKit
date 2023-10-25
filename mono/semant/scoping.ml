module L = Language.Syntax

type scope = (string * Id.t) list

let empty : scope = []

let initial : scope =
  List.map (fun s -> (s, Id.from_string s)) ["read_int"; "print_int"; "int"; "bool"; "unit"]

let get_reservedid (name : string) : Id.t =
  match List.assoc_opt name initial with
  | Some id -> id
  | None -> Error.impossible ("Not found " ^ name)

let extend (id : Id.t) (sc : scope) : scope = (Id.name id, id) :: sc

let extend_list : Id.t list -> scope -> scope = List.fold_right extend

let scoping id sc =
  match List.assoc_opt (Id.name id) sc with
  | Some id' -> id'
  | None ->
      Error.error ("Not in scope " ^ Id.name id);
      id

let rec scoping_exp (sc : scope) : L.exp -> L.exp =
  let rec scexp : L.exp -> L.exp = function
    | VarExp id -> VarExp (scoping id sc)
    | NilExp -> NilExp
    | BoolExp b -> BoolExp b
    | IntExp i -> IntExp i
    | AppExp {fcn; arg} -> AppExp {fcn= scexp fcn; arg= scexp arg}
    | LamExp {vars; body} ->
        let sc' = extend_list vars sc in
        LamExp {vars; body= scoping_exp sc' body}
    | OpExp {left; op; right} -> OpExp {left= scexp left; op; right= scexp right}
    | IfExp {cond; then_; else_} -> IfExp {cond= scexp cond; then_= scexp then_; else_= scexp else_}
    | LetExp {bnds; body} ->
        let sc' = extend_list (List.map (fun (L.Bind d) -> d.name) bnds) sc in
        LetExp {bnds= scoping_bnds sc bnds; body= scoping_exp sc' body}
    | LetrecExp {bnds; body} ->
        let sc' = extend_list (List.map (fun (L.Bind d) -> d.name) bnds) sc in
        LetrecExp {bnds= scoping_bnds sc' bnds; body= scoping_exp sc' body}
  in
  scexp

and scoping_bnds (sc : scope) (bnds : L.bnd list) : L.bnd list =
  let scbnd (Bind {name; params; body} : L.bnd) : L.bnd =
    let sc' = extend_list params sc in
    Bind {name; params; body= scoping_exp sc' body}
  in
  List.map scbnd bnds
