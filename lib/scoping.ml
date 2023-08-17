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

let scoping_def (sc : scope) : A.def -> A.def * scope = function
  | LetDef bnds ->
      let sc' = extend_list (List.map (fun (d : A.bnd) -> d.name) bnds) sc in
      (LetDef (scoping_bnds sc bnds), sc')
  | LetrecDef bnds ->
      let sc' = extend_list (List.map (fun (d : A.bnd) -> d.name) bnds) sc in
      (LetrecDef (scoping_bnds sc' bnds), sc')

let rec scoping_defs (sc : scope) : A.def list -> A.def list = function
  | [] -> []
  | def :: rest ->
      let def', sc' = scoping_def sc def in
      def' :: scoping_defs sc' rest
