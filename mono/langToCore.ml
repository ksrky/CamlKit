module L = Language.Syntax
module C = Core.Syntax

let rec trexp : L.exp -> C.exp = function
  | VarExp x -> Var x
  | NilExp -> Int 0
  | BoolExp true -> Int 1
  | BoolExp false -> Int 0
  | IntExp n -> Int n
  (* | AppExp _ as exp ->
      let fcn, args = appexp [] exp in
      App {fcn; args} *)
  | AppExp {fcn; arg} -> App {fcn= trexp fcn; args= [trexp arg]}
  | LamExp {vars; body} -> C.lams vars (trexp body)
  | OpExp {left; op; right} ->
      let oper : C.oper =
        match op with
        | PlusOp -> Add
        | MinusOp -> Sub
        | TimesOp -> Mul
        | DivideOp -> Div
        | EqOp -> Eq
        | NeqOp -> Ne
        | LtOp -> Lt
        | LeOp -> Le
        | GtOp -> Gt
        | GeOp -> Ge
      in
      Prim {oper; args= [trexp left; trexp right]}
  | IfExp {cond; then_; else_} ->
      If {cond= trexp cond; then_= trexp then_; else_= trexp else_}
  | LetExp {bnds; body} ->
      let vars, bnds =
        List.split
          (List.map
             (fun (L.Bind {name; params; body}) ->
               (name, C.lams params (trexp body)) )
             bnds )
      in
      Let {isrec= false; vars; bnds; body= trexp body}
  | LetrecExp {bnds; body} ->
      let vars, bnds =
        List.split
          (List.map
             (fun (L.Bind {name; params; body}) ->
               (name, C.lams params (trexp body)) )
             bnds )
      in
      Let {isrec= true; vars; bnds; body= trexp body}

and appexp (acc : C.exp list) : L.exp -> C.exp * C.exp list = function
  | AppExp {fcn; arg} -> appexp (trexp arg :: acc) fcn
  | exp -> (trexp exp, acc)
