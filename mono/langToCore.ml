module L = Language.Syntax
module C = Core.Syntax

let rec trexp : L.exp -> C.exp = function
  | VarExp x -> Var x
  | NilExp -> Int 0
  | BoolExp true -> Int 1
  | BoolExp false -> Int 0
  | IntExp n -> Int n
  | AppExp _ as exp ->
      let fcn, args = appexp [] exp in
      App {fcn; args}
  | LamExp {vars; body} -> Lam {vars; body= trexp body}
  | OpExp {left; op; right} ->
      let oper =
        match op with
        | PlusOp -> "add"
        | MinusOp -> "sub"
        | TimesOp -> "mul"
        | DivideOp -> "div"
        | EqOp -> "eq"
        | NeqOp -> "ne"
        | LtOp -> "lt"
        | LeOp -> "le"
        | GtOp -> "gt"
        | GeOp -> "ge"
      in
      Prim {oper; args= [trexp left; trexp right]}
  | IfExp {cond; then_; else_} -> If {cond= trexp cond; then_= trexp then_; else_= trexp else_}
  | LetExp {bnds; body} ->
      let vars, bnds =
        List.split
          (List.map
             (fun {L.name; params; body} -> (name, C.Lam {vars= params; body= trexp body}))
             bnds )
      in
      Let {isrec= false; vars; bnds; body= trexp body}
  | LetrecExp {bnds; body} ->
      let vars, bnds =
        List.split
          (List.map
             (fun {L.name; params; body} -> (name, C.Lam {vars= params; body= trexp body}))
             bnds )
      in
      Let {isrec= true; vars; bnds; body= trexp body}

and appexp (acc : C.exp list) : L.exp -> C.exp * C.exp list = function
  | AppExp {fcn; arg} -> appexp (trexp arg :: acc) fcn
  | exp -> (trexp exp, List.rev acc)
