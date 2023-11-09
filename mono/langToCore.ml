module L = Language.Syntax
module C = Core.Syntax

let rec l2c_exp : L.exp -> C.exp = function
  | VarExp x -> Var x
  | NilExp -> Const Nil
  | BoolExp true -> Const (Int 1)
  | BoolExp false -> Const (Int 0)
  | IntExp n -> Const (Int n)
  | AppExp {fcn; arg} -> App {fcn= l2c_exp fcn; args= [l2c_exp arg]}
  | LamExp {vars; body} -> C.lams vars (l2c_exp body)
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
      Prim {oper; args= [l2c_exp left; l2c_exp right]}
  | IfExp {cond; then_; else_} ->
      If {cond= l2c_exp cond; then_= l2c_exp then_; else_= l2c_exp else_}
  | LetExp {bnds; body} ->
      let vars, bnds =
        List.split
          (List.map
             (fun (L.Bind {name; params; body}) ->
               (name, C.lams params (l2c_exp body)) )
             bnds )
      in
      Let {isrec= false; vars; bnds; body= l2c_exp body}
  | LetrecExp {bnds; body} ->
      let vars, bnds =
        List.split
          (List.map
             (fun (L.Bind {name; params; body}) ->
               (name, C.lams params (l2c_exp body)) )
             bnds )
      in
      Let {isrec= true; vars; bnds; body= l2c_exp body}
