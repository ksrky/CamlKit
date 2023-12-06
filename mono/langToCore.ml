module L = Language.Syntax
module C = Core.Syntax

let rec l2c_exp : L.aexp -> C.exp = function
  | VarAExp x -> Var x
  | NilAExp -> Const Nil
  | BoolAExp true -> Const (Int 1)
  | BoolAExp false -> Const (Int 0)
  | IntAExp n -> Const (Int n)
  | AppAExp {fcn= fcn, _; arg= arg, _} ->
      App {fcn= l2c_exp fcn; arg= l2c_exp arg}
  | LamAExp {vars; body= body, _} -> C.lams vars (l2c_exp body)
  | OpAExp {left= left, _; op; right= right, _} ->
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
      Prim {left= l2c_exp left; oper; right= l2c_exp right}
  | IfAExp {cond= cond, _; then_= then_, _; else_= else_, _} ->
      If {cond= l2c_exp cond; then_= l2c_exp then_; else_= l2c_exp else_}
  | LetAExp {bnds; body= body, _} ->
      let vars, bnds =
        List.split
          (List.map
             (fun (L.ABind {name; params; body= body, _}) ->
               (name, C.lams (List.map fst params) (l2c_exp body)) )
             bnds )
      in
      Let {isrec= false; vars; bnds; body= l2c_exp body}
  | LetrecAExp {bnds; body= body, _} ->
      let vars, bnds =
        List.split
          (List.map
             (fun (L.ABind {name; params; body= body, _}) ->
               (name, C.lams (List.map fst params) (l2c_exp body)) )
             bnds )
      in
      Let {isrec= true; vars; bnds; body= l2c_exp body}
