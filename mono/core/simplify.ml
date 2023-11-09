module C = Syntax

let rec uncurrying : C.exp -> C.id list * C.exp = function
  | Lam {vars; body} ->
      let vars', body' = uncurrying body in
      (vars @ vars', body')
  | e -> ([], e)

let rec let_expansion : C.exp -> C.id list * C.exp list * C.exp = function
  | Let {isrec= false; vars; bnds; body} ->
      let vars', bnds', body' = let_expansion body in
      (vars @ vars', bnds @ bnds', body')
  | e -> ([], [], e)

let rec f : C.exp -> C.exp = function
  | Const c -> Const c
  | Var id -> Var id
  | App {fcn; args} -> App {fcn= f fcn; args= List.map f args}
  | Lam {vars; body} ->
      let vars', body' = uncurrying body in
      Lam {vars= vars @ vars'; body= f body'}
  | Prim {oper; args} -> Prim {oper; args= List.map f args}
  | Let {isrec= false; _} as e ->
      let vars', bnds', body' = let_expansion e in
      Let {isrec= false; vars= vars'; bnds= List.map f bnds'; body= f body'}
  | Let {isrec= true; vars; bnds; body} ->
      Let {isrec= true; vars; bnds= List.map f bnds; body= f body}
  | If {cond; then_; else_} -> If {cond= f cond; then_= f then_; else_= f else_}
  | Tuple es -> Tuple (List.map f es)
  | Split {inp; vars; body} -> Split {inp= f inp; vars; body= f body}
