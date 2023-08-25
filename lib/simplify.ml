module I = IntSyn

let rec uncurrying : I.exp -> Ident.t list * I.exp = function
  | Lam (vars, body) ->
      let vars', body' = uncurrying body in
      (vars @ vars', body')
  | e -> ([], e)

let rec let_expansion : I.exp -> Ident.t list * I.exp list * I.exp = function
  | Let (false, vars, bnds, body) ->
      let vars', bnds', body' = let_expansion body in
      (vars @ vars', bnds @ bnds', body')
  | e -> ([], [], e)

let rec f : I.exp -> I.exp = function
  | Int i -> Int i
  | Nil -> Nil
  | Var id -> Var id
  | App (fcn, args) -> App (f fcn, List.map f args)
  | Lam (vars, body) ->
      let vars', body' = uncurrying body in
      Lam (vars @ vars', f body')
  | Prim (fcn, args) -> Prim (fcn, List.map f args)
  | Let (false, _, _, _) as e ->
      let vars', bnds', body' = let_expansion e in
      Let (false, vars', List.map f bnds', f body')
  | Let (true, vars, bnds, body) -> Let (true, vars, List.map f bnds, f body)
  | If (test, then_, else_) -> If (f test, f then_, f else_)
  | Seq (exp, rest) -> Seq (f exp, f rest)
