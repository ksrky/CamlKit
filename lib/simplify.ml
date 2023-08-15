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

let rec simp_exp : I.exp -> I.exp = function
  | Int i -> Int i
  | Nil -> Nil
  | Var id -> Var id
  | App (fcn, args) -> App (simp_exp fcn, List.map simp_exp args)
  | Lam (vars, body) ->
      let vars', body' = uncurrying body in
      Lam (vars @ vars', simp_exp body')
  | Builtin (fcn, args) -> Builtin (fcn, List.map simp_exp args)
  | Let (false, _, _, _) as e ->
      let vars', bnds', body' = let_expansion e in
      Let (false, vars', List.map simp_exp bnds', simp_exp body')
  | Let (true, vars, bnds, body) -> Let (false, vars, List.map simp_exp bnds, simp_exp body)
  | If (test, then_, else_) -> If (simp_exp test, simp_exp then_, simp_exp else_)
