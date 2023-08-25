module I = IntSyn

let remove : 'a list -> 'a list -> 'a list = List.fold_right (fun x -> List.filter (( <> ) x))

let rec free_vars : I.exp -> I.id list = function
  | Int _ -> []
  | Nil -> []
  | Var id -> [id]
  | App (fcn, args) -> free_vars fcn @ List.concat_map free_vars args
  | Lam (vars, body) -> remove (free_vars body) vars
  | Prim (_, args) -> List.concat_map free_vars args
  | Let (false, vars, exps, body) -> List.concat_map free_vars exps @ remove (free_vars body) vars
  | Let (true, vars, exps, body) -> remove (List.concat_map free_vars exps @ free_vars body) vars
  | If (test, then', else') -> free_vars test @ free_vars then' @ free_vars else'
  | Seq (exp, rest) -> free_vars exp @ free_vars rest

let rec f : I.exp -> I.exp = function
  | I.Lam (vars, body) ->
      let fvs = remove (free_vars body) vars in
      I.App (I.Lam (fvs @ vars, f body), List.map (fun fv -> I.Var fv) fvs)
  | exp -> exp
