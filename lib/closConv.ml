module I = IntSyn

let remove : 'a list -> 'a list -> 'a list = List.fold_right (fun x -> List.filter (( <> ) x))

let rec free_vars : I.exp -> I.binders = function
  | Int _ -> []
  | Nil -> []
  | Var var -> [var]
  | App (fcn, args) -> free_vars fcn @ List.concat_map free_vars args
  | Lam (bndrs, body) -> remove (free_vars body) bndrs
  | Prim (_, args) -> List.concat_map free_vars args
  | Let (false, bndrs, exps, body) -> List.concat_map free_vars exps @ remove (free_vars body) bndrs
  | Let (true, bndrs, exps, body) -> remove (List.concat_map free_vars exps @ free_vars body) bndrs
  | If (test, then', else') -> free_vars test @ free_vars then' @ free_vars else'
  | Seq (exp, rest) -> free_vars exp @ free_vars rest

let rec f : I.exp -> I.exp = function
  | I.Lam (bndrs, body) ->
      let fvs = remove (free_vars body) bndrs in
      I.App (I.Lam (fvs @ bndrs, f body), List.map (fun fv -> I.Var fv) fvs)
  | exp -> exp
