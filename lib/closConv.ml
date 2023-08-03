open IntSyn

let diff : 'a list -> 'a list -> 'a list = List.fold_right (fun x -> List.filter (( <> ) x))

let rec free_vars : exp -> id list = function
  | Int _ -> []
  | Nil -> []
  | Var id -> [id]
  | App (fcn, args) -> free_vars fcn @ List.concat_map free_vars args
  | Lam (vars, body) -> diff (free_vars body) vars
  | Builtin (_, args) -> List.concat_map free_vars args
  | Let (vars, exps, body) -> List.concat_map free_vars exps @ diff (free_vars body) vars
  | Letrec (vars, exps, body) -> diff (List.concat_map free_vars exps @ free_vars body) vars
  | If (test, then', else') -> free_vars test @ free_vars then' @ free_vars else'

let convert globals = function
  | Lam (vars, body) ->
      let fvs = diff (free_vars body) (globals @ vars) in
      App (Lam (fvs @ vars, body), List.map (fun fv -> Var fv) fvs)
  | exp -> exp
