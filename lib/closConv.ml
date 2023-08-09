module I = IntSyn

let diff : 'a list -> 'a list -> 'a list = List.fold_right (fun x -> List.filter (( <> ) x))

let rec free_vars : I.exp -> I.id list = function
  | I.Int _ -> []
  | I.Nil -> []
  | I.Var id -> [id]
  | I.App (fcn, args) -> free_vars fcn @ List.concat_map free_vars args
  | I.Lam (vars, body) -> diff (free_vars body) vars
  | I.Builtin (_, args) -> List.concat_map free_vars args
  | I.Let (vars, exps, body) -> List.concat_map free_vars exps @ diff (free_vars body) vars
  | I.Letrec (vars, exps, body) -> diff (List.concat_map free_vars exps @ free_vars body) vars
  | I.If (test, then', else') -> free_vars test @ free_vars then' @ free_vars else'

let rec convert globals = function
  | I.Lam (vars, body) ->
      let fvs = diff (free_vars body) (globals @ vars) in
      I.App (I.Lam (fvs @ vars, convert globals body), List.map (fun fv -> I.Var fv) fvs)
  | exp -> exp

let f : I.exp -> I.exp = convert []
