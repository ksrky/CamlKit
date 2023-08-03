open IntSyn

let rec lift_lam (exp : exp) : exp =
  let defs : def list ref = ref [] in
  match exp with
  | Int _ -> exp
  | Nil -> exp
  | Var _ -> exp
  | App (fcn, args) -> App (lift_lam fcn, List.map lift_lam args)
  | Lam (vars, body) ->
      let name = Ident.fresh () in
      defs := {name; params= vars; body} :: !defs;
      Var name
  | Builtin (fcn, args) -> Builtin (fcn, List.map lift_lam args)
  | Let (vars, exps, body) -> Let (vars, List.map lift_lam exps, lift_lam body)
  | Letrec (vars, exps, body) -> Letrec (vars, List.map lift_lam exps, lift_lam body)
  | If (test, then', else') -> If (lift_lam test, lift_lam then', lift_lam else')
