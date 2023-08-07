open IntSyn

let defs : defs ref = ref []

let rec lift_lam (exp : exp) : exp =
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
  | Let (vars, exps, Lam (params, body)) ->
      List.iteri (fun i name -> defs := {name; params; body= List.nth exps i} :: !defs) vars;
      lift_lam body
  | Let (vars, exps, body) ->
      List.iteri (fun i v -> defs := {name= v; params= []; body= List.nth exps i} :: !defs) vars;
      lift_lam body
  | Letrec (vars, exps, Lam (params, body)) ->
      List.iteri (fun i name -> defs := {name; params; body= List.nth exps i} :: !defs) vars;
      lift_lam body
  | Letrec (vars, exps, body) ->
      List.iteri (fun i v -> defs := {name= v; params= []; body= List.nth exps i} :: !defs) vars;
      lift_lam body
  | If (test, then', else') -> If (lift_lam test, lift_lam then', lift_lam else')

let f (exp : exp) : def list * exp =
  let ans = lift_lam exp in
  (!defs, ans)
