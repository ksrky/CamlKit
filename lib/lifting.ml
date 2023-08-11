open IntSyn

let defs : defs ref = ref []

let append (def : def) = defs := def :: !defs

let rec lift_lam (exp : exp) : exp =
  match exp with
  | Int _ -> exp
  | Nil -> exp
  | Var _ -> exp
  | App (fcn, args) -> App (lift_lam fcn, List.map lift_lam args)
  | Lam (vars, body) ->
      let name = Ident.fresh () in
      append {name; params= vars; body= lift_lam body};
      Var name
  | Builtin (fcn, args) -> Builtin (fcn, List.map lift_lam args)
  | Let (vars, exps, body) ->
      List.iter2
        (fun name -> function
          | Lam (params, exp) -> append {name; params; body= lift_lam exp}
          | exp -> append {name; params= []; body= lift_lam exp} )
        vars exps;
      lift_lam body
  | Letrec (vars, exps, body) ->
      List.iter2
        (fun name -> function
          | Lam (params, exp) -> append {name; params; body= lift_lam exp}
          | exp -> append {name; params= []; body= lift_lam exp} )
        vars exps;
      lift_lam body
  | If (test, then', else') -> If (lift_lam test, lift_lam then', lift_lam else')

let f (exp : exp) : def list =
  let ans = lift_lam exp in
  let main = Ident.from_string "main" in
  List.rev ({name= main; params= []; body= ans} :: !defs)
