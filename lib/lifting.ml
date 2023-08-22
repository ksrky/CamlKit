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
      let id = Ident.fresh () in
      append {name= Ident.unique_name id; params= vars; body= lift_lam body};
      Var id
  | Prim (fcn, args) -> Prim (fcn, List.map lift_lam args)
  | Let (_, vars, exps, body) ->
      let memo = ref [] in
      List.iter2
        (fun id -> function
          | Lam (params, exp) -> append {name= Ident.unique_name id; params; body= lift_lam exp}
          | exp -> memo := (id, exp) :: !memo )
        vars exps;
      if !memo <> [] then (
        let f = Ident.fresh () in
        append {name= Ident.unique_name f; params= List.map fst !memo; body= lift_lam body};
        lift_lam (App (Var f, List.map snd !memo)) )
      else lift_lam body
  | If (test, then', else') -> If (lift_lam test, lift_lam then', lift_lam else')

let f (exp : exp) : def list =
  let ans = lift_lam exp in
  List.rev ({name= "main"; params= []; body= ans} :: !defs)
