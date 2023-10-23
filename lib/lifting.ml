open IntSyn
module Ident = Language.Ident

let frags : frags ref = ref []

let append (frag : frag) = frags := frag :: !frags

let rec lift_lam (exp : exp) : binders * exp =
  match exp with
  | Int _ -> ([], exp)
  | Nil -> ([], exp)
  | Var var -> ([var], exp)
  | App (fcn, args) ->
      let vars1, fcn' = lift_lam fcn in
      let vars2, args' = lift_lams args in
      (vars1 @ vars2, App (fcn', args'))
  | Lam (vars, body) ->
      let id = Ident.fresh () in
      let vars', body' = lift_lam body in
      let fvs = List.fold_right (fun x -> List.filter (( <> ) x)) vars' vars in
      append {name= Ident.unique_name id; params= fvs @ vars; body= body'};
      ([], App (Var (id, TypeCheck.type_of [] exp), List.map (fun fv -> Var fv) fvs))
  | Prim (fcn, args) ->
      let vars, args' = lift_lams args in
      (vars, Prim (fcn, args'))
  | Let (_, vars, exps, body) ->
      List.iter2
        (fun (id, _) -> function
          | Lam (vars1, exp) ->
              let vars2, exp' = lift_lam exp in
              append {name= Ident.unique_name id; params= vars2 @ vars1; body= exp'}
          | exp ->
              let vars1, exp' = lift_lam exp in
              append {name= Ident.unique_name id; params= vars1; body= exp'} )
        vars exps;
      lift_lam body
  | If (test, then_, else_) ->
      let vars1, test' = lift_lam test in
      let vars2, then' = lift_lam then_ in
      let vars3, else' = lift_lam else_ in
      (vars1 @ vars2 @ vars3, If (test', then', else'))
  | Seq (exp, rest) ->
      let vars1, exp' = lift_lam exp in
      let vars2, rest' = lift_lam rest in
      (vars1 @ vars2, Seq (exp', rest'))

and lift_lams (exps : exp list) : binders * exp list =
  let varss, exps' = List.split (List.map lift_lam exps) in
  (List.concat varss, exps')

let f (exp : exp) : frag list =
  let _, ans = lift_lam exp in
  List.rev ({name= "main"; params= []; body= ans} :: !frags)
