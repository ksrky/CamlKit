type frag = {name: string; params: Syntax.id list; body: Syntax.exp}

type frags = frag list

let notin : 'a list -> 'a list -> 'a list =
  List.fold_right (fun x -> List.filter (( <> ) x))

let frags : frags ref = ref []

let append (frag : frag) = frags := frag :: !frags

let rec lift_lam (exp : Syntax.exp) : Syntax.id list * Syntax.exp =
  match exp with
  | Var var -> ([var], exp)
  | Int _ -> ([], exp)
  | Nil -> ([], exp)
  | App {fcn; args} ->
      let fvs1, fcn' = lift_lam fcn in
      let fvs2, args' = lift_lams args in
      (fvs1 @ fvs2, App {fcn= fcn'; args= args'})
  | Lam {vars; body} ->
      let tmp = Id.fresh () in
      let vars', body' = lift_lam body in
      let fvs = notin vars' vars in
      append {name= Id.unique_name tmp; params= fvs @ vars; body= body'};
      (fvs, App {fcn= Var tmp; args= List.map (fun fv -> Syntax.Var fv) fvs})
  | Prim {oper; args} ->
      let fvs, args' = lift_lams args in
      (fvs, Prim {oper; args= args'})
  | Let {isrec; vars; bnds; body} ->
      let fvs_bnds, bnds' =
        List.split
          (List.map
             (fun exp ->
               let ps1, exp' =
                 match exp with
                 | Syntax.Lam {vars= ps1; body= exp} -> (ps1, exp)
                 | _ -> ([], exp)
               in
               let ps2, exp' = lift_lam exp in
               let fvs = notin ps2 (vars @ ps1) in
               let tmp = Id.fresh () in
               append {name= Id.unique_name tmp; params= fvs @ ps1; body= exp'};
               ( fvs
               , Syntax.App
                   { fcn= Syntax.Var tmp
                   ; args= List.map (fun fv -> Syntax.Var fv) fvs } ) )
             bnds )
      in
      let fvs_body, body' = lift_lam body in
      ( List.concat fvs_bnds @ fvs_body
      , Let {isrec; vars; bnds= bnds'; body= body'} )
  | If {cond; then_; else_} ->
      let fvs1, cond' = lift_lam cond in
      let fvs2, then' = lift_lam then_ in
      let fvs3, else' = lift_lam else_ in
      (fvs1 @ fvs2 @ fvs3, If {cond= cond'; then_= then'; else_= else'})

and lift_lams (exps : Syntax.exp list) : Syntax.id list * Syntax.exp list =
  let varss, exps' = List.split (List.map lift_lam exps) in
  (List.concat varss, exps')

let f (exp : Syntax.exp) : frags =
  lift_lam exp |> ignore;
  List.rev !frags
