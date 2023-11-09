module C = Core.Syntax
module Cm = Syntax

let notin : 'a list -> 'a list -> 'a list =
  List.fold_right (fun x -> List.filter (( <> ) x))

let frags : Cm.frags ref = ref []

let append (frag : Cm.frag) = frags := frag :: !frags

let rec lift_lam (exp : C.exp) : Cm.id list * Cm.exp =
  match exp with
  | Var var -> ([var], Var var)
  | Const c -> ([], Const c)
  | App {fcn; args} ->
      let fvs1, fcn' = lift_lam fcn in
      let fvs2, args' = lift_lams args in
      (fvs1 @ fvs2, App {fcn= fcn'; args= args'})
  | Lam {vars; body} ->
      let tmp = Id.fresh () in
      let vars', body' = lift_lam body in
      let fvs = notin vars' vars in
      append {name= Id.unique_name tmp; params= fvs @ vars; body= body'};
      (fvs, App {fcn= Var tmp; args= List.map (fun fv -> Cm.Var fv) fvs})
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
                 | C.Lam {vars= ps1; body= exp} -> (ps1, exp)
                 | _ -> ([], exp)
               in
               let ps2, exp' = lift_lam exp in
               let fvs = notin ps2 (vars @ ps1) in
               let tmp = Id.fresh () in
               append {name= Id.unique_name tmp; params= fvs @ ps1; body= exp'};
               ( fvs
               , Cm.App
                   {fcn= Cm.Var tmp; args= List.map (fun fv -> Cm.Var fv) fvs}
               ) )
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
  | Tuple exps ->
      let fvs, exps' = lift_lams exps in
      (fvs, Tuple exps')
  | Split {inp; vars; body} ->
      let fvs1, inp' = lift_lam inp in
      let fvs2, body' = lift_lam body in
      (fvs1 @ fvs2, Split {inp= inp'; vars; body= body'})

and lift_lams (exps : C.exp list) : Cm.id list * Cm.exp list =
  let varss, exps' = List.split (List.map lift_lam exps) in
  (List.concat varss, exps')

let f (exp : C.exp) : Cm.frags =
  append {name= "main"; params= []; body= snd (lift_lam exp)};
  List.rev !frags
