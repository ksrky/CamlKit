module C = Core.Syntax

let notin : 'a list -> 'a list -> 'a list = List.fold_right (fun x -> List.filter (( <> ) x))

let frags : Syntax.frags ref = ref []

let append (frag : Syntax.frag) = frags := frag :: !frags

let rec lift_lam (exp : C.exp) : C.id list * C.exp =
  match exp with
  | Var var -> ([var], exp)
  | Int _ -> ([], exp)
  | Nil -> ([], exp)
  | App {fcn; args} ->
      let vars1, fcn' = lift_lam fcn in
      let vars2, args' = lift_lams args in
      (vars1 @ vars2, App {fcn= fcn'; args= args'})
  | Lam {vars; body} ->
      let id = Id.fresh () in
      let vars', body' = lift_lam body in
      let fvs = notin vars' vars in
      append {name= Id.unique_name id; params= fvs @ vars; body= body'};
      ([], App {fcn= Var id; args= List.map (fun fv -> C.Var fv) fvs})
  | Prim {oper; args} ->
      let vars, args' = lift_lams args in
      (vars, Prim {oper; args= args'})
  | Let {vars; bnds; body; _} ->
      List.iter2
        (fun var -> function
          | C.Lam {vars= ps1; body= exp} ->
              let ps2, exp' = lift_lam exp in
              let fvs = notin ps2 ps1 in
              let tmp = Id.fresh () in
              if fvs = [] then append {name= Id.unique_name var; params= ps1; body= exp'}
              else (
                append {name= Id.unique_name tmp; params= fvs @ ps1; body= exp'};
                append
                  { name= Id.unique_name var
                  ; params= []
                  ; body= C.App {fcn= C.Var tmp; args= List.map (fun fv -> C.Var fv) fvs} } )
          | exp ->
              let params, exp' = lift_lam exp in
              append {name= Id.unique_name var; params; body= exp'} )
        vars bnds;
      lift_lam body
  | _ -> failwith "not implemented"

and lift_lams (exps : C.exp list) : C.id list * C.exp list =
  let varss, exps' = List.split (List.map lift_lam exps) in
  (List.concat varss, exps')

let f (exp : C.exp) : Syntax.frag list =
  lift_lam exp |> ignore;
  List.rev !frags
