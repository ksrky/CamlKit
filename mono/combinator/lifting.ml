module C = Core.Syntax
module Cm = Syntax

let ( // ) (init : 'a list) (elims : 'a list) : 'a list =
  List.fold_right (fun x -> List.filter (( <> ) x)) elims init

let remove_dup xs =
  List.fold_right (fun x xs -> if List.mem x xs then xs else x :: xs) xs []

let frags : Cm.frags ref = ref []

let append (frag : Cm.frag) = frags := frag :: !frags

type exp_clos = EXP of Cm.exp | CLOS of (Cm.id list * Cm.exp)

let rec lift_lam (exp : C.exp) : Cm.id list * Cm.exp =
  match exp with
  | Var var -> ([var], Var var)
  | Const c -> ([], Const c)
  | App {fcn; args} ->
      let fvs1, fcn' = lift_lam fcn in
      let fvs2, args' = lift_lams args in
      (fvs1 @ fvs2, App {fcn= fcn'; args= args'})
  | Lam {vars; body} ->
      let tmp = Id.from_string "lamtmp" in
      let vars', body' = lift_lam body in
      let fvs = remove_dup vars' // vars in
      append {name= Id.unique_name tmp; params= fvs @ vars; body= body'};
      if fvs = [] then (fvs, Fun tmp)
      else (fvs, App {fcn= Fun tmp; args= List.map (fun fv -> Cm.Var fv) fvs})
  | Prim {oper; args} ->
      let fvs, args' = lift_lams args in
      (fvs, Prim {oper; args= args'})
  | Let {isrec; vars; bnds; body} ->
      let bnds' =
        List.map2
          (fun var exp ->
            let vs, exp' = lift_lam exp in
            let fvs = remove_dup vs // vars in
            if fvs = [] then exp'
            else Cm.App {fcn= exp'; args= List.map (fun fv -> Cm.Var fv) fvs} )
          vars bnds
      in
      let fvs, body' = lift_lam body in
      (fvs // vars, Let {isrec; vars; bnds= bnds'; body= body'})
  | If {cond; then_; else_} ->
      let fvs1, cond' = lift_lam cond in
      let fvs2, then' = lift_lam then_ in
      let fvs3, else' = lift_lam else_ in
      (fvs1 @ fvs2 @ fvs3, If {cond= cond'; then_= then'; else_= else'})
  | Clos clos -> failwith "not implemented"

and lift_lams (exps : C.exp list) : Cm.id list * Cm.exp list =
  let varss, exps' = List.split (List.map lift_lam exps) in
  (List.concat varss, exps')

let f (exp : C.exp) : Cm.frags =
  append {name= "main"; params= []; body= snd (lift_lam exp)};
  List.rev !frags
