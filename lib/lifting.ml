open IntSyn

let remove : 'a list -> 'a list -> 'a list = List.fold_right (fun x -> List.filter (( <> ) x))

let rec free_vars : exp -> binders = function
  | Int _ -> []
  | Nil -> []
  | Var var -> [var]
  | App (fcn, args) -> free_vars fcn @ List.concat_map free_vars args
  | Lam (bndrs, body) -> remove (free_vars body) bndrs
  | Prim (_, args) -> List.concat_map free_vars args
  | Let (false, bndrs, exps, body) -> List.concat_map free_vars exps @ remove (free_vars body) bndrs
  | Let (true, bndrs, exps, body) -> remove (List.concat_map free_vars exps @ free_vars body) bndrs
  | If (test, then', else') -> free_vars test @ free_vars then' @ free_vars else'
  | Seq (exp, rest) -> free_vars exp @ free_vars rest

let frags : frags ref = ref []

let append (frag : frag) = frags := frag :: !frags

let rec lift_lam (exp : exp) : exp =
  match exp with
  | Int _ -> exp
  | Nil -> exp
  | Var _ -> exp
  | App (fcn, args) -> App (lift_lam fcn, List.map lift_lam args)
  | Lam (vars, body) ->
      let id = Ident.fresh () in
      let fvs = remove (free_vars body) vars in
      append {name= Ident.unique_name id; params= fvs @ vars; body= lift_lam body};
      App (Var (id, TypeCheck.type_of [] exp), List.map (fun fv -> Var fv) fvs)
  | Prim (fcn, args) -> Prim (fcn, List.map lift_lam args)
  | Let (_, vars, exps, body) ->
      let memo = ref [] in
      List.iter2
        (fun (id, ty) -> function
          | Lam (params, exp) -> append {name= Ident.unique_name id; params; body= lift_lam exp}
          | exp -> memo := ((id, ty), exp) :: !memo )
        vars exps;
      if !memo = [] then lift_lam body
      else
        let f = Ident.fresh () and params = List.map fst !memo in
        append {name= Ident.unique_name f; params; body= lift_lam body};
        lift_lam (App (Var (f, TypeCheck.type_of [] (Lam (params, body))), List.map snd !memo))
  | If (test, then', else') -> If (lift_lam test, lift_lam then', lift_lam else')
  | Seq (exp, rest) -> Seq (lift_lam exp, lift_lam rest)

let f (exp : exp) : frag list =
  let ans = lift_lam exp in
  List.rev ({name= "main"; params= []; body= ans} :: !frags)
