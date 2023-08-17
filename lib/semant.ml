module A = AbsSyn
module I = IntSyn
module E = Env

let rec trans_exp env exp =
  let rec trexp : A.exp -> I.exp = function
    | VarExp id -> Var id
    | NilExp -> Nil
    | IntExp x -> Int x
    | AppExp _ as exp ->
        let rec loop acc : A.exp -> I.exp = function
          | AppExp {fcn; arg} -> loop (trexp arg :: acc) fcn
          | VarExp id when Ident.name id = "read_int" -> Builtin ("readi", acc)
          | VarExp id when Ident.name id = "print_int" -> Builtin ("printi", acc)
          | fcn -> App (trexp fcn, acc)
        in
        loop [] exp
    | LamExp {vars; body} ->
        let env' = List.fold_right (fun id -> E.extend id ValBind) vars env in
        Lam (vars, trans_exp env' body)
    | OpExp {left; op= PlusOp; right} -> Builtin ("add", [trexp left; trexp right])
    | OpExp {left; op= MinusOp; right} -> Builtin ("sub", [trexp left; trexp right])
    | OpExp {left; op= TimesOp; right} -> Builtin ("mul", [trexp left; trexp right])
    | OpExp {left; op= DivideOp; right} -> Builtin ("div", [trexp left; trexp right])
    | OpExp {left; op= EqOp; right} -> Builtin ("eq", [trexp left; trexp right])
    | OpExp {left; op= NeqOp; right} -> Builtin ("ne", [trexp left; trexp right])
    | OpExp {left; op= LtOp; right} -> Builtin ("lt", [trexp left; trexp right])
    | OpExp {left; op= LeOp; right} -> Builtin ("le", [trexp left; trexp right])
    | OpExp {left; op= GtOp; right} -> Builtin ("lt", [trexp right; trexp left])
    | OpExp {left; op= GeOp; right} -> Builtin ("le", [trexp right; trexp left])
    | IfExp {test; then_; else_} -> If (trexp test, trexp then_, trexp else_)
    | LetExp {bnds; body} ->
        let env' = List.fold_right (fun {A.name; _} -> E.extend name ValBind) bnds env in
        Let
          (false, List.map (fun {A.name; _} -> name) bnds, trans_bnds env bnds, trans_exp env' body)
    | LetrecExp {bnds; body} ->
        let env' = List.fold_right (fun {A.name; _} -> E.extend name ValBind) bnds env in
        Let
          (true, List.map (fun {A.name; _} -> name) bnds, trans_bnds env' bnds, trans_exp env' body)
  in
  try trexp exp
  with E.Out_of_scope id ->
    ErrorMsg.error ("Unbound value " ^ Ident.name id);
    Nil

and trans_bnds env bnds =
  let trbnds =
    List.map (fun {A.params; A.body; _} ->
        match params with
        | [] -> trans_exp env body
        | _ ->
            let env' = List.fold_right (fun id -> E.extend id ValBind) params env in
            Lam (params, trans_exp env' body) )
  in
  trbnds bnds

let trans_defs (env : E.env) (defs : A.def list) : I.exp =
  let rec linear = function
    | [] -> A.IntExp 0
    | A.LetDef bnds :: rest -> A.LetExp {bnds; body= linear rest}
    | A.LetrecDef bnds :: rest -> A.LetrecExp {bnds; body= linear rest}
  in
  trans_exp env (linear defs)
