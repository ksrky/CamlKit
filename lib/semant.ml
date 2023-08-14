open IntSyn
open Env

let rec trans_exp env exp =
  let rec trexp : AbsSyn.exp -> exp = function
    | AbsSyn.VarExp id -> Var id
    | AbsSyn.NilExp -> Nil
    | AbsSyn.IntExp x -> Int x
    | AbsSyn.AppExp _ as exp ->
        let rec loop acc = function
          | AbsSyn.AppExp {fcn; arg} -> loop (trexp arg :: acc) fcn
          | VarExp id when Ident.to_string id = "read_int" -> Builtin ("readi", acc)
          | VarExp id when Ident.to_string id = "print_int" -> Builtin ("printi", acc)
          | fcn -> App (trexp fcn, acc)
        in
        loop [] exp
    | AbsSyn.LamExp {vars; body} ->
        let env' = List.fold_right (fun id -> extend id ValBind) vars env in
        Lam (vars, trans_exp env' body)
    | AbsSyn.OpExp {left; oper= PlusOp; right} -> Builtin ("add", [trexp left; trexp right])
    | AbsSyn.OpExp {left; oper= MinusOp; right} -> Builtin ("sub", [trexp left; trexp right])
    | AbsSyn.OpExp {left; oper= TimesOp; right} -> Builtin ("mul", [trexp left; trexp right])
    | AbsSyn.OpExp {left; oper= DivideOp; right} -> Builtin ("div", [trexp left; trexp right])
    | AbsSyn.OpExp {left; oper= EqOp; right} -> Builtin ("eq", [trexp left; trexp right])
    | AbsSyn.OpExp {left; oper= NeqOp; right} -> Builtin ("ne", [trexp left; trexp right])
    | AbsSyn.OpExp {left; oper= LtOp; right} -> Builtin ("lt", [trexp left; trexp right])
    | AbsSyn.OpExp {left; oper= LeOp; right} -> Builtin ("le", [trexp left; trexp right])
    | AbsSyn.OpExp {left; oper= GtOp; right} -> Builtin ("lt", [trexp right; trexp left])
    | AbsSyn.OpExp {left; oper= GeOp; right} -> Builtin ("le", [trexp right; trexp left])
    | AbsSyn.IfExp {test; then_; else_} -> If (trexp test, trexp then_, trexp else_)
    | AbsSyn.LetExp {decs; body} ->
        let env' = List.fold_right (fun {AbsSyn.name; _} -> extend name ValBind) decs env in
        Let (List.map (fun {AbsSyn.name; _} -> name) decs, trans_decs env decs, trans_exp env' body)
    | AbsSyn.LetrecExp {decs; body} ->
        let env' = List.fold_right (fun {AbsSyn.name; _} -> extend name ValBind) decs env in
        Letrec
          (List.map (fun {AbsSyn.name; _} -> name) decs, trans_decs env' decs, trans_exp env' body)
  in
  try trexp exp
  with Out_of_scope id ->
    ErrorMsg.error ("Unbound value " ^ Ident.to_string id);
    Nil

and trans_decs env decs =
  let trdecs =
    List.map (fun {AbsSyn.params; AbsSyn.body; _} ->
        match params with
        | [] -> trans_exp env body
        | _ ->
            let env' = List.fold_right (fun id -> extend id ValBind) params env in
            Lam (params, trans_exp env' body) )
  in
  trdecs decs
