module A = Abstract.Syntax
module S = Sexp.Syntax

let rec a2s_exp : A.aexp -> S.exp = function
  | VarAExp (x, _) -> Var x
  | NilAExp -> raise Utils.Unreachable
  | BoolAExp b -> Const (Bool b)
  | IntAExp i -> Const (Int i)
  | AppAExp {fcn; arg} -> App {fcn= a2s_expty fcn; args= [a2s_expty arg]}
  | LamAExp {params; body} ->
      Lam {vars= List.map fst params; body= a2s_expty body}
  | OpAExp {left; op; right} ->
      let prim =
        List.assoc op
          [ (PlusOp, S.Add); (MinusOp, S.Sub); (TimesOp, S.Mul)
          ; (DivideOp, S.Div); (EqOp, S.Eq); (LtOp, S.Lt); (LeOp, S.Le)
          ; (GtOp, S.Gt); (GeOp, S.Ge) ]
      in
      Prim {prim; args= [a2s_expty left; a2s_expty right]}
  | IfAExp {cond; then_; else_} ->
      If {cond= a2s_expty cond; then_= a2s_expty then_; else_= a2s_expty else_}
  | LetAExp {bnds; body} ->
      let vars, bnds' = List.map a2s_bnd bnds |> List.split in
      Let {isrec= false; vars; bnds= bnds'; body= a2s_expty body}
  | LetrecAExp {bnds; body} ->
      let vars, bnds' = List.map a2s_bnd bnds |> List.split in
      Let {isrec= true; vars; bnds= bnds'; body= a2s_expty body}

and a2s_bnd (A.ABind {name; params; body}) =
  if params = [] then (name, a2s_expty body)
  else (name, S.Lam {vars= List.map fst params; body= a2s_expty body})

and a2s_expty (exp, _) = a2s_exp exp

let a2s_prog exp = a2s_expty exp
