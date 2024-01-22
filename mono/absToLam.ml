module A = Abstract.Syntax
module L = Lambda.Syntax

let rec a2l_ty : A.ty -> L.ty = function
  | A.NilTy -> raise Utils.Unreachable
  | A.BoolTy -> L.BoolTy
  | A.IntTy -> L.IntTy
  | A.FunTy (t1, t2) -> L.FunTy (a2l_ty t1, a2l_ty t2)
  | A.MetaTy _ -> raise Utils.Unreachable

let a2l_var ((id, ty) : A.var) : L.var = (id, a2l_ty ty)

let lambda_ty (params : A.var list) (body_ty : A.ty) : L.ty =
  L.fun_tys (List.map (fun (_, ty) -> a2l_ty ty) params) (a2l_ty body_ty)

let rec a2l_exp : A.aexp -> L.exp = function
  | VarAExp x -> Var (a2l_var x)
  | NilAExp -> raise Utils.Unreachable
  | BoolAExp b -> Const (Bool b)
  | IntAExp i -> Const (Int i)
  | AppAExp {fcn; arg} -> App {fcn= a2l_expty fcn; arg= a2l_expty arg}
  | LamAExp {params; body} ->
      L.lams (List.map a2l_var params) (a2l_expty body) |> fst
  | OpAExp {left; op; right} ->
      let oper =
        List.assoc op
          [ (PlusOp, L.Add); (MinusOp, L.Sub); (TimesOp, L.Mul)
          ; (DivideOp, L.Div); (EqOp, L.Eq); (LtOp, L.Lt); (LeOp, L.Le)
          ; (GtOp, L.Gt); (GeOp, L.Ge) ]
      in
      Prim {left= a2l_expty left; oper; right= a2l_expty right}
  | IfAExp {cond; then_; else_} ->
      If {cond= a2l_expty cond; then_= a2l_expty then_; else_= a2l_expty else_}
  | LetAExp {bnds; body} ->
      let decs = List.map a2l_bnd bnds in
      L.mk_let decs (a2l_expty body) |> fst
  | LetrecAExp {bnds; body} -> failwith "TODO"

and a2l_bnd (A.ABind {name; params; body}) =
  ( (name, lambda_ty params (snd body))
  , L.lams (List.map a2l_var params) (a2l_expty body) |> fst )

and a2l_expty ((exp, ty) : A.expty) : L.expty = (a2l_exp exp, a2l_ty ty)

let a2l_prog : A.aprog -> L.prog = a2l_expty
