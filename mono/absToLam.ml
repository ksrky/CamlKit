module A = Abstract.Syntax
module L = Lambda.Syntax

let rec a2c_ty : A.ty -> L.ty = function
  | A.NilTy -> raise Utils.Unreachable
  | A.BoolTy -> L.BoolTy
  | A.IntTy -> L.IntTy
  | A.FunTy (t1, t2) -> L.FunTy (a2c_ty t1, a2c_ty t2)
  | A.MetaTy _ -> raise Utils.Unreachable

let a2c_var ((id, ty) : A.var) : L.var = (id, a2c_ty ty)

let lambda_ty (params : A.var list) (body_ty : A.ty) : L.ty =
  L.fun_tys (List.map (fun (_, ty) -> a2c_ty ty) params) (a2c_ty body_ty)

let rec a2c_exp : A.aexp -> L.exp = function
  | VarAExp x -> Var (a2c_var x)
  | NilAExp -> raise Utils.Unreachable
  | BoolAExp b -> Const (Bool b)
  | IntAExp i -> Const (Int i)
  | AppAExp {fcn; arg} -> App {fcn= a2c_expty fcn; arg= a2c_expty arg}
  | LamAExp {params; body} ->
      L.lams (List.map a2c_var params) (a2c_expty body) |> fst
  | OpAExp {left; op; right} ->
      let oper =
        List.assoc op
          [ (PlusOp, L.Add); (MinusOp, L.Sub); (TimesOp, L.Mul)
          ; (DivideOp, L.Div); (EqOp, L.Eq); (LtOp, L.Lt); (LeOp, L.Le)
          ; (GtOp, L.Gt); (GeOp, L.Ge) ]
      in
      Prim {left= a2c_expty left; oper; right= a2c_expty right}
  | IfAExp {cond; then_; else_} ->
      If {cond= a2c_expty cond; then_= a2c_expty then_; else_= a2c_expty else_}
  | LetAExp {bnds; body} ->
      let decs = List.map a2c_bnd bnds in
      L.mk_let decs (a2c_expty body) |> fst
  | LetrecAExp {bnds; body} ->
      let defs =
        List.map
          (fun bnd ->
            let var, exp = a2c_bnd bnd in
            match exp with
            | Lam {var= var'; body} -> {L.var; params= [var']; body}
            | _ -> raise Utils.Unreachable )
          bnds
      in
      Fix {defs; body= a2c_expty body}

and a2c_bnd (A.ABind {name; params; body}) =
  ( (name, lambda_ty params (snd body))
  , L.lams (List.map a2c_var params) (a2c_expty body) |> fst )

and a2c_expty ((exp, ty) : A.expty) : L.expty = (a2c_exp exp, a2c_ty ty)

let a2c_prog : A.aprog -> L.prog = a2c_expty
