module A = Abstract.Syntax
module C = Core.Syntax

let rec a2c_ty : A.ty -> C.ty = function
  | A.NilTy -> failwith "nil is not supported in core"
  | A.BoolTy -> C.BoolTy
  | A.IntTy -> C.IntTy
  | A.FunTy (t1, t2) -> C.FunTy (a2c_ty t1, a2c_ty t2)
  | A.MetaTy _ -> failwith "unreachable"

let a2c_var ((id, ty) : A.var) : C.var = (id, a2c_ty ty)

let lambda_ty (params : A.var list) (body_ty : A.ty) : C.ty =
  C.fun_tys (List.map (fun (_, ty) -> a2c_ty ty) params) (a2c_ty body_ty)

let rec a2c_exp : A.aexp -> C.exp = function
  | VarAExp x -> Var (a2c_var x)
  | NilAExp -> failwith "nil is not supported in core"
  | BoolAExp b -> Const (Bool b)
  | IntAExp i -> Const (Int i)
  | AppAExp {fcn; arg} -> App {fcn= a2c_expty fcn; arg= a2c_expty arg}
  | LamAExp {params; body} ->
      C.lams (List.map a2c_var params) (a2c_expty body) |> fst
  | OpAExp {left; op; right} ->
      let oper =
        List.assoc op
          [ (PlusOp, C.Add); (MinusOp, C.Sub); (TimesOp, C.Mul)
          ; (DivideOp, C.Div); (EqOp, C.Eq); (LtOp, C.Lt); (LeOp, C.Le)
          ; (GtOp, C.Gt); (GeOp, C.Ge) ]
      in
      Prim {left= a2c_expty left; oper; right= a2c_expty right}
  | IfAExp {cond; then_; else_} ->
      If {cond= a2c_expty cond; then_= a2c_expty then_; else_= a2c_expty else_}
  | LetAExp {bnds; body} ->
      let vars, bnds' =
        List.split
          (List.map
             (fun (A.ABind {name; params; body}) ->
               ( (name, lambda_ty params (snd body))
               , C.lams (List.map a2c_var params) (a2c_expty body) ) )
             bnds )
      in
      Let {isrec= false; vars; bnds= bnds'; body= a2c_expty body}
  | LetrecAExp {bnds; body} ->
      let vars, bnds =
        List.split
          (List.map
             (fun (A.ABind {name; params; body}) ->
               ( (name, lambda_ty params (snd body))
               , C.lams (List.map a2c_var params) (a2c_expty body) ) )
             bnds )
      in
      Let {isrec= true; vars; bnds; body= a2c_expty body}

and a2c_expty ((exp, ty) : A.expty) : C.expty = (a2c_exp exp, a2c_ty ty)

let a2c_prog : A.aprog -> C.prog = a2c_expty
