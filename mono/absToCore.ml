module A = Abstract.Syntax
module C = Core.Syntax

let tyctx : C.tyctx ref = ref []

let rec a2c_ty : A.ty -> C.ty = function
  | A.NilTy -> failwith "nil is not supported in core"
  | A.BoolTy -> C.BoolTy
  | A.IntTy -> C.IntTy
  | A.FunTy (t1, t2) -> C.FunTy (a2c_ty t1, a2c_ty t2)
  | A.MetaTy _ -> failwith "unreachable"

let rec a2c_exp : A.aexp -> C.exp = function
  | VarAExp x -> Var x
  | NilAExp -> failwith "nil is not supported in core"
  | BoolAExp b -> Const (Bool b)
  | IntAExp i -> Const (Int i)
  | AppAExp {fcn= fcn, _; arg= arg, _} ->
      App {fcn= a2c_exp fcn; arg= a2c_exp arg}
  | LamAExp {params; body= body, _} ->
      C.lams (List.map fst params) (a2c_exp body)
  | OpAExp {left= left, _; op; right= right, _} ->
      let oper =
        List.assoc op
          [ (PlusOp, C.Add); (MinusOp, C.Sub); (TimesOp, C.Mul)
          ; (DivideOp, C.Div); (EqOp, C.Eq); (LtOp, C.Lt); (LeOp, C.Le)
          ; (GtOp, C.Gt); (GeOp, C.Ge) ]
      in
      Prim {left= a2c_exp left; oper; right= a2c_exp right}
  | IfAExp {cond= cond, _; then_= then_, _; else_= else_, _} ->
      If {cond= a2c_exp cond; then_= a2c_exp then_; else_= a2c_exp else_}
  | LetAExp {bnds; body= body, _} ->
      let vars, bnds =
        List.split
          (List.map
             (fun (A.ABind {name; params; body= body, body_ty}) ->
               let param_tys = List.map (fun (_, ty) -> a2c_ty ty) params in
               tyctx := (name, C.fun_tys param_tys (a2c_ty body_ty)) :: !tyctx;
               (name, C.lams (List.map fst params) (a2c_exp body)) )
             bnds )
      in
      Let {isrec= false; vars; bnds; body= a2c_exp body}
  | LetrecAExp {bnds; body= body, _} ->
      let vars, bnds =
        List.split
          (List.map
             (fun (A.ABind {name; params; body= body, body_ty}) ->
               let param_tys = List.map (fun (_, ty) -> a2c_ty ty) params in
               tyctx := (name, C.fun_tys param_tys (a2c_ty body_ty)) :: !tyctx;
               (name, C.lams (List.map fst params) (a2c_exp body)) )
             bnds )
      in
      Let {isrec= true; vars; bnds; body= a2c_exp body}
