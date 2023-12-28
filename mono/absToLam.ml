module A = Abstract.Syntax
module C = Lambda.Syntax

let rec a2c_ty : A.ty -> C.ty = function
  | A.NilTy -> raise Utils.Unreachable
  | A.BoolTy -> C.BoolTy
  | A.IntTy -> C.IntTy
  | A.FunTy (t1, t2) -> C.FunTy (a2c_ty t1, a2c_ty t2)
  | A.MetaTy _ -> raise Utils.Unreachable

let a2c_var ((id, ty) : A.var) : C.var = (id, a2c_ty ty)

let lambda_ty (params : A.var list) (body_ty : A.ty) : C.ty =
  C.fun_tys (List.map (fun (_, ty) -> a2c_ty ty) params) (a2c_ty body_ty)

let rec a2c_exp : A.aexp -> C.exp = function
  | VarAExp x -> Var (a2c_var x)
  | NilAExp -> raise Utils.Unreachable
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
      let decs =
        List.map
          (fun (A.ABind {name; params; body}) ->
            ( (name, lambda_ty params (snd body))
            , C.lams (List.map a2c_var params) (a2c_expty body) |> fst ) )
          bnds
      in
      C.mk_let decs (a2c_expty body) |> fst
  | LetrecAExp {bnds; body} ->
      let vars, bnds =
        List.split
          (List.map
             (function
               | A.ABind {params= []; _} ->
                   Format.eprintf
                     "right-hand side of let rec must be a function";
                   raise Utils.Bug_error
               | A.ABind {name; params= p :: ps; body} ->
                   ( (name, lambda_ty (p :: ps) (snd body))
                   , C.lams (List.map a2c_var ps) (a2c_expty body) ) )
             bnds )
      in
      let tuple_ty = C.TupleTy (List.map snd vars) in
      let fix_var = (Id.from_string "ff", C.FunTy (tuple_ty, tuple_ty)) in
      let arg_var = (Id.from_string "x", tuple_ty) in
      let let_var = (Id.from_string "r", tuple_ty) in
      let bnds' =
        List.map
          (C.subst
             (List.mapi
                (fun idx (id, _) ->
                  (id, C.Proj {tup= (Var fix_var, snd fix_var); idx}) )
                vars ) )
          bnds
      in
      let decs =
        ( let_var
        , C.Fix {name= fix_var; var= arg_var; body= (Tuple bnds', tuple_ty)} )
        :: List.mapi
             (fun i x -> (x, C.Proj {tup= (Var let_var, tuple_ty); idx= i + 1}))
             vars
      in
      C.mk_let decs (a2c_expty body) |> fst

and a2c_expty ((exp, ty) : A.expty) : C.expty = (a2c_exp exp, a2c_ty ty)

let a2c_prog : A.aprog -> C.prog = a2c_expty
