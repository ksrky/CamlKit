module L = Lambda.Syntax
module A = Anf.Syntax

let rec l2a_ty : L.ty -> A.ty = function
  | IntTy -> IntTy
  | BoolTy -> BoolTy
  | FunTy (ty1, ty2) -> FunTy ([l2a_ty ty1], l2a_ty ty2)
  | TupleTy tys -> TupleTy (List.map l2a_ty tys)

let l2a_var (id, ty) : A.var = (id, l2a_ty ty)

type kont = A.valty -> A.expty

let rec l2a_exp ((exp, ty) : L.expty) (k : kont) : A.expty =
  match exp with
  | Const c -> k (A.Const c, l2a_ty ty)
  | Var (id, _) -> k (A.Var id, l2a_ty ty)
  | App {fcn; arg} ->
      let call_id = Id.from_string "call" in
      let call_ty = l2a_ty ty in
      l2a_exp fcn (fun fcn ->
          l2a_exp arg (fun arg ->
              let body = k (Var call_id, call_ty) in
              ( Let
                  { dec= CallDec {var= (call_id, call_ty); fcn; args= [arg]}
                  ; body }
              , snd body ) ) )
  | Lam {var; body} ->
      k
        ( Lam
            {vars= [l2a_var var]; body= l2a_exp body (fun x -> (A.Ret x, snd x))}
        , l2a_ty ty )
  | Prim {left; oper; right} ->
      let prim_id = Id.from_string "prim" in
      let prim_ty = l2a_ty ty in
      l2a_exp left (fun left ->
          l2a_exp right (fun right ->
              let body = k (Var prim_id, prim_ty) in
              ( Let
                  { dec= PrimDec {var= (prim_id, prim_ty); left; oper; right}
                  ; body }
              , snd body ) ) )
  | If {cond; then_; else_} ->
      l2a_exp cond (fun cond ->
          let then_ = l2a_exp then_ k in
          (If {cond; then_; else_= l2a_exp else_ k}, snd then_) )
  | Let {var; bnd; body} ->
      l2a_exp
        (bnd, snd var)
        (fun val_ ->
          let body = l2a_exp body k in
          (Let {dec= ValDec {var= l2a_var var; val_}; body}, snd body) )
  | Tuple _ -> raise Utils.Unreachable
  | Proj _ -> raise Utils.Unreachable

let l2a_prog (expty : L.prog) : A.prog =
  l2a_exp expty (fun x -> (A.Ret x, snd x)) |> fst
