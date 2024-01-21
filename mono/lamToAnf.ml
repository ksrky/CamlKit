module L = Lambda.Syntax
module A = Anf.Syntax

let rec l2a_ty : L.ty -> A.ty = function
  | IntTy -> IntTy
  | BoolTy -> BoolTy
  | FunTy (ty1, ty2) -> FunTy ([l2a_ty ty1], l2a_ty ty2)
  | TupleTy tys -> TupleTy (List.map l2a_ty tys)

let l2a_var (id, ty) : A.var = (id, l2a_ty ty)

type kont = A.valty -> A.exp

let rec l2a_exp ((exp, ty) : L.expty) (k : kont) : A.exp =
  match exp with
  | Const c -> k (A.Const c, l2a_ty ty)
  | Var (id, _) -> k (A.Var id, l2a_ty ty)
  | App {fcn; arg} ->
      l2a_exp fcn (fun fcn ->
          l2a_exp arg (fun arg -> k (Call {fcn; args= [arg]}, l2a_ty ty)) )
  | Lam {var; body} ->
      k
        ( Lam {vars= [l2a_var var]; body= l2a_expty body (fun x -> A.Ret x)}
        , l2a_ty ty )
  | Prim {left; oper; right} ->
      l2a_exp left (fun left ->
          l2a_exp right (fun right -> k (Prim {left; oper; right}, l2a_ty ty)) )
  | If {cond; then_; else_} ->
      l2a_exp cond (fun cond ->
          If {cond; then_= l2a_expty then_ k; else_= l2a_expty else_ k} )
  | Let {var; bnd; body} ->
      l2a_exp
        (bnd, snd var)
        (fun bind -> Let {var= l2a_var var; bind; body= l2a_expty body k})
  | Tuple _ -> raise Utils.Unreachable
  | Proj _ -> raise Utils.Unreachable

and l2a_expty (exp, ty) k = (l2a_exp (exp, ty) k, l2a_ty ty)

let l2a_prog (expty : L.prog) : A.prog = l2a_exp expty (fun x -> A.Ret x)
