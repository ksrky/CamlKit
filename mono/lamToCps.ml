module L = Lambda.Syntax
module K = Cps.Syntax

let rec c2k_ty : L.ty -> K.ty = function
  | IntTy -> IntTy
  | BoolTy -> BoolTy
  | FunTy (ty1, ty2) -> ContTy [c2k_ty ty1; c2k_cont ty2]
  | _ -> failwith "TODO"

and c2k_cont ty : K.ty = ContTy [c2k_ty ty]

let c2k_var (id, ty) : K.var = (id, c2k_ty ty)

let rec c2k_exp (exp : L.exp) (k : K.valty) : K.exp =
  match exp with
  | Const (Int _ as c) -> K.mk_app k (K.Const c, IntTy)
  | Const (Bool _ as c) -> K.mk_app k (K.Const c, BoolTy)
  | Var (id, ty) -> K.mk_app k (K.Var id, c2k_ty ty)
  | App {fcn= fcn, fcn_ty; arg= arg, arg_ty} ->
      let fcn_ty' = c2k_ty fcn_ty in
      let arg_ty' = c2k_ty arg_ty in
      let fcn_id = Id.from_string "fcn" in
      let arg_id = Id.from_string "arg" in
      c2k_exp fcn
        ( K.mk_lam (fcn_id, fcn_ty')
            (c2k_exp arg
               ( K.mk_lam (arg_id, arg_ty')
                   (K.App
                      { fcn= (Var fcn_id, fcn_ty')
                      ; args= [(K.Var arg_id, arg_ty'); k] } )
               , ContTy [arg_ty'] ) )
        , ContTy [fcn_ty'] )
  | Lam {var; body= body, body_ty} ->
      let body_ty' = c2k_ty body_ty in
      let cont_id = Id.from_string "cont" in
      K.App
        { fcn= k
        ; args=
            [ ( K.mk_lams
                  [c2k_var var; (cont_id, K.ContTy [body_ty'])]
                  (c2k_exp body (Var cont_id, ContTy [body_ty']))
              , ContTy [c2k_ty (snd var); ContTy [body_ty']] ) ] }
  | Prim {left= left, left_ty; oper; right= right, right_ty} ->
      let prim_ty =
        match oper with
        | Add | Sub | Mul | Div -> K.IntTy
        | Eq | Ne | Lt | Le | Gt | Ge -> K.BoolTy
      in
      let prim_id = Id.from_string "prim" in
      let left_ty' = c2k_ty left_ty in
      let right_ty' = c2k_ty right_ty in
      let left_id = Id.from_string "left" in
      let right_id = Id.from_string "right" in
      c2k_exp left
        ( K.mk_lam (left_id, left_ty')
            (c2k_exp right
               ( K.mk_lam (right_id, right_ty')
                   (K.Let
                      { dec=
                          K.PrimDec
                            { var= (prim_id, prim_ty)
                            ; left= (Var left_id, left_ty')
                            ; oper
                            ; right= (Var right_id, right_ty') }
                      ; body= K.mk_app k (K.Var prim_id, prim_ty) } )
               , ContTy [right_ty'] ) )
        , ContTy [left_ty'] )
  | If {cond= cond, cond_ty; then_= then_, _; else_= else_, _} ->
      let cond_ty' = c2k_ty cond_ty in
      let cond_id = Id.from_string "cond" in
      c2k_exp cond
        ( K.mk_lam (cond_id, cond_ty')
            (K.If
               { cond= (Var cond_id, cond_ty')
               ; then_= c2k_exp then_ k
               ; else_= c2k_exp else_ k } )
        , ContTy [cond_ty'] )
  | Let {var; bnd; body= body, _} ->
      c2k_exp bnd
        (K.mk_lam (c2k_var var) (c2k_exp body k), ContTy [c2k_ty (snd var)])
  | Tuple _ -> raise Utils.Unreachable
  | Proj _ -> raise Utils.Unreachable

and c2k_def ({var; param; body= body, body_ty} : L.def) : K.def =
  let body_ty' = c2k_ty body_ty in
  let cont_id = Id.from_string "c" in
  { var= c2k_var var
  ; params= [c2k_var param; (cont_id, K.ContTy [body_ty'])]
  ; body= c2k_exp body (Var cont_id, K.ContTy [body_ty']) }

(* | Lam {var; body= body, body_ty}, _ ->
       let body_ty' = c2k_ty body_ty in
       let cont_id = Id.from_string "c" in
       { var= c2k_var name
       ; params= [c2k_var var; (cont_id, K.ContTy [body_ty'])]
       ; body= c2k_exp body (Var cont_id, K.ContTy [body_ty']) }
   | exp, exp_ty ->
       let exp_ty' = c2k_ty exp_ty in
       let cont_id = Id.from_string "c" in
       { var= c2k_var name
       ; params= [(cont_id, K.ContTy [exp_ty'])]
       ; body= c2k_exp exp (Var cont_id, K.ContTy [exp_ty']) } *)

let c2k_prog ((exp, ty) : L.prog) : K.prog =
  let exp_ty = c2k_ty ty in
  let prog_id = Id.from_string "prog" in
  c2k_exp exp
    (K.mk_lam (prog_id, exp_ty) (K.Halt (Var prog_id, exp_ty)), ContTy [exp_ty])
