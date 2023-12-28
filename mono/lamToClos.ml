module C = Core.Syntax
module Cl = Closure.Syntax

type escapes = Cl.var list

type locals = Cl.id list

let globals : Cl.id list ref = ref []

let rec c2cl_ty : C.ty -> Cl.ty = function
  | IntTy -> IntTy
  | BoolTy -> BoolTy
  | FunTy (ty1, ty2) -> FunTy (c2cl_ty ty1, c2cl_ty ty2)

let c2k_var (id, ty) : Cl.var = (id, c2cl_ty ty)

let rec c2cl_exp (escs : escapes) (lcls : locals) :
    C.expty -> Cl.expty * escapes = function
  | Const c, ty -> ((Const c, c2cl_ty ty), escs)
  | Var (x, _), ty -> ((Var x, c2cl_ty ty), escs)
  | App {fcn; arg}, ty -> (
      let fcn', escs1 = c2cl_exp escs lcls fcn in
      let arg', escs2 = c2cl_exp escs1 lcls arg in
      let res_ty = c2cl_ty ty in
      match snd fcn' with
      | ClosTy (code_ty, env_ty) ->
          let env_id = Id.from_string "env" in
          let code_id = Id.from_string "code" in
          let clos_id = Id.from_string "clos" in
          let clos_ty = Cl.ClosTy (code_ty, env_ty) in
          ( ( Let
                { isrec= false
                ; vars=
                    [(clos_id, clos_ty); (code_id, code_ty); (env_id, env_ty)]
                ; bnds=
                    [ fcn'; (Proj {tup= (Var clos_id, clos_ty); idx= 1}, code_ty)
                    ; (Proj {tup= (Var clos_id, clos_ty); idx= 2}, env_ty) ]
                ; body=
                    ( ClosApp
                        { fcn= (Var code_id, code_ty)
                        ; env= (Var env_id, env_ty)
                        ; arg= arg' }
                    , res_ty ) }
            , res_ty )
          , escs2 )
      | _ -> ((TailApp {fcn= fcn'; arg= arg'}, res_ty), escs2) )
  | Lam {var; body}, _ -> failwith "not implemented"
  | Fix _, _ -> failwith "not implemented"
  | Prim {left; oper; right}, ty ->
      let left', escs1 = c2cl_exp escs lcls left in
      let right', escs2 = c2cl_exp escs1 lcls right in
      ((Prim {left= left'; oper; right= right'}, c2cl_ty ty), escs2)
  | If {cond; then_; else_}, ty ->
      let cond', escs1 = c2cl_exp escs lcls cond in
      let then', escs2 = c2cl_exp escs1 lcls then_ in
      let else', escs3 = c2cl_exp escs2 lcls else_ in
      ((If {cond= cond'; then_= then'; else_= else'}, c2cl_ty ty), escs3)
  | Let {var; bnd; body}, _ -> failwith "not implemented"
  | Tuple _, _ -> failwith "not implemented"
  | Proj _, _ -> failwith "not implemented"
