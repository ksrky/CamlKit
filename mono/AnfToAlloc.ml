module CC = Anf.ClosConv
module Anf = Anf.Syntax
module A = Alloc.Syntax

let a2a_const : Anf.const -> A.const = function
  | Int i -> I32 i
  | Bool b -> I1 (Bool.to_int b)

let rec a2a_ty : Anf.ty -> A.ty = function
  | IntTy -> I32Ty
  | BoolTy -> I1Ty
  | FunTy (tys1, ty2) -> PtrTy (Some (FunTy (a2a_ty ty2, List.map a2a_ty tys1)))
  | TupleTy tys -> PtrTy (Some (StrctTy (List.map a2a_ty tys)))
  | CodeTy (ty1, tys2, ty3) ->
      PtrTy (Some (FunTy (a2a_ty ty3, List.map a2a_ty (ty1 :: tys2))))

let a2a_var ((id, ty) : Anf.var) : A.var = (id, a2a_ty ty)

let rec a2a_val : Anf.valty -> A.dec list * A.value = function
  | Const c, _ -> ([], Const (a2a_const c))
  | Var x, ty -> ([], Var (a2a_var (x, ty)))
  | Glb f, ty -> ([], Glb (a2a_var (f, ty)))
  | Lam _, _ -> raise Utils.Unreachable
  | Tuple vals, _ ->
      let tuple_ty =
        A.PtrTy (Some (StrctTy (List.map (fun v -> a2a_ty (snd v)) vals)))
      in
      let var0 = (Id.from_string "y0", tuple_ty) in
      let vars =
        var0
        :: List.mapi
             (fun i _ ->
               (Id.from_string ("y" ^ string_of_int (i + 1)), tuple_ty) )
             vals
      in
      let decs = ref [] in
      let rec mk_tuple : int -> A.dec list = function
        | 0 -> [MallocDec {var= var0; len= List.length vals}]
        | i ->
            let di, vi = a2a_val (List.nth vals (i - 1)) in
            decs := di @ !decs;
            UpdateDec
              { var= List.nth vars i
              ; strct= Var (List.nth vars (i - 1))
              ; idx= i
              ; val_= vi }
            :: mk_tuple (i - 1)
      in
      ( !decs @ List.rev (mk_tuple (List.length vals))
      , Var (List.hd (List.rev vars)) )

let rec a2a_exp : Anf.exp -> A.exp = function
  | Let {dec= PrimDec {var; left; oper; right}; body}
    when List.mem oper [Eq; Ne; Lt; Le; Gt; Ge] ->
      let ds1, left' = a2a_val left in
      let ds2, right' = a2a_val right in
      let body' = a2a_expty body in
      A.mk_let (ds1 @ ds2)
        (A.If
           { oper=
               List.assoc oper
                 [ (Eq, A.Eq); (Ne, A.Ne); (Lt, A.Lt); (Le, A.Le); (Gt, A.Gt)
                 ; (Ge, A.Ge) ]
           ; left= left'
           ; right= right'
           ; then_=
               Let
                 { dec= ValDec {var= a2a_var var; val_= Const (I1 1)}
                 ; body= body' }
           ; else_=
               Let
                 { dec= ValDec {var= a2a_var var; val_= Const (I1 0)}
                 ; body= body' } } )
  | Let {dec; body} -> A.mk_let (a2a_dec dec) (a2a_expty body)
  | If {cond; then_; else_} ->
      let ds, cond' = a2a_val cond in
      let then' = a2a_expty then_ in
      let else' = a2a_expty else_ in
      A.mk_let ds
        (A.If
           { oper= Ne
           ; left= cond'
           ; right= Const (I1 0)
           ; then_= else'
           ; else_= then' } )
  | Ret val_ ->
      let ds, val' = a2a_val val_ in
      A.mk_let ds (Return val')

and a2a_expty ((exp, _) : Anf.expty) : A.exp = a2a_exp exp

and a2a_dec : Anf.dec -> A.dec list = function
  | ValDec {var; val_} ->
      let ds, val' = a2a_val val_ in
      ds @ [ValDec {var= a2a_var var; val_= val'}]
  | CallDec {var; fcn; args} ->
      let ds, fcn' = a2a_val fcn in
      let dss, args' = List.split (List.map a2a_val args) in
      ds @ List.concat dss @ [CallDec {var= a2a_var var; fcn= fcn'; args= args'}]
  | PrimDec {var; left; oper; right} ->
      let ds1, left' = a2a_val left in
      let ds2, right' = a2a_val right in
      ds1 @ ds2
      @ [ PrimDec
            { var= a2a_var var
            ; left= left'
            ; oper=
                List.assoc oper
                  [(Add, A.Add); (Sub, A.Sub); (Mul, A.Mul); (Div, A.Div)]
            ; right= right' } ]
  | ProjDec {var; val_; idx} ->
      let ds, val' = a2a_val val_ in
      ds @ [SubscrDec {var= a2a_var var; val_= val'; idx}]

let a2a_def : Anf.def -> A.heap = function
  | {var; env= None; params; body} ->
      let params' = List.map (fun (x, ty) -> (x, a2a_ty ty)) params in
      Code {var= a2a_var var; params= params'; body= a2a_expty body}
  | {var; env= Some env; params; body} ->
      let params' = List.map (fun (x, ty) -> (x, a2a_ty ty)) (env :: params) in
      Code {var= a2a_var var; params= params'; body= a2a_expty body}

let a2a_prog ((defs, expty) : CC.prog) : A.prog =
  (List.map a2a_def defs, a2a_expty expty)
