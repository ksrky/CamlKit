module K = Cps.Syntax
module CC = Cps.ClosConv
module A = Alloc.Syntax

let k2i_const : K.const -> A.const = function
  | Int i -> I32 i
  | Bool b -> I1 (Bool.to_int b)

let rec k2i_ty : K.ty -> A.ty = function
  | IntTy -> I32Ty
  | BoolTy -> I1Ty
  | ContTy tys -> PtrTy (Some (FunTy (A.return_type, List.map k2i_ty tys)))
  | TupleTy tys -> PtrTy (Some (StrctTy (List.map k2i_ty tys)))
  | ExistsTy (id, ty) -> PtrTy (Some (StrctTy [PtrTy None; StrctTy [k2i_ty ty]]))

let k2i_var ((id, ty) : K.var) : A.var = (id, k2i_ty ty)

let rec k2i_val : K.valty -> A.dec list * A.value = function
  | Const c, _ -> ([], Const (k2i_const c))
  | Var x, ty -> ([], Var (k2i_var (x, ty)))
  | Glb f, ty -> ([], Glb (k2i_var (f, ty)))
  | Lam _, _ -> raise Utils.Unreachable
  | Tuple vals, _ ->
      let tuple_ty =
        A.PtrTy (Some (StrctTy (List.map (fun v -> k2i_ty (snd v)) vals)))
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
            let di, vi = k2i_val (List.nth vals (i - 1)) in
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
  | Pack _, _ -> failwith "not implemented"

let rec k2i_exp : K.exp -> A.exp = function
  | Let {dec= PrimDec {var; left; oper; right}; body}
    when List.mem oper [Eq; Ne; Lt; Le; Gt; Ge] ->
      let ds1, left' = k2i_val left in
      let ds2, right' = k2i_val right in
      let body' = k2i_exp body in
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
                 { dec= ValDec {var= k2i_var var; val_= Const (I1 1)}
                 ; body= body' }
           ; else_=
               Let
                 { dec= ValDec {var= k2i_var var; val_= Const (I1 0)}
                 ; body= body' } } )
  | Let {dec; body} -> A.mk_let (k2i_dec dec) (k2i_exp body)
  | App {fcn; args} ->
      let ds, fcn' = k2i_val fcn in
      let dss, args' = List.split (List.map k2i_val args) in
      let var = (Id.from_string "apptmp", A.return_type) in
      A.mk_let
        (ds @ List.concat dss @ [CallDec {var; fcn= fcn'; args= args'}])
        (Return (Var var))
  | If {cond; then_; else_} ->
      let ds, cond' = k2i_val cond in
      let then' = k2i_exp then_ in
      let else' = k2i_exp else_ in
      A.mk_let ds
        (A.If
           { oper= Ne
           ; left= cond'
           ; right= Const (I1 0)
           ; then_= else'
           ; else_= then' } )
  | Halt val_ ->
      let ds, val' = k2i_val val_ in
      A.mk_let ds (Return val')

and k2i_dec : K.dec -> A.dec list = function
  | ValDec {var; val_} ->
      let ds, val' = k2i_val val_ in
      ds @ [ValDec {var= k2i_var var; val_= val'}]
  | PrimDec {var; left; oper; right} ->
      let ds1, left' = k2i_val left in
      let ds2, right' = k2i_val right in
      ds1 @ ds2
      @ [ PrimDec
            { var= k2i_var var
            ; left= left'
            ; oper=
                List.assoc oper
                  [(Add, A.Add); (Sub, A.Sub); (Mul, A.Mul); (Div, A.Div)]
            ; right= right' } ]
  | ProjDec {var; val_; idx} ->
      let ds, val' = k2i_val val_ in
      ds @ [SubscrDec {var= k2i_var var; val_= val'; idx}]
  | UnpackDec _ -> failwith "not implemented"

let k2i_heap ({var; params; body} : K.def) : A.heap =
  let params' = List.map (fun (x, ty) -> (x, k2i_ty ty)) params in
  Code {var= k2i_var var; params= params'; body= k2i_exp body}

let k2i_prog ((heaps, exp) : CC.prog) : A.prog =
  (List.map k2i_heap heaps, k2i_exp exp)
