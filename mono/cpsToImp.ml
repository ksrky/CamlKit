module K = Cps.Syntax
module CC = Cps.ClosConv
module I = Imp.Syntax

let c2i_const : K.const -> I.const = function
  | Int i -> I32 i
  | Bool b -> I1 (Bool.to_int b)

let rec c2i_ty : K.ty -> I.ty = function
  | IntTy -> I32Ty
  | BoolTy -> I1Ty
  | ContTy tys -> PtrTy (Some (FunTy (I.return_type, List.map c2i_ty tys)))
  | TupleTy tys -> PtrTy (Some (StrctTy (List.map c2i_ty tys)))
  | ExistsTy (id, ty) -> PtrTy (Some (StrctTy [PtrTy None; StrctTy [c2i_ty ty]]))

let c2i_var ((id, ty) : K.var) : I.var = (id, c2i_ty ty)

let rec c2i_val : K.valty -> I.dec list * I.value = function
  | Const c, _ -> ([], Const (c2i_const c))
  | Var x, ty -> ([], Var (c2i_var (x, ty)))
  | Glb f, ty -> ([], Glb (c2i_var (f, ty)))
  | Lam _, _ -> raise Utils.Unreachable
  | Tuple vals, _ ->
      let tuple_ty =
        I.PtrTy (Some (StrctTy (List.map (fun v -> c2i_ty (snd v)) vals)))
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
      let rec mk_tuple : int -> I.dec list = function
        | 0 -> [MallocDec {var= var0; len= List.length vals}]
        | i ->
            let di, vi = c2i_val (List.nth vals (i - 1)) in
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

let rec c2i_exp : K.exp -> I.exp = function
  | Let {dec= PrimDec {var; left; oper; right}; body}
    when List.mem oper [Eq; Ne; Lt; Le; Gt; Ge] ->
      let ds1, left' = c2i_val left in
      let ds2, right' = c2i_val right in
      let body' = c2i_exp body in
      I.mk_let (ds1 @ ds2)
        (I.If
           { oper=
               List.assoc oper
                 [ (Eq, I.Eq); (Ne, I.Ne); (Lt, I.Lt); (Le, I.Le); (Gt, I.Gt)
                 ; (Ge, I.Ge) ]
           ; left= left'
           ; right= right'
           ; then_=
               Let
                 { dec= ValDec {var= c2i_var var; val_= Const (I1 1)}
                 ; body= body' }
           ; else_=
               Let
                 { dec= ValDec {var= c2i_var var; val_= Const (I1 0)}
                 ; body= body' } } )
  | Let {dec; body} -> I.mk_let (c2i_dec dec) (c2i_exp body)
  | Letrec _ -> raise Utils.Unreachable
  | App {fcn; args} ->
      let ds, fcn' = c2i_val fcn in
      let dss, args' = List.split (List.map c2i_val args) in
      let var = (Id.from_string "apptmp", I.return_type) in
      I.mk_let
        (ds @ List.concat dss @ [CallDec {var; fcn= fcn'; args= args'}])
        (Return (Var var))
  | If {cond; then_; else_} ->
      let ds, cond' = c2i_val cond in
      let then' = c2i_exp then_ in
      let else' = c2i_exp else_ in
      I.mk_let ds
        (I.If
           { oper= Ne
           ; left= cond'
           ; right= Const (I1 0)
           ; then_= else'
           ; else_= then' } )
  | Halt val_ ->
      let ds, val' = c2i_val val_ in
      I.mk_let ds (Return val')

and c2i_dec : K.dec -> I.dec list = function
  | ValDec {var; val_} ->
      let ds, val' = c2i_val val_ in
      ds @ [ValDec {var= c2i_var var; val_= val'}]
  | PrimDec {var; left; oper; right} ->
      let ds1, left' = c2i_val left in
      let ds2, right' = c2i_val right in
      ds1 @ ds2
      @ [ PrimDec
            { var= c2i_var var
            ; left= left'
            ; oper=
                List.assoc oper
                  [(Add, I.Add); (Sub, I.Sub); (Mul, I.Mul); (Div, I.Div)]
            ; right= right' } ]
  | ProjDec {var; val_; idx} ->
      let ds, val' = c2i_val val_ in
      ds @ [SubscrDec {var= c2i_var var; val_= val'; idx}]
  | UnpackDec _ -> failwith "not implemented"

let c2i_heap ({var; params; body} : K.def) : I.heap =
  let params' = List.map (fun (x, ty) -> (x, c2i_ty ty)) params in
  Code {var= c2i_var var; params= params'; body= c2i_exp body}

let c2i_prog ((heaps, exp) : CC.prog) : I.prog =
  (List.map c2i_heap heaps, c2i_exp exp)
