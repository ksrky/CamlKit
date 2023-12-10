module C = Cps.Syntax
module CC = Cps.ClosConv
module I = Imp.Syntax

let c2i_const : C.const -> int = function Int i -> i | Nil -> 0

let rec c2i_val : CC.value -> I.dec list * I.value = function
  | Const c -> ([], Const (c2i_const c))
  | Var x -> ([], Var x)
  | Glb x -> ([], Glb x)
  | Lam _ -> failwith "unreachable"
  | Tuple vals ->
      let var0 = Id.from_string "y0" in
      let vars =
        var0
        :: List.mapi
             (fun i _ -> Id.from_string ("y" ^ string_of_int (i + 1)))
             vals
      in
      let decs = ref [] in
      let rec mk_tuple : int -> I.dec list = function
        | 0 -> [MallocDec {name= var0; len= List.length vals}]
        | i ->
            let di, vi = c2i_val (List.nth vals (i - 1)) in
            decs := di @ !decs;
            UpdateDec
              { name= List.nth vars i
              ; var= List.nth vars (i - 1)
              ; idx= i
              ; val_= vi }
            :: mk_tuple (i - 1)
      in
      ( !decs @ List.rev (mk_tuple (List.length vals))
      , Var (List.hd (List.rev vars)) )

let rec c2i_exp : CC.exp -> I.exp = function
  | Let {dec= PrimDec {name; left; oper; right}; body}
    when List.mem oper [Eq; Ne; Lt; Le; Gt; Ge] ->
      let ds1, left' = c2i_val left in
      let ds2, right' = c2i_val right in
      let body' = c2i_exp body in
      I.let_decs (ds1 @ ds2)
        (I.If
           { oper=
               List.assoc oper
                 [ (Eq, I.Eq); (Ne, I.Ne); (Lt, I.Lt); (Le, I.Le); (Gt, I.Gt)
                 ; (Ge, I.Ge) ]
           ; left= left'
           ; right= right'
           ; then_= I.Let {dec= ValDec {name; val_= I.Const 1}; body= body'}
           ; else_= I.Let {dec= ValDec {name; val_= I.Const 0}; body= body'} }
        )
  | Let {dec; body} -> I.let_decs (c2i_dec dec) (c2i_exp body)
  | Letrec _ -> failwith "unreachable"
  | App {fcn; args} ->
      let ds, fcn' = c2i_val fcn in
      let dss, args' = List.split (List.map c2i_val args) in
      I.let_decs (ds @ List.concat dss) (App {fcn= fcn'; args= args'})
  | If {cond; then_; else_} ->
      let ds, cond' = c2i_val cond in
      let then' = c2i_exp then_ in
      let else' = c2i_exp else_ in
      I.let_decs ds
        (I.If
           { oper= I.Ne
           ; left= cond'
           ; right= I.Const 0
           ; then_= else'
           ; else_= then' } )
  | Halt val_ ->
      let ds, val' = c2i_val val_ in
      I.let_decs ds (Halt val')

and c2i_dec : CC.dec -> I.dec list = function
  | ValDec {name; val_} ->
      let ds, val' = c2i_val val_ in
      ds @ [ValDec {name; val_= val'}]
  | PrimDec {name; left; oper; right} ->
      let ds1, left' = c2i_val left in
      let ds2, right' = c2i_val right in
      ds1 @ ds2
      @ [ PrimDec
            { name
            ; left= left'
            ; oper=
                List.assoc oper
                  [(Add, I.Add); (Sub, I.Sub); (Mul, I.Mul); (Div, I.Div)]
            ; right= right' } ]
  | ProjDec {name; val_; idx} ->
      let ds, val' = c2i_val val_ in
      ds @ [ProjDec {name; val_= val'; idx}]

let c2i_heap ({name; vars; body} : CC.fundef) : I.heap =
  I.Code {name; vars; body= c2i_exp body}

let c2i_prog ((heaps, exp) : CC.prog) : I.prog =
  (List.map c2i_heap heaps, c2i_exp exp)
