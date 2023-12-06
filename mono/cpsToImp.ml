module C = Cps.Syntax
module H = Cps.Hoisting
module I = Imp.Syntax

let c2i_const : C.const -> int = function Int i -> i | Nil -> 0

let rec c2i_val : H.value -> I.dec list * I.value = function
  | Const c -> ([], Const (c2i_const c))
  | Var x -> ([], Var x)
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
      (!decs @ mk_tuple (List.length vals), Var (List.hd (List.rev vars)))

let rec c2i_exp : H.exp -> I.exp = function
  | Let {dec; body} -> I.let_decs (c2i_dec dec) (c2i_exp body)
  | App {fcn; args} ->
      let ds, fcn' = c2i_val fcn in
      let dss, args' = List.split (List.map c2i_val args) in
      I.let_decs (ds @ List.concat dss) (App {fcn= fcn'; args= args'})
  | If {cond; then_; else_} ->
      let cond' = c2i_val cond in
      let then' = c2i_exp then_ in
      let else' = c2i_exp else_ in
      failwith ""
  | Halt val_ ->
      let ds, val' = c2i_val val_ in
      I.let_decs ds (Halt val')

and c2i_dec : H.dec -> I.dec list = function
  | ValDec {name; val_} ->
      let ds, val' = c2i_val val_ in
      ds @ [ValDec {name; val_= val'}]
  | PrimDec {name; oper; args} -> (
      let dss, args' = List.split (List.map c2i_val args) in
      List.concat dss
      @
      match oper with
      | Add -> [PrimDec {name; oper= Add; args= args'}]
      | Sub -> [PrimDec {name; oper= Sub; args= args'}]
      | Mul -> [PrimDec {name; oper= Mul; args= args'}]
      | Div -> [PrimDec {name; oper= Div; args= args'}]
      | Eq -> failwith ""
      | Ne -> failwith ""
      | Lt -> failwith ""
      | Le -> failwith ""
      | Gt -> failwith ""
      | Ge -> failwith "" )
  | ProjDec {name; val_; idx} ->
      let ds, val' = c2i_val val_ in
      ds @ [ProjDec {name; val_= val'; idx}]

let c2i_heap ({name; vars; body} : H.code) : I.heap =
  I.Code {name; vars; body= c2i_exp body}

let c2i_prog ((heaps, exp) : H.prog) : I.prog =
  (List.map c2i_heap heaps, c2i_exp exp)
