module C = Cps.Syntax
module H = Cps.Hoisting
module I = Imp.Syntax

let ( +> ) e s = I.ESEQ (e, s)

let ( <+> ) s1 s2 = I.SEQ (s1, s2)

let c2i_const : C.const -> int = function Int i -> i | Nil -> 0

let rec c2i_val : H.value -> I.exp = function
  | Const c -> CONST (c2i_const c)
  | Var x -> VAR x
  | Tuple vals ->
      let vars =
        Id.from_string "y0"
        :: List.mapi
             (fun i _ -> Id.from_string ("y" ^ string_of_int (i + 1)))
             vals
      in
      let tys = List.map (fun _ -> I.INT) vals (* tmp *) in
      let rec mk_tuple : int -> I.stm = function
        | i when i <= 0 -> ASSIGN (Id.from_string "y0", MALLOC tys)
        | i ->
            mk_tuple (i - 1)
            <+> I.ASSIGN
                  ( List.nth vars i
                  , I.UPDATE
                      ( I.VAR (List.nth vars (i - 1))
                      , i
                      , c2i_val (List.nth vals (i - 1)) ) )
      in
      mk_tuple (List.length vals) +> VAR (List.hd (List.rev vars))

type exp_stm = E of I.exp | S of I.stm

let unE : exp_stm -> I.exp = function E e -> e | S s -> ESEQ (s, CONST 0)

let unS : exp_stm -> I.stm = function E e -> EXP e | S s -> s

let rec c2i_exp : H.exp -> exp_stm = function
  | Let {dec; body} -> S (unS (c2i_dec dec) <+> unS (c2i_exp body))
  | App {fcn; args} -> E (CALL (c2i_val fcn, List.map c2i_val args))
  | If {cond; then_; else_} ->
      S (IF (c2i_val cond, c2i_exp then_ |> unE, c2i_exp else_ |> unE))
  | Halt val_ -> S (EXIT (c2i_val val_))

and c2i_dec : H.dec -> exp_stm = function
  | ValDec {name; val_} -> S (ASSIGN (name, c2i_val val_))
  | PrimDec {name; oper; args= [arg1; arg2]} ->
      E (BINOP (c2i_oper oper, c2i_val arg1, c2i_val arg2))
  | PrimDec _ -> failwith "no prim"
  | ProjDec {name; val_; idx} -> S (ASSIGN (name, PROJ (c2i_val val_, idx)))

and c2i_oper : C.oper -> I.binop = function
  | Add -> PLUS
  | Sub -> MINUS
  | Mul -> TIMES
  | Div -> DIVIDE
  | _ -> failwith "not implemented"

and c2i_func (vars : C.id list) (exp : C.exp) : I.exp = failwith ""
