module C = Cps.Syntax
module CC = Cps.ClosConv
module I = Imp.Syntax

let ( +> ) e s = I.ESEQ (e, s)

let ( <+> ) s1 s2 = I.SEQ (s1, s2)

let c2i_const : C.const -> int = function Int i -> i | Nil -> 0

let c2i_val : CC.value -> I.exp = function
  | Const c -> CONST (c2i_const c)
  | Var x -> VAR x
  | Clos _ -> failwith ""
  | Select _ -> failwith ""

type exp_stm = E of I.exp | S of I.stm

let unE : exp_stm -> I.exp = function E e -> e | S s -> ESEQ (s, CONST 0)

let unS : exp_stm -> I.stm = function E e -> EXP e | S s -> s

let rec c2i_exp : CC.exp -> exp_stm = function
  | Let {dec; body} -> S (unS (c2i_dec dec) <+> unS (c2i_exp body))
  | Letrec _ -> failwith ""
  | ClosApp {fcn; args} -> E (CALL (c2i_val fcn, List.map c2i_val args))
  | If {cond; then_; else_} ->
      S (IF (c2i_val cond, c2i_exp then_ |> unE, c2i_exp else_ |> unE))
  | Halt val_ -> S (EXIT (c2i_val val_))

and c2i_dec : CC.dec -> exp_stm = function
  | ValDec {name; val_} -> S (ASSIGN (name, c2i_val val_))
  | PrimDec {name; oper; args} -> failwith ""

and c2i_clos : CC.clos -> I.exp = function
  | CVar x -> VAR x
  | CLam {cvar; vars; body; env} ->
      let env_var = Id.from_string "env" in
      I.ASSIGN (env_var, I.MALLOC (List.map (fun _ -> I.INT) env))
      +> failwith ""

and c2i_func (vars : C.id list) (exp : C.exp) : I.exp = failwith ""
