module CS = Syntax

type exp =
  | Const of CS.const
  | Var of CS.id
  | App of {fcn: exp; arg: exp}
  | Prim of {oper: CS.oper; args: exp list}
  | Let of {isrec: bool; vars: CS.id list; bnds: exp list; body: exp}
  | If of {cond: exp; then_: exp; else_: exp}
  | Clos of clos
  | Select of {clos: clos; idx: int}

and clos =
  | CVar of CS.id
  | CAbs of {cvar: CS.id; var: CS.id; body: exp; env: escapes}

and escapes = CS.id list

type state = {escs: escapes; locs: CS.id list; cvar: CS.id}

let ppr_const : CS.const -> string = function
  | Int i -> string_of_int i
  | Nil -> "nil"

let rec ppr_exp (exp : exp) =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pexp ctx = function
    | Var var -> Id.unique_name var
    | Const c -> ppr_const c
    | App {fcn; arg} -> parens ctx 1 (pexp 1 fcn ^ "(" ^ pexp 0 arg ^ ")")
    | Prim {oper; args} ->
        CS.ppr_oper oper ^ "("
        ^ String.concat ", " (List.map (pexp 0) args)
        ^ ")"
    | If {cond; then_; else_} ->
        parens ctx 0
          ( "if " ^ pexp 0 cond ^ " then " ^ pexp 0 then_ ^ " else "
          ^ pexp 0 else_ )
    | Let {isrec; vars; bnds; body} ->
        parens ctx 0
          ( "let "
          ^ (if isrec then "rec " else "")
          ^ String.concat "; "
              (List.map2
                 (fun v e -> Id.unique_name v ^ " = " ^ pexp 0 e)
                 vars bnds )
          ^ " in " ^ pexp 0 body )
    | Clos clos -> ppr_clos clos
    | Select {clos; idx} -> ppr_clos clos ^ "#" ^ string_of_int idx
  in
  pexp 0 exp

and ppr_clos : clos -> string = function
  | CVar var -> Id.unique_name var
  | CAbs {cvar; var; body; env} ->
      "[fun " ^ Id.unique_name var ^ " -> " ^ ppr_exp body
      ^ String.concat ", " (List.map Id.unique_name env)
      ^ "]"

let ( // ) xs ys = List.filter (fun y -> not (List.mem y ys)) xs

let remove_dup xs =
  List.fold_right (fun x xs -> if List.mem x xs then xs else x :: xs) xs []

let lookup_env (escs : escapes) (x : CS.id) : int * escapes =
  let rec find_idx i = function
    | [] -> (i, escs @ [x])
    | y :: ys -> if x = y then (i, escs) else find_idx (i + 1) ys
  in
  find_idx 0 escs

let rec cc_exp : CS.exp -> exp = function
  | Const c -> Const c
  | Var x -> Var x
  | App {fcn; arg} -> App {fcn= cc_exp fcn; arg= cc_exp arg}
  | Lam {var; body} ->
      let cvar = Id.from_string "c" in
      let body', env = cc_loc {escs= []; locs= [var]; cvar} body in
      Clos (CAbs {cvar; var; body= body'; env})
  | Prim {oper; args} -> Prim {oper; args= List.map cc_exp args}
  | Let {isrec; vars; bnds; body} -> failwith "let is not supported"
  | If {cond; then_; else_} ->
      If {cond= cc_exp cond; then_= cc_exp then_; else_= cc_exp else_}

and cc_loc ({escs; locs; cvar} as st : state) : CS.exp -> exp * escapes =
  function
  | Const c -> (Const c, escs)
  | Var x when List.mem x locs -> (Var x, escs)
  | Var x ->
      let idx, env' = lookup_env escs x in
      (Select {clos= CVar cvar; idx}, env')
  | App {fcn; arg} ->
      let fcn', escs1 = cc_loc st fcn in
      let arg', escs2 = cc_loc {escs= escs1; locs; cvar} arg in
      (App {fcn= fcn'; arg= arg'}, escs2)
  | Lam {var; body} ->
      let cvar' = Id.from_string "c" in
      let body', escs' = cc_loc {escs= []; locs= [var]; cvar= cvar'} body in
      (Clos (CAbs {cvar= cvar'; var; body= body'; env= escs'}), escs @ escs')
  | Prim {oper; args} ->
      let args', env' = cc_loc_seq st args in
      (Prim {oper; args= args'}, env')
  | Let {isrec; vars; bnds; body} ->
      let bnds', escs1 = cc_loc_seq st bnds in
      let body', escs2 = cc_loc {escs= escs1; locs; cvar} body in
      (Let {isrec; vars; bnds= bnds'; body= body'}, escs2)
  | If {cond; then_; else_} ->
      let cond', escs1 = cc_loc st cond in
      let then', escs2 = cc_loc {escs= escs1; locs; cvar} then_ in
      let else', escs3 = cc_loc {escs= escs2; locs; cvar} else_ in
      (If {cond= cond'; then_= then'; else_= else'}, escs3)

and cc_loc_seq ({escs; locs; cvar} : state) (exps : CS.exp list) :
    exp list * escapes =
  let rec loop acc env = function
    | [] -> (List.rev acc, env)
    | exp :: exps ->
        let exp', env' = cc_loc {escs; locs; cvar} exp in
        loop (exp' :: acc) env' exps
  in
  loop [] escs exps

let cc_prog (exp : CS.exp) : exp = cc_exp exp
