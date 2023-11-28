type exp =
  | EVar of string
  | EApp of exp * exp
  | EAbs of string * exp
  | ELet of string * exp * exp

type term =
  | TVar of string
  | TSelect of int * clos
  | TClos of clos
  | TApp of term * term

and clos = CVar of string | CAbs of string * string * term * env

and env = string list

type state = env * string * string

let lookup_env (env : env) (x : string) : int * env =
  let rec find_idx i = function
    | [] -> (i, env @ [x])
    | y :: ys -> if x = y then (i, env) else find_idx (i + 1) ys
  in
  find_idx 0 env

let rec conv_top : exp -> term = function
  | EVar x -> TVar x
  | EApp (e1, e2) -> TApp (conv_top e1, conv_top e2)
  | EAbs (x, e) ->
      let c = "c0" in
      let t, env = conv_loc ([], x, c) e in
      TClos (CAbs (c, x, t, env))
  | ELet (x, e1, e2) ->
      let c = "c1" in
      let t1 = conv_top e1 in
      let t2, env = conv_loc ([], x, c) e2 in
      TApp (TClos (CAbs (c, x, t2, env)), t1)

and conv_loc ((env, x, c) : state) : exp -> term * env = function
  | EVar y when x = y -> (TVar y, env)
  | EVar y ->
      let i, env' = lookup_env env y in
      (TSelect (i, CVar c), env')
  | EApp (e1, e2) ->
      let t1, env1 = conv_loc (env, x, c) e1 in
      let t2, env2 = conv_loc (env1, x, c) e2 in
      (TApp (t1, t2), env2)
  | EAbs (y, e) ->
      let c' = "c2" in
      let t, env' = conv_loc ([], y, c') e in
      (TClos (CAbs (c', y, t, env')), env @ env')
  | ELet (y, e1, e2) ->
      let c' = "c3" in
      let t1, env1 = conv_loc (env, x, c') e1 in
      let t2, env2 = conv_loc (env1, x, c) e2 in
      (TApp (TClos (CAbs (c', y, t2, env2)), t1), env1 @ env2)

let conv : state option -> exp -> term = function
  | None -> conv_top
  | Some st -> fun e -> conv_loc st e |> fst
