module I = IntSyn
module IT = Ident.Table

(** the maximum contraction steps. *)
let max_steps = 8

(** the minimum ratio of current reduction counts to previous reduction counts *)
let thresh_ratio = 0.95 (* tmp *)

(** number of reductions performed in one step. *)
let nred = ref 0

let namefunc = ref IT.empty

let extend_namefunc id exp = namefunc := IT.add id exp !namefunc

let find_namefunc id : I.exp =
  match IT.find_opt id !namefunc with Some e -> (* inlining *) incr nred; e | None -> Var id

let maybe_simple id : I.exp option =
  match IT.find_opt id !namefunc with
  | Some e -> ( match e with Int _ | Nil | Var _ -> incr nred; Some e | _ -> None )
  | None -> None

let varcount : int ref IT.t ref = ref IT.empty

let extend_varcount ids : unit = List.iter (fun id -> varcount := IT.add id (ref 0) !varcount) ids

let incr_varcount id : unit = incr (IT.find id !varcount)

let is_varcount id cnt : bool = !(IT.find id !varcount) = cnt

let init () =
  nred := 0;
  namefunc := IT.empty;
  varcount := IT.empty

let rec gather : I.exp -> unit = function
  | Var id -> incr_varcount id
  | App (Lam (vars, body), args) ->
      (* (fun x1 x2 .. xn -> e) a1 a2 .. ak *)
      (try List.iter2 extend_namefunc vars args with Invalid_argument _ (* n > k *) -> ());
      extend_varcount vars; gather body; List.iter gather args
  | App (fcn, args) -> gather fcn; List.iter gather args
  | Lam (vars, body) -> extend_varcount vars; gather body
  | Builtin (_, args) -> List.iter gather args
  | Let (_, vars, bnds, body) ->
      List.iter2 extend_namefunc vars bnds;
      extend_varcount vars;
      List.iter gather bnds;
      gather body
  | If (test, then_, else_) -> gather test; gather then_; gather else_
  | _ -> ()

let rec reduce : I.exp -> I.exp = function
  | Var id -> (
    match maybe_simple id with
    | Some e -> e
    | None when is_varcount id 1 -> find_namefunc id
    | None -> Var id )
  | App (Lam (vars, body), args) ->
      let vars', args' =
        List.split
          (List.fold_right2
             (fun v a acc ->
               if is_varcount v 0 then (
                 (* dead-variable elimination *) incr nred;
                 acc )
               else (v, reduce a) :: acc )
             vars args [] )
      in
      if vars' = [] then reduce body else App (Lam (vars', reduce body), args')
  | App (fcn, args) -> App (reduce fcn, List.map reduce args)
  | Lam (vars, body) -> Lam (vars, reduce body)
  | Builtin (fcn, args) -> Builtin (fcn, List.map reduce args)
  | Let (isrec, vars, bnds, body) ->
      let vars', bnds' =
        List.split
          (List.fold_right2
             (fun v b acc ->
               if is_varcount v 0 then (
                 (* dead-variable elimination *) incr nred;
                 acc )
               else if is_varcount v 1 then
                 (v, b) :: acc (* not expand a let binding when it can be inlined. *)
               else (v, reduce b) :: acc )
             vars bnds [] )
      in
      if vars' = [] then reduce body else Let (isrec, vars', bnds', reduce body)
  | e -> e

let step exp : I.exp = init (); gather exp; reduce exp

let rec steps (n : int) (exp : I.exp) =
  if n = 0 then exp
  else
    let prev = !nred in
    let exp' = step exp in
    let next = prev = 0 || float_of_int !nred /. float_of_int prev > thresh_ratio in
    if next then steps (n - 1) exp' else exp'
