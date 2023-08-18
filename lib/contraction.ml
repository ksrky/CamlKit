module I = IntSyn
module IT = Ident.Table

(** the maximum contraction steps. *)
let max_steps : int = 8

(** the minimum ratio of current reduction counts to previous reduction counts *)
let thresh_ratio : float = 0.95 (* tmp *)

(** number of reductions performed in one step. *)
let nred = ref 1

type varinfo =
  { mutable side_effect: bool
  ; mutable usecount: int
  ; mutable repres: I.exp
  ; mutable simple: bool
  ; mutable isrec: bool }

let default_info () = {side_effect= false; usecount= 0; repres= I.Nil; simple= false; isrec= false}

let hashtbl : (int, varinfo) Hashtbl.t = Hashtbl.create ~random:true 4096

let hashtbl_find (id : Ident.t) : varinfo =
  try Hashtbl.find hashtbl (Ident.unique id)
  with Not_found -> ErrorMsg.impossible "Unknown identifier at hashtbl_find"

let init_varinfo ?(isrec = false) ids : unit =
  List.iter
    (fun id ->
      let info = default_info () in
      info.isrec <- isrec;
      Hashtbl.add hashtbl (Ident.unique id) info )
    ids

let incr_usecount id : unit =
  let info = hashtbl_find id in
  info.usecount <- info.usecount + 1

let decr_usecount id : unit =
  let info = hashtbl_find id in
  info.usecount <- info.usecount - 1

let is_usecount id cnt : bool = (hashtbl_find id).usecount = cnt

let update_repres id exp =
  let info = hashtbl_find id in
  info.repres <- exp;
  match exp with Int _ | Nil | Var _ -> info.simple <- true | _ -> info.simple <- false

let find_repres (id : Ident.t) : I.exp =
  match Hashtbl.find_opt hashtbl (Ident.unique id) with
  | Some info -> (* inlining *) incr nred; decr_usecount id; info.repres
  | None -> Var id

let is_deadvar (id : Ident.t) =
  let info = hashtbl_find id in
  info.usecount = 0 && not info.side_effect

let rec gather (sc : Ident.t list) : I.exp -> unit = function
  | Var id -> incr_usecount id
  | App (Lam (vars, body), args) ->
      (* (fun x1 x2 .. xn -> e) a1 a2 .. ak *)
      init_varinfo vars;
      (try List.iter2 update_repres vars args with Invalid_argument _ (* n > k *) -> ());
      gather (vars @ sc) body;
      List.iter (gather sc) args
  | App (fcn, args) ->
      gather sc fcn;
      List.iter (gather sc) args
  | Lam (vars, body) ->
      init_varinfo vars;
      gather (vars @ sc) body
  | Builtin (fcn, args) ->
      if List.mem fcn I.io then List.iter (fun v -> (hashtbl_find v).side_effect <- true) sc;
      List.iter (gather sc) args
  | Let (isrec, vars, bnds, body) ->
      if isrec then init_varinfo ~isrec:true vars else init_varinfo vars;
      List.iter2 update_repres vars bnds;
      List.iter2 (fun v b -> gather (v :: sc) b) vars bnds;
      gather sc body
  | If (test, then_, else_) -> gather sc test; gather sc then_; gather sc else_
  | _ -> ()

let rec reduce : I.exp -> I.exp = function
  | Var id ->
      let info = hashtbl_find id in
      if info.simple then (incr nred; decr_usecount id; info.repres (* inlining a small function *))
      else if is_usecount id 1 then find_repres id
      else Var id
  | App (Lam (vars, body), args) ->
      let vars', args' =
        List.split
          (List.fold_right2
             (fun v a acc ->
               if is_deadvar v then (incr nred; acc (* dead-variable elimination *))
               else (v, reduce a) :: acc )
             vars args [] )
      in
      (try List.iter2 update_repres vars' args' with Invalid_argument _ (* n > k *) -> ());
      if vars' = [] then reduce body else App (Lam (vars', reduce body), args')
  | App (fcn, args) -> App (reduce fcn, List.map reduce args)
  | Lam (vars, body) -> Lam (vars, reduce body)
  | Builtin (fcn, args) -> Builtin (fcn, List.map reduce args)
  | Let (isrec, vars, bnds, body) ->
      let vars', bnds' =
        List.split
          (List.fold_right2
             (fun v b acc ->
               if is_deadvar v then (incr nred; acc (* dead-variable elimination *))
               else if is_usecount v 1 then
                 (v, b) :: acc (* not expand a let binding when it can be inlined. *)
               else (v, reduce b) :: acc )
             vars bnds [] )
      in
      List.iter2 update_repres vars' bnds';
      if vars' = [] then reduce body else Let (isrec, vars', bnds', reduce body)
  | e -> e

let step exp : I.exp =
  nred := 1;
  reduce exp

let rec steps (n : int) (exp : I.exp) =
  if n = 0 then exp
  else
    let prev = !nred in
    let exp' = step exp in
    let next = float_of_int !nred /. float_of_int prev > thresh_ratio in
    if next then steps (n - 1) exp' else exp'

let steps n exp = gather [] exp; steps n exp
