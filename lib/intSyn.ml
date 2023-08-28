type id = Ident.t

type ty = AbsSyn.ty

type binder = id * ty

and binders = binder list

type exps = exp list

and exp =
  | Int of int
  | Nil
  | Var of binder
  | App of exp * exps
  | Lam of binders * exp
  | Prim of string * exps
  | Let of bool * binders * exps * exp
  | If of exp * exp * exp
  | Seq of exp * exp

type frag = {name: string; params: binders; body: exp}

type frags = frag list

let arith = ["add"; "sub"; "mul"; "div"]

let rel = ["eq"; "ne"; "lt"; "le"; "gt"; "ge"]

let effect = ["printi"; "readi"; "store"; "array_alloca"]

(*
   IO:     printi, readi
   Memory: load, store, gep, array_alloca
*)

let rec ppr_ty : ty -> string = AbsSyn.ppr_ty

let ppr_binder (pprid : id -> string) ((id, ty) : binder) = pprid id ^ ": " ^ ppr_ty ty

let ppr_exp (pprid : id -> string) (exp : exp) =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let pbndr ((id, ty) : binder) = pprid id ^ ": " ^ ppr_ty ty in
  let rec pexp ctx exp =
    match exp with
    | Var var -> pbndr var
    | Nil -> "nil"
    | Int i -> string_of_int i
    | App (fcn, args) ->
        parens ctx 2 (pexp 2 fcn ^ "(" ^ String.concat ", " (List.map (pexp 0) args) ^ ")")
    | Lam (bndrs, body) ->
        parens ctx 0
          ( "fun "
          ^ String.concat " " (List.map (fun b -> "(" ^ pbndr b ^ ")") bndrs)
          ^ "-> " ^ pexp 0 body )
    | Prim (fcn, args) -> fcn ^ "(" ^ String.concat ", " (List.map (pexp 0) args) ^ ")"
    | If (cond, then_, else_) ->
        parens ctx 0 ("if " ^ pexp 0 cond ^ " then " ^ pexp 0 then_ ^ " else " ^ pexp 0 else_)
    | Let (isrec, bndrs, exps, body) ->
        parens ctx 0
          ( "let "
          ^ (if isrec then "rec " else "")
          ^ String.concat "; " (List.map2 (fun b e -> pbndr b ^ " = " ^ pexp 0 e) bndrs exps)
          ^ " in " ^ pexp 0 body )
    | Seq (exp, rest) -> "(" ^ pexp 0 exp ^ "; " ^ pexp 0 rest ^ ")"
  in
  pexp 0 exp

let ppr_frag {name; params; body} =
  name ^ "("
  ^ String.concat ", " (List.map (ppr_binder Ident.unique_name) params)
  ^ ") = " ^ ppr_exp Ident.unique_name body

let ppr_frags frags = String.concat "\n" (List.map ppr_frag frags)
