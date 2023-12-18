type id = Id.t

type lit = IntLit of int | BoolLit of bool

type exp =
  | VarExp of id
  | NilExp
  | BoolExp of bool
  | IntExp of int
  | AppExp of {fcn: exp; arg: exp}
  | LamExp of {vars: id list; body: exp}
  | OpExp of {left: exp; op: op; right: exp}
  | IfExp of {cond: exp; then_: exp; else_: exp}
  | LetExp of {bnds: bnd list; body: exp}
  | LetrecExp of {bnds: bnd list; body: exp}

and bnd = Bind of {name: id; params: id list; body: exp}

and op =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp

type ty = NilTy | BoolTy | IntTy | FunTy of ty * ty | MetaTy of tyvar

and tyvar = {uniq: int; mutable repres: ty option}

type param = id * ty

type aexp =
  | VarAExp of id
  | NilAExp
  | BoolAExp of bool
  | IntAExp of int
  | AppAExp of {fcn: expty; arg: expty}
  | LamAExp of {params: param list; body: expty}
  | OpAExp of {left: expty; op: op; right: expty}
  | IfAExp of {cond: expty; then_: expty; else_: expty}
  | LetAExp of {bnds: abnd list; body: expty}
  | LetrecAExp of {bnds: abnd list; body: expty}

and expty = aexp * ty

and abnd = ABind of {name: id; params: param list; body: expty}

type aprog = expty

let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s

let ppr_ty (ty : ty) : string =
  let parens ctx prec s = if ctx > prec then "(" ^ s ^ ")" else s in
  let rec pretty ctx = function
    | NilTy -> "nil"
    | IntTy -> "int"
    | BoolTy -> "bool"
    | FunTy (fcn, arg) -> parens ctx 0 (pretty 1 fcn ^ " -> " ^ pretty 0 arg)
    | MetaTy tv -> "$" ^ string_of_int tv.uniq
  in
  pretty 0 ty

(*

   let ppr_oper : op -> string = function
     | PlusOp -> "+"
     | MinusOp -> "-"
     | TimesOp -> "*"
     | DivideOp -> "/"
     | EqOp -> "="
     | NeqOp -> "<>"
     | LtOp -> "<"
     | LeOp -> "<="
     | GtOp -> ">"
     | GeOp -> ">="

   let rec ppr_exp exp =
     let rec pretty ctx = function
       | VarExp id -> Id.name id
       | NilExp -> "nil"
       | BoolExp b -> string_of_bool b
       | IntExp i -> string_of_int i
       | AppExp {fcn; arg} -> parens ctx 1 (pretty 1 fcn ^ " " ^ pretty 2 arg)
       | OpExp {left; op; right} ->
           parens ctx 1 (pretty 1 left ^ " " ^ ppr_oper op ^ " " ^ pretty 1 right)
       | LamExp {vars; body} ->
           parens ctx 0
             ( "fun "
             ^ String.concat " " (List.map Id.name vars)
             ^ " -> " ^ pretty 0 body )
       | IfExp {cond; then_; else_} ->
           parens ctx 0
             ( "if " ^ pretty 0 cond ^ " then " ^ pretty 0 then_ ^ " else "
             ^ pretty 0 else_ )
       | LetExp {bnds; body} ->
           parens ctx 0
             ( "let "
             ^ String.concat " and " (List.map ppr_bnd bnds)
             ^ " in " ^ pretty 0 body )
       | LetrecExp {bnds; body} ->
           parens ctx 0
             ( "let rec "
             ^ String.concat " and " (List.map ppr_bnd bnds)
             ^ " in " ^ pretty 0 body )
     in
     pretty 0 exp

   and ppr_bnd (Bind {name; params; body} : bnd) : string =
     String.concat " " (List.map Id.name (name :: params)) ^ " = " ^ ppr_exp body



   let rec ppr_aexp ctx : aexp -> string = function
     | VarAExp id -> Id.name id
     | NilAExp -> "nil"
     | BoolAExp b -> string_of_bool b
     | IntAExp i -> string_of_int i
     | AppAExp {fcn; arg} ->
         parens ctx 1 (ppr_aexp 1 (fst fcn) ^ " " ^ ppr_aexp 2 (fst arg))
     | OpAExp {left; op; right} ->
         parens ctx 1
           ( ppr_aexp 1 (fst left)
           ^ " " ^ ppr_oper op ^ " "
           ^ ppr_aexp 1 (fst right) )
     | LamAExp {vars; body} ->
         parens ctx 0
           ( "fun "
           ^ String.concat " " (List.map Id.name vars)
           ^ " -> "
           ^ ppr_aexp 0 (fst body) )
     | IfAExp {cond; then_; else_} ->
         "if "
         ^ ppr_aexp 0 (fst cond)
         ^ " then "
         ^ ppr_aexp 0 (fst then_)
         ^ " else "
         ^ ppr_aexp 0 (fst else_)
     | LetAExp {bnds; body} ->
         parens ctx 0
           ( "let "
           ^ String.concat " and " (List.map ppr_abnd bnds)
           ^ " in "
           ^ ppr_aexp ctx (fst body) )
     | LetrecAExp {bnds; body} ->
         parens ctx 0
           ( "let "
           ^ String.concat " and " (List.map ppr_abnd bnds)
           ^ " in "
           ^ ppr_aexp ctx (fst body) )

   and ppr_abnd (ABind {name; params; body} : abnd) : string =
     Id.name name
     ^ ( if params = [] then ""
         else
           String.concat " "
             (List.map
                (fun (id, ty) -> "(" ^ Id.name id ^ " : " ^ ppr_ty ty ^ ")")
                params ) )
     ^ " : "
     ^ ppr_ty (snd body)
     ^ " = "
     ^ ppr_aexp 0 (fst body) *)