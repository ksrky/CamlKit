type id = Id.t

type value = Const of int | Var of id | Glb of id

and exp =
  | Let of {dec: dec; body: exp}
  | App of {fcn: value; args: value list}
  | If of {oper: relop; left: value; right: value; then_: exp; else_: exp}
  | Halt of value

and dec =
  | ValDec of {name: id; val_: value}
  | PrimDec of {name: id; left: value; oper: arithop; right: value}
  | ProjDec of {name: id; val_: value; idx: int}
  | MallocDec of {name: id; len: int}
  | UpdateDec of {name: id; var: id; idx: int; val_: value}

and arithop = Add | Sub | Mul | Div

and relop = Eq | Ne | Lt | Le | Gt | Ge

type heap =
  | Code of {name: id; vars: id list; body: exp}
  | Tuple of {name: id; vals: value list}

type prog = heap list * exp

let let_decs decs body =
  List.fold_right (fun dec body -> Let {dec; body}) decs body

let parens (outer : int) (prec : int) s =
  if outer > prec then "(" ^ s ^ ")" else s

let rec ppr_val prec : value -> string = function
  | Const i -> Printf.sprintf "%i" i
  | Var x -> Id.unique_name x
  | Glb x -> Id.unique_name x

and ppr_exp prec : exp -> string = function
  | Let {dec; body} ->
      let dec = ppr_dec dec in
      let body = ppr_exp 0 body in
      parens prec 0 (Printf.sprintf "let %s in\n%s" dec body)
  | App {fcn; args} ->
      let fcn = ppr_val 1 fcn in
      let args = List.map (ppr_val 0) args in
      parens prec 1 (Printf.sprintf "%s(%s)" fcn (String.concat ", " args))
  | If {left; oper; right; then_; else_} ->
      let left = ppr_val 0 left in
      let oper =
        match oper with
        | Eq -> "="
        | Ne -> "<>"
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="
      in
      let right = ppr_val 0 right in
      let then_ = ppr_exp 0 then_ in
      let else_ = ppr_exp 0 else_ in
      parens prec 0
        (Printf.sprintf "if %s %s %s\n  then %s\n  else %s" left oper right
           then_ else_ )
  | Halt v -> parens prec 0 (Printf.sprintf "halt %s" (ppr_val 2 v))

and ppr_dec : dec -> string = function
  | ValDec {name; val_} ->
      let name = Id.unique_name name in
      let val_ = ppr_val 0 val_ in
      Printf.sprintf "%s = %s" name val_
  | PrimDec {name; left; oper; right} ->
      let name = Id.unique_name name in
      let left = ppr_val 0 left in
      let oper =
        match oper with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      in
      let right = ppr_val 0 right in
      Printf.sprintf "%s = %s %s %s" name left oper right
  | ProjDec {name; val_; idx} ->
      let name = Id.unique_name name in
      let val_ = ppr_val 0 val_ in
      Printf.sprintf "%s = %s.%i" name val_ idx
  | MallocDec {name; len} ->
      let name = Id.unique_name name in
      Printf.sprintf "%s = malloc %i" name len
  | UpdateDec {name; var; idx; val_} ->
      let name = Id.unique_name name in
      let var = Id.unique_name var in
      let val_ = ppr_val 0 val_ in
      Printf.sprintf "%s = %s.%i <- %s" name var idx val_

let ppr_heap : heap -> string = function
  | Code {name; vars; body} ->
      let name = Id.unique_name name in
      let vars = List.map Id.unique_name vars in
      let body = ppr_exp 0 body in
      Printf.sprintf "%s = code(%s).\n  %s" name (String.concat ", " vars) body
  | Tuple {name; vals} ->
      let name = Id.unique_name name in
      let vals = List.map (ppr_val 0) vals in
      Printf.sprintf "%s = (%s)" name (String.concat ", " vals)

let ppr_prog (heaps, exp) =
  let heaps = List.map ppr_heap heaps in
  let exp = ppr_exp 0 exp in
  Printf.sprintf "letrec\n  %s\nin\n  %s" (String.concat "\n  " heaps) exp
