module V = Virtual

let lab_num = ref (-1)

let new_label () =
  incr lab_num;
  "L" ^ string_of_int !lab_num

let result gen =
  let t = Ident.fresh () in
  gen t; t

let rv : int = 0

let index (x : Ident.t) (env : Ident.t list) : int =
  let rec loop i = function
    | [] -> raise Not_found
    | hd :: tl -> if x = hd then i else loop (i + 1) tl
  in
  loop 0 env

let rec compile_def ({name; params; body} : IntSyn.def) : Virtual.frag =
  let args = params in
  let rec compile_exp (e : IntSyn.exp) : V.instr list =
    match e with
    | Int i -> [V.Push (V.Const, i)]
    | Nil -> [V.Push (V.Const, 0)]
    | Var id -> [V.Push (V.Arg, index id args)]
    | App (Var f, args) -> compile_args args @ [V.Call (Ident.to_string f, List.length args)]
    | App _ | Let _ | Letrec _ -> ErrorMsg.impossible "should be removed in Lifting module"
    | Builtin (f, args) when List.mem f IntSyn.arith ->
        let op = match f with "ADD" -> V.Add | "SUB" -> V.Sub | "MUL" -> V.Mul | "DIV" -> V.Div in
        compile_args args @ [V.Arith op]
    | Builtin (f, args) when List.mem f IntSyn.rel ->
        let op = match f with "EQ" -> V.Eq | "NE" -> V.Neq | "LT" -> V.Lt | "LE" -> V.Le in
        let t = new_label () in
        let j = new_label () in
        compile_args args
        @ [V.IfGoto (op, t); V.Push (V.Const, 0); V.Goto j; V.Label t; V.Push (V.Const, 1); V.Goto j]
    | If (test, then', else') ->
        let t = new_label () in
        let j = new_label () in
        compile_exp test
        @ [V.Push (V.Const, 0); V.IfGoto (V.Neq, t)]
        @ compile_exp else' @ [V.Goto j; V.Label t] @ compile_exp then' @ [V.Label j]
    | _ -> []
  and compile_args (es : IntSyn.exp list) : V.instr list = List.concat_map compile_exp es in
  let instrs = compile_exp body in
  Proc (Ident.to_string name, List.length params, instrs @ [V.Push (V.Temp, rv)])
