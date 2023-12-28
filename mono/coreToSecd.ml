module C = Core.Syntax
module S = Secd.Operation

let prims : (C.oper * S.t) list =
  [ (Add, ADD); (Sub, SUB); (Mul, MUL); (Div, DIV); (Eq, EQ); (Ne, NE); (Lt, LT)
  ; (Le, LE) ]

let rec c2s_exp (e : C.exp) (n : Id.t list list) (c : S.t list) : S.t list =
  match e with
  | Const (Bool b) -> LDC (Bool.to_int b) :: c
  | Const (Int i) -> LDC i :: c
  | Var (x, _) ->
      let i, j = index x n in
      LD (i, j) :: c
  | App {fcn= fcn, _; arg= arg, _} ->
      NIL :: c2s_app [arg] n (c2s_exp fcn n (AP :: c))
  | Lam {var; body= body, _} ->
      let n' = [fst var] :: n in
      c2s_lambda body n' c
  | Fix {var; body= body, _} -> failwith ""
  | Prim {left= left, _; oper; right= right, _} ->
      c2s_prim [left; right] n (List.assoc oper prims :: c)
  | If {cond= cond, _; then_= then_, _; else_= else_, _} ->
      c2s_if cond then_ else_ n c
  | Let {var; bnd= Fix {body= bnd, _; _}; body= body, _} ->
      let newn = [fst var] :: n in
      DUM :: NIL :: c2s_app [bnd] newn (c2s_lambda body newn (RAP :: c))
  | Let {var; bnd; body= body, _} ->
      let newn = [fst var] :: n in
      NIL :: c2s_app [bnd] n (c2s_lambda body newn (AP :: c))
  | Tuple _ -> failwith "Tuple not implemented"
  | Proj _ -> failwith "Proj not implemented"

and c2s_prim (args : C.exp list) (n : C.id list list) (c : S.t list) =
  if args = [] then c
  else c2s_prim (List.tl args) n (c2s_exp (List.hd args) n c)

and c2s_lambda (body : C.exp) (n : C.id list list) (c : S.t list) : S.t list =
  LDF (c2s_exp body n [RTN]) :: c

and c2s_if test tr fa n c =
  c2s_exp test n [SEL (c2s_exp tr n [JOIN], c2s_exp fa n [JOIN])] @ c

and c2s_app (args : C.exp list) n c =
  if args = [] then c
  else c2s_app (List.tl args) n (c2s_exp (List.hd args) n (CONS :: c))

and index (x : Id.t) (n : Id.t list list) : int * int =
  let rec indx (x : Id.t) (n : Id.t list list) (i : int) : int * int =
    if n = [] then (
      Format.fprintf Format.str_formatter
        "Occurance of '%a' must be scope-checked" Id.pp_print_id x;
      raise Utils.Bug_error )
    else
      let rec indx2 (x : Id.t) (n : Id.t list) (j : int) =
        if n = [] then 0
        else if List.hd n = x then j
        else indx2 x (List.tl n) (j + 1)
      in
      let j = indx2 x (List.hd n) 1 in
      if j = 0 then indx x (List.tl n) (i + 1) else (i, j)
  in
  indx x n 1

let c2s_prog ((e, _) : C.prog) : S.t list = c2s_exp e [] [S.STOP]
