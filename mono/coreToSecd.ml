module C = Core.Syntax
module S = Secd.Operation

let prims : (C.oper * S.t) list =
  [ (Add, ADD); (Sub, SUB); (Mul, MUL); (Div, DIV); (Eq, EQ); (Ne, NE); (Lt, LT)
  ; (Le, LE) ]

let rec c2s_exp (e : C.exp) (n : Id.t list list) (c : S.t list) : S.t list =
  match e with
  | Nil -> NIL :: c
  | Int x -> LDC x :: c
  | Var x ->
      let i, j = index x n in
      LD (i, j) :: c
  | App {fcn; args} -> NIL :: c2s_app args n (c2s_exp fcn n (AP :: c))
  | Lam {vars; body} ->
      let n' = vars :: n in
      c2s_lambda body n' c
  | Prim {oper; args} -> c2s_prim args n (List.assoc oper prims :: c)
  | If {cond; then_; else_} -> c2s_if cond then_ else_ n c
  | Let {isrec= false; vars; bnds; body} ->
      let newn = vars :: n in
      NIL :: c2s_app bnds n (c2s_lambda body newn (AP :: c))
  | Let {isrec= true; vars; bnds; body} ->
      let newn = vars :: n in
      DUM :: NIL :: c2s_app bnds newn (c2s_lambda body newn (RAP :: c))

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
    if n = [] then
      ErrorMsg.impossible
        ("Occurance of '" ^ Id.name x ^ "' must be scope-checked")
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

let f (e : C.exp) : S.t list = c2s_exp e [] [S.PRINTI; S.STOP]
