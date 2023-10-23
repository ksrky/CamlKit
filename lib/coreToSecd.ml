module C = Core.Syntax
module S = Secd.Machine

let primitives : (string * S.t) list =
  [ ("car", CAR); ("cdr", CDR); ("cons", CONS); ("add", ADD); ("sub", SUB); ("mul", MUL)
  ; ("div", DIV); ("eq", EQ); ("ne", NE); ("lt", LT); ("le", LE); ("readi", READI)
  ; ("printi", PRINTI) ]

let rec compile (e : C.exp) (n : Id.t list list) (c : S.t list) : S.t list =
  match e with
  | Nil -> NIL :: c
  | Int x -> LDC x :: c
  | Var x ->
      let i, j = index x n in
      LD (i, j) :: c
  | App {fcn; args} -> NIL :: compile_app args n (compile fcn n (AP :: c))
  | Lam {vars; body} ->
      let n' = vars :: n in
      compile_lambda body n' c
  | Prim {oper; args} -> compile_prim args n (List.assoc oper primitives :: c)
  | If {cond; then_; else_} -> compile_if cond then_ else_ n c
  | Let {isrec= false; vars; bnds; body} ->
      let newn = vars :: n in
      NIL :: compile_app bnds n (compile_lambda body newn (AP :: c))
  | Let {isrec= true; vars; bnds; body} ->
      let newn = vars :: n in
      DUM :: NIL :: compile_app bnds newn (compile_lambda body newn (RAP :: c))

and compile_prim (args : C.exp list) (n : C.id list list) (c : S.t list) =
  if args = [] then c else compile_prim (List.tl args) n (compile (List.hd args) n c)

and compile_lambda (body : C.exp) (n : C.id list list) (c : S.t list) : S.t list =
  LDF (compile body n [RTN]) :: c

and compile_if test tr fa n c = compile test n [SEL (compile tr n [JOIN], compile fa n [JOIN])] @ c

and compile_app (args : C.exp list) n c =
  if args = [] then c else compile_app (List.tl args) n (compile (List.hd args) n (CONS :: c))

and index (x : Id.t) (n : Id.t list list) : int * int = indx x n 1

and indx (x : Id.t) (n : Id.t list list) (i : int) : int * int =
  if n = [] then ErrorMsg.impossible ("Occurance of '" ^ Id.name x ^ "' must be scope-checked")
  else
    let rec indx2 (x : Id.t) (n : Id.t list) (j : int) =
      if n = [] then 0 else if List.hd n = x then j else indx2 x (List.tl n) (j + 1)
    in
    let j = indx2 x (List.hd n) 1 in
    if j = 0 then indx x (List.tl n) (i + 1) else (i, j)

let f (e : C.exp) : S.t list = compile e [] [S.STOP]
