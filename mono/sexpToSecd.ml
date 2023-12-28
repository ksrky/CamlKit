module Sx = Sexp.Syntax
module S = Secd.Operation

let prims : (Sx.prim * S.t) list =
  [ (Add, ADD); (Sub, SUB); (Mul, MUL); (Div, DIV); (Eq, EQ); (Ne, NE); (Lt, LT)
  ; (Le, LE) ]

let rec sx2s_exp (e : Sx.exp) (n : Id.t list list) (c : S.t list) : S.t list =
  match e with
  | Const (Bool b) -> LDC (Bool.to_int b) :: c
  | Const (Int i) -> LDC i :: c
  | Var x ->
      let i, j = index x n in
      LD (i, j) :: c
  | Prim {prim; args} -> sx2s_prim args n (List.assoc prim prims :: c)
  | App {fcn; args} -> NIL :: sx2s_app args n (sx2s_exp fcn n (AP :: c))
  | Lam {vars; body} ->
      let n' = vars :: n in
      sx2s_lambda body n' c
  | If {cond; then_; else_} -> sx2s_if cond then_ else_ n c
  | Let {isrec= false; vars; bnds; body} ->
      let newn = vars :: n in
      NIL :: sx2s_app bnds n (sx2s_lambda body newn (AP :: c))
  | Let {isrec= true; vars; bnds; body} ->
      let newn = vars :: n in
      DUM :: NIL :: sx2s_app bnds newn (sx2s_lambda body newn (RAP :: c))

and sx2s_prim (args : Sx.exp list) (n : Sx.id list list) (c : S.t list) =
  if args = [] then c
  else sx2s_prim (List.tl args) n (sx2s_exp (List.hd args) n c)

and sx2s_lambda (body : Sx.exp) (n : Sx.id list list) (c : S.t list) : S.t list
    =
  LDF (sx2s_exp body n [RTN]) :: c

and sx2s_if test tr fa n c =
  sx2s_exp test n [SEL (sx2s_exp tr n [JOIN], sx2s_exp fa n [JOIN])] @ c

and sx2s_app (args : Sx.exp list) n c =
  if args = [] then c
  else sx2s_app (List.tl args) n (sx2s_exp (List.hd args) n (CONS :: c))

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

let sx2s_prog (e : Sx.prog) : S.t list = sx2s_exp e [] [S.STOP]
