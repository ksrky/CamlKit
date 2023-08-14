module I = IntSyn
module M = Machine

let builtins : (string * M.t) list =
  [ ("car", CAR); ("cdr", CDR); ("cons", CONS); ("add", ADD); ("sub", SUB); ("mul", MUL)
  ; ("div", DIV); ("eq", EQ); ("ne", NE); ("lt", LT); ("le", LE); ("readi", READI)
  ; ("printi", PRINTI) ]

let rec compile (e : I.exp) (n : Ident.t list list) (c : M.t list) : M.t list =
  match e with
  | Nil -> NIL :: c
  | Int x -> LDC x :: c
  | Var x ->
      let i, j = index x n in
      LD (i, j) :: c
  | App (fcn, args) -> NIL :: compile_app args n (compile fcn n (AP :: c))
  | Lam (vars, body) ->
      let n' = vars :: n in
      compile_lambda body n' c
  | Builtin (f, args) -> compile_builtin args n (List.assoc f builtins :: c)
  | If (test, then', else') -> compile_if (test, then', else') n c
  | Let (vars, vals, body) ->
      let newn = vars :: n in
      NIL :: compile_app vals n (compile_lambda body newn (AP :: c))
  | Letrec (vars, vals, body) ->
      let newn = vars :: n in
      DUM :: NIL :: compile_app vals newn (compile_lambda body newn (RAP :: c))

and compile_builtin (args : I.exp list) (n : I.id list list) (c : M.t list) =
  if args == [] then c else compile_builtin (List.tl args) n (compile (List.hd args) n c)

and compile_lambda (body : I.exp) (n : I.id list list) (c : M.t list) : M.t list =
  LDF (compile body n [RTN]) :: c

and compile_if (test, tr, fa) n c =
  compile test n [SEL (compile tr n [JOIN], compile fa n [JOIN])] @ c

and compile_app (args : I.exp list) n c =
  if args == [] then c else compile_app (List.tl args) n (compile (List.hd args) n (CONS :: c))

and index (x : Ident.t) (n : Ident.t list list) : int * int = indx x n 1

and indx (x : Ident.t) (n : Ident.t list list) (i : int) : int * int =
  if n = [] then
    ErrorMsg.impossible ("Occurance of '" ^ Ident.to_string x ^ "' must be scope-checked")
  else
    let rec indx2 (x : Ident.t) (n : Ident.t list) (j : int) =
      if n = [] then 0 else if List.hd n = x then j else indx2 x (List.tl n) (j + 1)
    in
    let j = indx2 x (List.hd n) 1 in
    if j = 0 then indx x (List.tl n) (i + 1) else (i, j)

let compile (e : I.exp) : Machine.t list = compile e [] [M.STOP]
