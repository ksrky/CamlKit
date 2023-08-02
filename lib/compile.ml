open Machine

exception ScopeError of Internal.id

let builtins : (string * t) list =
  [ ("CAR", CAR); ("CDR", CDR); ("CONS", CONS); ("ADD", ADD); ("SUB", SUB); ("MUL", MUL)
  ; ("DIV", DIV); ("EQ", EQ); ("NE", NE); ("LT", LT); ("LE", LE); ("READC", READC)
  ; ("WRITEC", WRITEC) ]

let rec compile (e : Internal.exp) (n : Ident.t list list) (c : t list) : t list =
  match e with
  | Internal.Nil -> NIL :: c
  | Internal.Int x -> LDC x :: c
  | Internal.Var x ->
      let i, j = index x n in
      LD (i, j) :: c
  | Internal.App (fcn, args) -> NIL :: compile_app args n (compile fcn n (AP :: c))
  | Internal.Lam (vars, body) ->
      let n' = vars :: n in
      compile_lambda body n' c
  | Internal.Builtin (f, args) -> compile_builtin args n (List.assoc f builtins :: c)
  | Internal.If (test, then', else') -> compile_if (test, then', else') n c
  | Internal.Let (vars, vals, body) ->
      let newn = vars :: n in
      NIL :: compile_app vals n (compile_lambda body newn (AP :: c))
  | Internal.Letrec (vars, vals, body) ->
      let newn = vars :: n in
      DUM :: NIL :: compile_app vals newn (compile_lambda body newn (RAP :: c))

and compile_builtin (args : Internal.exp list) (n : Internal.id list list) (c : t list) =
  if args == [] then c else compile_builtin (List.tl args) n (compile (List.hd args) n c)

and compile_lambda (body : Internal.exp) (n : Internal.id list list) (c : t list) : t list =
  LDF (compile body n [RTN]) :: c

and compile_if (test, tr, fa) n c =
  compile test n [SEL (compile tr n [JOIN], compile fa n [JOIN])] @ c

and compile_app (args : Internal.exp list) n c =
  if args == [] then c else compile_app (List.tl args) n (compile (List.hd args) n (CONS :: c))

and index (x : Ident.t) (n : Ident.t list list) : int * int = indx x n 1

and indx (x : Ident.t) (n : Ident.t list list) (i : int) : int * int =
  if n = [] then raise (ScopeError x)
  else
    let rec indx2 (x : Ident.t) (n : Ident.t list) (j : int) =
      if n = [] then 0 else if List.hd n = x then j else indx2 x (List.tl n) (j + 1)
    in
    let j = indx2 x (List.hd n) 1 in
    if j = 0 then indx x (List.tl n) (i + 1) else (i, j)

let compile (e : Internal.exp) : Machine.t list =
  try compile e [] [STOP]
  with ScopeError x ->
    ErrorMsg.error ("Unbound value " ^ Ident.to_string x);
    []
