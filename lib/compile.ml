open Machine

exception ScopeError of Sexp.id

let builtins : (Sexp.id * t) list =
  [ ("CAR", CAR); ("CDR", CDR); ("CONS", CONS); ("ADD", ADD); ("SUB", SUB); ("MUL", MUL)
  ; ("DIV", DIV); ("EQ", EQ); ("NE", NE); ("LT", LT); ("LE", LE); ("WRITEC", WRITEC) ]

let rec compile (e : Sexp.exp) (n : Sexp.id list list) (c : t list) : t list =
  match e with
  | Sexp.Nil -> NIL :: c
  | Sexp.Int x -> LDC x :: c
  | Sexp.Var x ->
      let i, j = index x n in
      LD (i, j) :: c
  | Sexp.Seq [] -> NIL :: c
  | Sexp.Seq (fcn :: args) -> NIL :: compile_app args n (compile fcn n (AP :: c))
  | Sexp.Lam (vars, body) ->
      let n' = vars :: n in
      compile_lambda body n' c
  | Sexp.Special (Sexp.Builtin (f, args)) -> compile_builtin args n (List.assoc f builtins :: c)
  | Sexp.Special (Sexp.If (test, then', else')) -> compile_if (test, then', else') n c
  | Sexp.Special (Sexp.Let (vars, vals, body)) ->
      let newn = vars :: n in
      NIL :: compile_app vals n (compile_lambda body newn (AP :: c))
  | Sexp.Special (Sexp.Letrec (vars, vals, body)) ->
      let newn = vars :: n in
      DUM :: NIL :: compile_app vals newn (compile_lambda body newn (RAP :: c))

and compile_builtin (args : Sexp.exp list) (n : Sexp.id list list) (c : t list) =
  if args == [] then c else compile_builtin (List.tl args) n (compile (List.hd args) n c)

and compile_lambda (body : Sexp.exp) (n : Sexp.id list list) (c : t list) : t list =
  LDF (compile body n [RTN]) :: c

and compile_if (test, tr, fa) n c =
  compile test n [SEL (compile tr n [JOIN], compile fa n [JOIN])] @ c

and compile_app (args : Sexp.exp list) n c =
  if args == [] then c else compile_app (List.tl args) n (compile (List.hd args) n (CONS :: c))

and index (x : AbsSyn.id) (n : AbsSyn.id list list) : int * int = indx x n 1

and indx (x : AbsSyn.id) (n : AbsSyn.id list list) (i : int) : int * int =
  if n = [] then raise (ScopeError x)
  else
    let rec indx2 (x : AbsSyn.id) (n : AbsSyn.id list) (j : int) =
      if n = [] then 0 else if List.hd n = x then j else indx2 x (List.tl n) (j + 1)
    in
    let j = indx2 x (List.hd n) 1 in
    if j = 0 then indx x (List.tl n) (i + 1) else (i, j)

let compile (e : Sexp.exp) : Machine.t list =
  try compile e [] [STOP]
  with ScopeError x ->
    ErrorMsg.error ("Unbound value " ^ x);
    []
