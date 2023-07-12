type id = string

type ids = id list

type exps = exp list

and exp = Int of int | Nil | Var of id | Seq of exps | Lam of ids * exp | Special of special

and special =
  | Builtin of id * exps
  | Let of ids * exps * exp
  | Letrec of ids * exps * exp
  | If of exp * exp * exp

let rec abs2sexp : AbsSyn.exp -> exp = function
  | AbsSyn.VarExp id -> Var id
  | AbsSyn.NilExp -> Nil
  | AbsSyn.IntExp x -> Int x
  | AbsSyn.AppExp _ as exp ->
      let rec loop acc = function
        | AbsSyn.AppExp {fcn; arg} -> loop (abs2sexp arg :: acc) fcn
        | VarExp "print" -> Special (Builtin ("PRINT", acc))
        | fcn -> Seq (abs2sexp fcn :: acc)
      in
      loop [] exp
  | AbsSyn.LamExp {vars; body} -> Lam (vars, abs2sexp body)
  | AbsSyn.OpExp {left; oper= PlusOp; right} ->
      Special (Builtin ("ADD", [abs2sexp left; abs2sexp right]))
  | AbsSyn.OpExp {left; oper= MinusOp; right} ->
      Special (Builtin ("SUB", [abs2sexp left; abs2sexp right]))
  | AbsSyn.OpExp {left; oper= TimesOp; right} ->
      Special (Builtin ("MUL", [abs2sexp left; abs2sexp right]))
  | AbsSyn.OpExp {left; oper= DivideOp; right} ->
      Special (Builtin ("DIV", [abs2sexp left; abs2sexp right]))
  | AbsSyn.OpExp {left; oper= EqOp; right} ->
      Special (Builtin ("EQ", [abs2sexp left; abs2sexp right]))
  | AbsSyn.OpExp _ -> failwith "not implemented"
  | AbsSyn.IfExp {test; then'; else'} -> Special (If (abs2sexp test, abs2sexp then', abs2sexp else'))
  | AbsSyn.LetExp {decs; body} ->
      Special
        (Let
           ( List.map (fun {AbsSyn.name; _} -> name) decs
           , List.map
               (fun {AbsSyn.params; AbsSyn.body; _} ->
                 match params with [] -> abs2sexp body | _ -> Lam (params, abs2sexp body) )
               decs
           , abs2sexp body ) )
  | AbsSyn.LetrecExp {decs; body} ->
      Special
        (Letrec
           ( List.map (fun {AbsSyn.name; _} -> name) decs
           , List.map
               (fun {AbsSyn.params; AbsSyn.body; _} ->
                 match params with [] -> abs2sexp body | _ -> Lam (params, abs2sexp body) )
               decs
           , abs2sexp body ) )
