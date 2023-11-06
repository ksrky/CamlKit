open Syntax

type value = VVar of Id.t | VAbs of Id.t * value

let rec subst_term (x : Id.t) (v : term) : term -> term = function
  | Var y -> if x = y then v else Var y
  | Abs {var; var_ty; body} -> Abs {var; var_ty; body= subst_term x v body}

let rec subst_command (x : Id.t) (v : term) : command -> command = function
  | Let {bnd; cmd} -> Let {bnd; cmd= subst_command x v cmd}
  | Cut {left; right} -> Cut {left= subst_term x v left; right}
  | Jump {lab; args} -> Jump {lab; args= List.map (subst_term x v) args}

let eval : command -> command = function
  | Let {bnd= Nonrec (Bind {var; body; _}); cmd} -> subst_command var body cmd
  | Cut {left= Abs {var; body; _}; right= Kont {left; right}} ->
      Cut {left= subst_term var left body; right}
  | _ -> failwith "Not implemented"
