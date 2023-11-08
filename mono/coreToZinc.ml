module C = Core.Syntax
module Z = Zinc.Code

let rec find_idx x = function
  | [] -> raise Not_found
  | h :: t -> if x = h then 0 else 1 + find_idx x t

let rec c2z_exp (env : Id.t list) : C.exp -> Z.t = function
  | Const Nil -> [Z.PushInt 0]
  | Const (Int i) -> [Z.PushInt i]
  | Var x -> [Z.Access (find_idx x env)]
  | App {fcn; args} ->
      (Z.Pushmark :: List.concat_map (fun e -> c2z_exp env e) (List.rev args))
      @ c2z_exp env fcn @ [Z.Apply]
  | Lam {vars; body} -> [Z.Cur (c2z_exp (vars @ env) body @ [Z.Return])]
  | Let {isrec= false; vars; bnds; body} ->
      List.concat_map (fun e -> c2z_exp env e) (List.rev bnds)
      @ (Z.Let :: c2z_exp (vars @ env) body)
      @ [Z.Endlet]
  | Let {isrec= true; vars; bnds; body} -> failwith "Not implemented"
  | _ -> failwith "Not implemented"
