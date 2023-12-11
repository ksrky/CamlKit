module C = Core.Syntax
module Z = Zinc.Code

let rec find_idx x = function
  | [] -> raise Not_found
  | h :: t -> if x = h then 0 else 1 + find_idx x t

let rec c2z_exp (env : Id.t list) : C.exp -> Z.t = function
  | Const (Bool b) -> [Z.PushInt (Bool.to_int b)] (* tmp *)
  | Const (Int i) -> [Z.PushInt i]
  | Var x -> [Z.Access (find_idx x env)]
  | App {fcn; arg} ->
      (Z.Pushmark :: c2z_exp env arg) @ c2z_exp env fcn @ [Z.Apply]
  | Lam {var; body} -> [Z.Cur (c2z_exp (var :: env) body @ [Z.Return])]
  | Let {isrec= false; vars; bnds; body} ->
      List.concat_map (fun e -> c2z_exp env e) (List.rev bnds)
      @ (Z.Let :: c2z_exp (vars @ env) body)
      @ [Z.Endlet]
  | Let {isrec= true; vars; bnds; body} -> failwith "Not implemented"
  | _ -> failwith "Not implemented"
