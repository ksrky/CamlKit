module C = Core.Syntax
module G = Gmachine.Code

let rec find_idx x = function
  | [] -> raise Not_found
  | h :: t -> if x = h then 0 else 1 + find_idx x t

let rec c2g_exp (stack : Id.t list) (fsize : int) : C.exp -> G.t = function
  | Int i -> [G.PushInt i]
  | Var x -> [G.Push (fsize - find_idx x stack)]
  | Nil -> [G.Alloc 1]
  | App {fcn; args} ->
      List.concat_map (c2g_exp stack fsize) args
      @ c2g_exp stack (fsize + 1) fcn
      @ [G.MkAp]
  | Lam _ -> failwith "impossible"
  | Let {isrec= false; vars; bnds; body} ->
      let n = List.length vars in
      let stack' = List.rev vars @ stack in
      List.concat_map (c2g_exp stack fsize) bnds
      @ c2g_exp stack' (fsize + n) body
      @ [G.Slide n]
  | Let {isrec= true; vars; bnds; body} ->
      let n = List.length vars in
      let stack' = List.rev vars @ stack in
      let fsize' = fsize + n in
      G.Alloc n
      :: List.concat
           (List.mapi
              (fun i exp -> c2g_exp stack' fsize' exp @ [Update (n - i)])
              bnds )
      @ c2g_exp stack' fsize' body @ [G.Slide n]
  | Prim {oper; args} ->
      let oper' =
        match oper with
        | "add" -> G.Add
        | "sub" -> G.Sub
        | "mul" -> G.Mul
        | "div" -> G.Div
        | _ -> failwith ""
      in
      List.concat_map (c2g_exp stack fsize) args @ [oper'; G.MkAp]
  | _ -> failwith "not implemented"
