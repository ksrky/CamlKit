module C = Combinator.Syntax
module G = Gmachine.Code

let rec find_idx x = function
  | [] -> raise Not_found
  | h :: t -> if x = h then 0 else 1 + find_idx x t

let rec c2g_exp (stack : Id.t list) (fsize : int) : C.exp -> G.t = function
  | Const (Int i) -> [G.PushInt i]
  | Const Nil -> [G.Alloc 1]
  | Var x -> [G.Push (fsize - find_idx x stack)]
  | App {fcn; args} ->
      List.concat_map (c2g_exp stack fsize) args
      @ c2g_exp stack (fsize + 1) fcn
      @ [G.MkAp]
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
      let oper' : G.instr =
        match oper with
        | Add -> Add
        | Sub -> Sub
        | Mul -> Mul
        | Div -> Div
        | _ -> failwith ""
      in
      List.concat_map (c2g_exp stack fsize) args @ [oper'; G.MkAp]
  | _ -> failwith "not implemented"