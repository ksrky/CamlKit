module C = Core.Syntax
module Cg = CodeGen.Syntax

let code_list : Cg.codes ref = ref []

let append_code (code : Cg.code) : unit = code_list := code :: !code_list

let rec hoisting : C.exp -> Cg.exp = function
  | Const c -> Const c
  | Var id -> Var id
  | App {fcn= Var id; args} ->
      App {fcn= Id.unique_name id; args= List.map hoisting args}
  | App _ -> failwith "TODO"
  | Lam {vars; body} ->
      let tmp = Id.from_string "lamtmp" in
      append_code {name= Id.unique_name tmp; params= vars; body= hoisting body};
      Var tmp
  | Prim {oper; args} -> Prim {oper; args= List.map hoisting args}
  | If {cond; then_; else_} ->
      If {cond= hoisting cond; then_= hoisting then_; else_= hoisting else_}
  | Let {isrec= false; vars; bnds; body} ->
      Let {vars; bnds= List.map hoisting bnds; body= hoisting body}
  | Let {isrec= true; vars; bnds; body} ->
      List.iter2
        (fun var bnd ->
          append_code {name= Id.unique_name var; params= []; body= hoisting bnd}
          )
        vars bnds;
      hoisting body
  | Tuple exps -> Tuple (List.map hoisting exps)
  | Split {inp; vars; body} ->
      let bnds =
        List.mapi (fun idx _ -> Cg.Proj {exp= hoisting inp; idx}) vars
      in
      Let {vars; bnds; body= hoisting body}

let c2l_exp (exp : C.exp) : Cg.codes =
  code_list := [];
  let exp' = hoisting exp in
  List.rev ({Cg.name= "main"; params= []; body= exp'} :: List.rev !code_list)
