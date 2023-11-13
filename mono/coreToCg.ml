module C = Core.Syntax
module Cg = CodeGen.Syntax

let code_list : Cg.codes ref = ref []

let append_code (code : Cg.code) : unit = code_list := code :: !code_list

let rec hoisting : C.exp -> Cg.exp = function
  | Const c -> Const c
  | Var id -> Var id
  | App {fcn; args} -> App {fcn= hoisting fcn; args= List.map hoisting args}
  | Lam {vars; body} ->
      let tmp = Id.from_string "lamtmp" in
      append_code {name= Id.unique_name tmp; params= vars; body= hoisting body};
      Var tmp
  | Prim {oper; args} -> Prim {oper; args= List.map hoisting args}
  | If {cond; then_; else_} ->
      If {cond= hoisting cond; then_= hoisting then_; else_= hoisting else_}
  | Let {vars; bnds; body} ->
      List.iter2
        (fun var bnd ->
          let params, exp = C.unlam bnd in
          append_code {name= Id.unique_name var; params; body= hoisting exp} )
        vars bnds;
      hoisting body
  | Tuple exps -> Tuple (List.map hoisting exps)
  | Split {inp; vars; body} ->
      let bnds =
        List.mapi (fun idx _ -> Cg.Proj {exp= hoisting inp; idx}) vars
      in
      Let {vars; bnds; body= hoisting body}

let c2cg_exp (exp : C.exp) : Cg.codes =
  code_list := [];
  let exp' = hoisting exp in
  List.rev ({Cg.name= "main"; params= []; body= exp'} :: List.rev !code_list)
