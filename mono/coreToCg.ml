module C = Core.ClosConv
module Cg = CodeGen.Syntax

let code_list : Cg.codes ref = ref []

let append_code (code : Cg.code) : unit = code_list := code :: !code_list

(** function
    | Const c -> Const c
    | Var id -> Var id
    | App {fcn; arg} -> App {fcn= hoisting fcn; arg= hoisting arg}
   (* | Lam {var; body} ->
        let tmp = Id.from_string "lamtmp" in
        append_code {name= Id.unique_name tmp; params= var; body= hoisting body};
        Var tmp *)
    | Prim {oper; args} -> Prim {oper; args= List.map hoisting args}
    | If {cond; then_; else_} ->
        If {cond= hoisting cond; then_= hoisting then_; else_= hoisting else_}
    | Let {vars; bnds; body} ->
        let decs = ref [] in
        List.iter2
          (fun var bnd ->
            match bnd with
            | C.Clos {var= params; body= exp} ->
                append_code {name= Id.unique_name var; params; body= hoisting exp}
            (*| C.Clos (Clos {env; code}) ->
                  decs := Cg.ClosDec {var; env; code= hoisting code} :: !decs
            *)
            | _ -> decs := Cg.ValDec {var; exp= hoisting bnd} :: !decs )
          vars bnds;
        if !decs = [] then hoisting body
        else Let {decs= !decs; body= hoisting body}
   | Clos clos -> Clos clos
   | Select {clos; idx} -> Select {clos= hoisting clos; idx} 

     and c2cg_clos : C.clos -> Cg.dec * Cg.exp = function
       | Clos {env; code} ->
           let clos_var = Id.from_string "clos" in
           (ClosDec {var= clos_var; env; code= hoisting code}, Var clos_var)
       | ClosApp {clos; args} ->
           let dec, exp = c2cg_clos clos in
           (dec, Cg.App {fcn= exp; args= List.map hoisting args}) *)
let rec hoisting : C.exp -> Cg.exp = failwith ""

let c2cg_exp (exp : Core.Syntax.exp) : Cg.codes =
  code_list := [];
  let exp' = C.cc_prog exp in
  let exp'' = hoisting exp' in
  List.rev ({Cg.name= "main"; params= []; body= exp''} :: List.rev !code_list)
