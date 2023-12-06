open Syntax
open Llvm

let context : llcontext = global_context ()

let builder : llbuilder = builder context
(*
let named_values : (label, llvalue) Hashtbl.t = Hashtbl.create 100

let int_type = i64_type context

let bool_type = i1_type context

let rec codegen_exp (llmod : llmodule) : exp -> llvalue = function
  | CONST i -> const_int int_type i
  | VAR x -> Hashtbl.find named_values x
  | BINOP (op, lhs, rhs) ->
      let lhs_val = codegen_exp llmod lhs in
      let rhs_val = codegen_exp llmod rhs in
      ( match op with
      | PLUS -> build_add
      | MINUS -> build_sub
      | TIMES -> build_mul
      | DIVIDE -> build_sdiv )
        lhs_val rhs_val "binoptmp" builder
  | CALL (fcn, args) ->
      let fcn' = codegen_exp llmod fcn in
      let args' = Array.of_list (List.map (codegen_exp llmod) args) in
      build_call fcn' args' "calltmp" builder
  | MALLOC _ -> failwith ""
  | UPDATE _ -> failwith ""
  | PROJ _ -> failwith ""
  | ESEQ _ -> failwith ""

and codegen_stm (llmod : llmodule) : stm -> unit = function
  | EXP exp -> codegen_exp llmod exp |> ignore
  | JUMP _ -> failwith ""
  | CJUMP _ -> failwith ""
  | ASSIGN _ -> failwith ""
  | EXIT _ -> failwith ""
  | SEQ _ -> failwith ""
  | LABEL _ -> failwith "" *)
