open Syntax
open Llvm

let context : llcontext = global_context ()

let builder : llbuilder = builder context

let named_values : (id, llvalue) Hashtbl.t = Hashtbl.create 100

let int_type = i64_type context

let tuple_type (tys : lltype list) = struct_type context (Array.of_list tys)

let rec codegen_exp (llmod : llmodule) : exp -> llvalue = function
  | Const Nil -> const_null int_type
  | Const (Int i) -> const_int int_type i
  | Var id -> Hashtbl.find named_values id
  | App {fcn; args} ->
      let callee =
        match lookup_function fcn llmod with
        | Some callee -> callee
        | None -> failwith "unknown function referenced"
      in
      let args' = Array.of_list (List.map (codegen_exp llmod) args) in
      build_call callee args' "calltmp" builder
  | Prim {oper; args= [lhs; rhs]} ->
      let lhs_val = codegen_exp llmod lhs in
      let rhs_val = codegen_exp llmod rhs in
      ( match oper with
      | Add -> build_add
      | Sub -> build_sub
      | Mul -> build_mul
      | Div -> build_sdiv
      | Eq -> build_icmp Icmp.Eq
      | Ne -> build_icmp Icmp.Ne
      | Lt -> build_icmp Icmp.Slt
      | Le -> build_icmp Icmp.Sle
      | Gt -> build_icmp Icmp.Sgt
      | Ge -> build_icmp Icmp.Sge )
        lhs_val rhs_val "primtmp" builder
  | Prim _ -> failwith "no such primitives"
  | Let {vars; bnds; body} ->
      List.iter2
        (fun id bnd ->
          let bnd_val = codegen_exp llmod bnd in
          Hashtbl.add named_values id bnd_val )
        vars bnds;
      codegen_exp llmod body
  | If {cond; then_; else_} ->
      let cond' = codegen_exp llmod cond in
      let zero = const_int int_type 0 in
      let cond_val = build_icmp Icmp.Ne cond' zero "ifcond" builder in
      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in
      let then_bb = append_block context "then" the_function in
      position_at_end then_bb builder;
      let then_val = codegen_exp llmod then_ in
      let new_then_bb = insertion_block builder in
      let else_bb = append_block context "else" the_function in
      position_at_end else_bb builder;
      let else_val = codegen_exp llmod else_ in
      let new_else_bb = insertion_block builder in
      let merge_bb = append_block context "ifcont" the_function in
      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = build_phi incoming "iftmp" builder in
      position_at_end start_bb builder;
      build_cond_br cond_val then_bb else_bb builder |> ignore;
      position_at_end new_then_bb builder;
      build_br merge_bb builder |> ignore;
      position_at_end new_else_bb builder;
      build_br merge_bb builder |> ignore;
      position_at_end merge_bb builder;
      phi
  | Tuple exps ->
      let tys = List.map (fun _ -> int_type) exps in
      let tuple_val = build_alloca (tuple_type tys) "tupletmp" builder in
      List.iteri
        (fun i exp ->
          let exp_val = codegen_exp llmod exp in
          let tuple_ptr = build_struct_gep tuple_val i "tupleptr" builder in
          build_store exp_val tuple_ptr builder |> ignore )
        exps;
      tuple_val
  | Proj {exp; idx} ->
      let exp_val = codegen_exp llmod exp in
      let tuple_ptr = build_struct_gep exp_val idx "tupleptr" builder in
      build_load tuple_ptr "projtmp" builder

let set_params (func : llvalue) (params : id list) : unit =
  Array.iteri
    (fun i v ->
      let id = List.nth params i in
      set_value_name (Id.unique_name id) v;
      Hashtbl.add named_values id v )
    (Llvm.params func)

let codegen_proto (llmod : llmodule) (name : string) (params : id list) : unit =
  let param_tys = Array.of_list (List.map (fun _ -> int_type) params) in
  let func_ty = function_type int_type param_tys in
  let func =
    match lookup_function name llmod with
    | None -> declare_function name func_ty llmod
    | Some _ -> failwith "redefinition of function"
  in
  set_params func params

let codegen_func (llmod : llmodule) ({name; params; body} : code) : unit =
  Hashtbl.clear named_values;
  let func = Option.get (lookup_function name llmod) in
  set_params func params;
  let bb = append_block context name func in
  position_at_end bb builder;
  try
    let ret_val = codegen_exp llmod body in
    build_ret ret_val builder |> ignore;
    Llvm_analysis.assert_valid_function func
  with e -> delete_function func; raise e

let codegen (modid : string) (codes : codes) : llmodule =
  let llmod = create_module context modid in
  List.iter (fun {name; params; _} -> codegen_proto llmod name params) codes;
  List.iter (codegen_func llmod) codes;
  llmod

let format (path : string) (llmod : llmodule) : unit =
  Llvm.print_module (Filename.remove_extension path ^ ".ll") llmod
