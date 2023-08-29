open Llvm

let context = global_context ()

let the_module = ref (create_module context "main")

let builder = builder context

let named_values : (Ident.t, llvalue) Hashtbl.t = Hashtbl.create 10

let int_type = i64_type context

let arr_type = array_type int_type 100

let llvm_type ty =
  if ty = Types.tINT then int_type
  else if ty = Types.tBOOL then int_type
  else if ty = Types.tNIL then int_type
  else if ty = Types.tUNIT then int_type
  else if ty = Types.tARRAY then arr_type
  else ErrorMsg.impossible ("unknown type: " ^ IntSyn.ppr_ty ty)

let rec codegen_expr : IntSyn.exp -> llvalue = function
  | Int i -> const_int int_type i
  | Nil -> const_null int_type
  | Var (id, _) -> Hashtbl.find named_values id
  | App (Var (fcn, _), args) ->
      let callee =
        match lookup_function (Ident.unique_name fcn) !the_module with
        | Some callee -> callee
        | None -> ErrorMsg.impossible "unknown function referenced"
      in
      let args' = Array.of_list (List.map codegen_expr args) in
      let ci = build_call callee args' "calltmp" builder in
      (* if (Contraction.hashtbl_find fcn).isrec then set_tail_call true ci;
         tmp: LlvmGen depends on Contraction module *)
      ci
  | Prim (fcn, [lhs; rhs]) when List.mem fcn IntSyn.arith -> (
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      match fcn with
      | "add" -> build_add lhs_val rhs_val "addtmp" builder
      | "sub" -> build_sub lhs_val rhs_val "subtmp" builder
      | "mul" -> build_mul lhs_val rhs_val "multmp" builder
      | "div" -> build_sdiv lhs_val rhs_val "divtmp" builder )
  | Prim (fcn, [lhs; rhs]) when List.mem fcn IntSyn.rel ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      let i =
        match fcn with
        | "eq" -> build_icmp Icmp.Eq lhs_val rhs_val "eqtmp" builder
        | "ne" -> build_icmp Icmp.Ne lhs_val rhs_val "netmp" builder
        | "lt" -> build_icmp Icmp.Ult lhs_val rhs_val "lttmp" builder
        | "le" -> build_icmp Icmp.Ule lhs_val rhs_val "letmp" builder
        | "gt" -> build_icmp Icmp.Ugt lhs_val rhs_val "gttmp" builder
        | "ge" -> build_icmp Icmp.Uge lhs_val rhs_val "getmp" builder
      in
      build_intcast i int_type "booltmp" builder
  | Prim ("load", [arg]) ->
      let arg_val = codegen_expr arg in
      build_load arg_val "loadtmp" builder
  | Prim ("store", [ptr; rhs]) ->
      let ptr_val = codegen_expr ptr in
      let rhs_val = codegen_expr rhs in
      build_store rhs_val ptr_val builder
  | Prim ("gep", [arr; idx]) ->
      let arr_val = codegen_expr arr in
      let idx_val = codegen_expr idx in
      build_gep arr_val (Array.of_list [idx_val]) "geptmp" builder
  | Prim ("array_alloca", [size; init]) ->
      let size_val = codegen_expr size in
      let _init_val = codegen_expr init in
      build_array_alloca int_type size_val "allocatmp" builder
  | Prim (fcn, args) ->
      let callee = Option.get (lookup_function fcn !the_module) in
      let args' = Array.of_list (List.map codegen_expr args) in
      build_call callee args' "calltmp" builder
  | Lam _ | Let _ -> ErrorMsg.impossible "must be removed in Lifting module"
  | If (cond, then_, else_) ->
      let cond' = codegen_expr cond in
      (* Convert condition to a bool by comparing equal to 0.0 *)
      let zero = const_int int_type 0 in
      let cond_val = build_icmp Icmp.Ne cond' zero "ifcond" builder in
      (* Grab the first block so that we might later add the conditional branch
         * to it at the end of the function. *)
      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in
      let then_bb = append_block context "then" the_function in
      (* Emit 'then' value. *)
      position_at_end then_bb builder;
      let then_val = codegen_expr then_ in
      (* Codegen of 'then' can change the current block, update then_bb for the
       * phi. We create a new name because one is used for the phi node, and the
       * other is used for the conditional branch. *)
      let new_then_bb = insertion_block builder in
      (* Emit 'else' value. *)
      let else_bb = append_block context "else" the_function in
      position_at_end else_bb builder;
      let else_val = codegen_expr else_ in
      (* Codegen of 'else' can change the current block, update else_bb for the
       * phi. *)
      let new_else_bb = insertion_block builder in
      (* Emit merge block. *)
      let merge_bb = append_block context "ifcont" the_function in
      position_at_end merge_bb builder;
      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = build_phi incoming "iftmp" builder in
      (* Return to the start block to add the conditional branch. *)
      position_at_end start_bb builder;
      ignore (build_cond_br cond_val then_bb else_bb builder);
      (* Set a unconditional branch at the end of the 'then' block and the
         * 'else' block to the 'merge' block. *)
      position_at_end new_then_bb builder;
      ignore (build_br merge_bb builder);
      position_at_end new_else_bb builder;
      ignore (build_br merge_bb builder);
      (* Finally, set the builder to the end of the merge block. *)
      position_at_end merge_bb builder;
      phi
  | Seq (exp, rest) ->
      ignore (codegen_expr exp);
      codegen_expr rest
  | _ -> ErrorMsg.impossible "malformed intermediate syntax"

and codegen_proto ((name, params) : string * IntSyn.binders) : unit =
  (* Make the function type: double(double,double) etc. *)
  let param_tys =
    Array.of_list
      (List.map
         (fun (id, ty) ->
           print_endline (Ident.name id);
           llvm_type ty )
         params )
  in
  let func_ty = function_type int_type param_tys in
  let func =
    match lookup_function name !the_module with
    | None -> declare_function name func_ty !the_module
    | Some _ -> ErrorMsg.impossible "redefinition of function"
  in
  (* Set names for all arguments. *)
  Array.iteri
    (fun i a ->
      let id, _ = List.nth params i in
      set_value_name (Ident.unique_name id) a;
      Hashtbl.add named_values id a )
    (Llvm.params func)

let codegen_func : IntSyn.frag -> unit = function
  | {name; params; body} -> (
      Hashtbl.clear named_values;
      let name = name in
      let func = Option.get (lookup_function name !the_module) in
      Array.iteri
        (fun i a ->
          let id, _ = List.nth params i in
          set_value_name (Ident.unique_name id) a;
          Hashtbl.add named_values id a )
        (Llvm.params func);
      (* Create a new basic block to start insertion into. *)
      let bb = append_block context name func in
      position_at_end bb builder;
      try
        let ret_val = codegen_expr body in
        (* Finish off the function. *)
        let _ = build_ret ret_val builder in
        (* Validate the generated code, checking for consistency. *)
        Llvm_analysis.assert_valid_function func
      with e -> delete_function func; raise e )

let codegen_prims () : unit =
  codegen_proto ("printi", [(Ident.from_string "x", Types.tINT)]);
  codegen_proto ("readi", [(Ident.from_string "x", Types.tUNIT)])

let codegen (modid : string) (frags : IntSyn.frags) : unit =
  the_module := create_module context modid;
  codegen_prims ();
  List.iter (fun {IntSyn.name; params; _} -> codegen_proto (name, params)) frags;
  List.iter (fun frag -> codegen_func frag) frags
