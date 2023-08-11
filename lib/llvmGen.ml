open Llvm

exception Error of string

let context = global_context ()

let the_module = create_module context "my cool jit"

let builder = builder context

let named_values : (Ident.t, llvalue) Hashtbl.t = Hashtbl.create 10

let int_type = i64_type context

let rec codegen_expr = function
  | IntSyn.Int i -> const_int int_type i
  | IntSyn.Nil -> const_null int_type
  | IntSyn.Var id -> Hashtbl.find named_values id
  | IntSyn.App (Var fcn, args) ->
      (* Look up the name in the module table. *)
      let callee =
        match lookup_function (Ident.to_string fcn) the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      let args' = Array.of_list (List.map codegen_expr args) in
      build_call callee args' "calltmp" builder
  | IntSyn.If (cond, then_, else_) ->
      let cond' = codegen_expr cond in
      (* Convert condition to a bool by comparing equal to 0.0 *)
      let zero = const_int int_type 0 in
      let cond_val = build_fcmp Fcmp.One cond' zero "ifcond" builder in
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

let codegen_func : IntSyn.def -> llvalue = function
  | {name; params; body} -> (
      Hashtbl.clear named_values;
      (* Make the function type: double(double,double) etc. *)
      let param_tys = Array.make (List.length params) int_type in
      let func_ty = function_type int_type param_tys in
      let func =
        match lookup_function (Ident.to_string name) the_module with
        | None ->
            declare_function (Ident.to_string name) func_ty the_module
            (* If 'f' conflicted, there was already something named 'name'. If it
               * has a body, don't allow redefinition or reextern. *)
        | Some _ -> raise (Error "redefinition of function")
      in
      (* Set names for all arguments. *)
      Array.iteri
        (fun i a ->
          let id = List.nth params i in
          set_value_name (Ident.to_string id) a;
          Hashtbl.add named_values id a )
        (Llvm.params func);
      (* Create a new basic block to start insertion into. *)
      let bb = append_block context "entry" func in
      position_at_end bb builder;
      try
        let ret_val = codegen_expr body in
        (* Finish off the function. *)
        let _ = build_ret ret_val builder in
        (* Validate the generated code, checking for consistency. *)
        Llvm_analysis.assert_valid_function func;
        func
      with e -> delete_function func; raise e )
