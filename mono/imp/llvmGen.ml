module S = Syntax
open Llvm

let context : llcontext = global_context ()

let builder : llbuilder = builder context

let index_int = const_int (i32_type context)

let rec to_lltype : S.ty -> lltype = function
  | I1Ty -> i1_type context
  | I32Ty -> i32_type context
  | PtrTy _ -> pointer_type context
  | FunTy (ret_ty, arg_tys) ->
      function_type (to_lltype ret_ty)
        (Array.of_list (List.map to_lltype arg_tys))
  | StrctTy tys -> struct_type context (Array.of_list (List.map to_lltype tys))

let named_values : (S.id, llvalue) Hashtbl.t = Hashtbl.create 100

let codegen_const : S.const -> llvalue * S.ty = function
  | I1 i -> (const_int (i1_type context) i, I1Ty)
  | I32 i -> (const_int (i32_type context) i, I32Ty)

let codegen_valty (llmod : llmodule) : S.value -> llvalue * S.ty = function
  | Const c -> codegen_const c
  | Var (x, ty) -> (
    try (Hashtbl.find named_values x, ty)
    with _ -> failwith ("no such variable " ^ Id.unique_name x) )
  | Glb (f, ty) -> (
    match lookup_function (Id.unique_name f) llmod with
    | Some func -> (func, ty)
    | None -> failwith ("no such function " ^ Id.unique_name f) )

let codegen_val (llmod : llmodule) (val_ : S.value) : llvalue =
  fst (codegen_valty llmod val_)

let rec codegen_exp (llmod : llmodule) : S.exp -> unit = function
  | Let {dec; body} -> codegen_dec llmod dec; codegen_exp llmod body
  | If {oper; left; right; then_; else_} ->
      let left_val = codegen_val llmod left in
      let right_val = codegen_val llmod right in
      let cond_val =
        ( match oper with
        | Eq -> build_icmp Icmp.Eq
        | Ne -> build_icmp Icmp.Ne
        | Lt -> build_icmp Icmp.Slt
        | Le -> build_icmp Icmp.Sle
        | Gt -> build_icmp Icmp.Sgt
        | Ge -> build_icmp Icmp.Sge )
          left_val right_val "condtmp" builder
      in
      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in
      (* then block *)
      let then_bb = append_block context "then" the_function in
      position_at_end then_bb builder;
      codegen_exp llmod then_;
      let new_then_bb = insertion_block builder in
      (* else block *)
      let else_bb = append_block context "else" the_function in
      position_at_end else_bb builder;
      codegen_exp llmod else_;
      let new_else_bb = insertion_block builder in
      (* building blocks *)
      position_at_end start_bb builder;
      build_cond_br cond_val then_bb else_bb builder |> ignore;
      position_at_end new_then_bb builder;
      position_at_end new_else_bb builder
  | Return val_ -> build_ret (codegen_val llmod val_) builder |> ignore

and codegen_dec (llmod : llmodule) : S.dec -> unit = function
  | ValDec {var= id, ty; val_} ->
      let val' = codegen_val llmod val_ in
      Hashtbl.add named_values id val'
  | PrimDec {var= id, _; left; oper; right} ->
      let left_val = codegen_val llmod left in
      let right_val = codegen_val llmod right in
      let prim_val =
        ( match oper with
        | Add -> build_add
        | Sub -> build_sub
        | Mul -> build_mul
        | Div -> build_sdiv )
          left_val right_val "primtmp" builder
      in
      Hashtbl.add named_values id prim_val
  | CallDec {var= id, ty; fcn; args} ->
      let fcn_val, fcn_ptrty = codegen_valty llmod fcn in
      let fcn_ty = to_lltype (S.deref_type fcn_ptrty) in
      let arg_vals = Array.of_list (List.map (codegen_val llmod) args) in
      let call_val = build_call fcn_ty fcn_val arg_vals "calltmp" builder in
      Hashtbl.add named_values id call_val
  | SubscrDec {var= id, ty; val_; idx} ->
      let strct_val = codegen_val llmod val_ in
      let elm_ptr =
        build_gep (type_of strct_val) strct_val
          [|index_int (idx - 1)|]
          "elmptr" builder
      in
      let elm_val = build_load (to_lltype ty) elm_ptr "elmtmp" builder in
      Hashtbl.add named_values id elm_val
  | MallocDec {var= id, _; len} ->
      let strct_val = build_malloc (i32_type context) "envtmp" builder in
      Hashtbl.add named_values id strct_val
  | UpdateDec {var= id, _; strct; idx; val_} ->
      let strct_val = codegen_val llmod strct in
      let elm_ptr =
        build_gep (type_of strct_val) strct_val
          [|index_int (idx - 1)|]
          "elmptr" builder
      in
      build_store (codegen_val llmod val_) elm_ptr builder |> ignore;
      Hashtbl.add named_values id strct_val

let set_params (func : llvalue) (params : S.id list) : unit =
  Array.iteri
    (fun i v ->
      let id = List.nth params i in
      set_value_name (Id.unique_name id) v;
      Hashtbl.add named_values id v )
    (Llvm.params func)

let codegen_proto (llmod : llmodule) ((id, ty) : S.var) (params : S.var list) =
  let name = Id.unique_name id in
  let param_tys =
    Array.of_list (List.map (fun (_, ty) -> to_lltype ty) params)
  in
  let ret_ty = to_lltype S.return_type in
  let func_ty = function_type ret_ty param_tys in
  let func =
    match lookup_function name llmod with
    | None -> declare_function name func_ty llmod
    | Some _ -> failwith "redefinition of function"
  in
  set_params func (List.map fst params)

let codegen_func (llmod : llmodule) : S.heap -> unit = function
  | Code {var; params; body} -> (
      let name = Id.unique_name (fst var) in
      Hashtbl.clear named_values;
      let func = Option.get (lookup_function name llmod) in
      set_params func (List.map fst params);
      let bb = append_block context name func in
      position_at_end bb builder;
      try
        codegen_exp llmod body;
        Llvm_analysis.assert_valid_function func
      with e -> delete_function func; raise e )
  | Tuple _ -> failwith "not implemented"

let codegen_main (llmod : llmodule) (exp : S.exp) : unit =
  let func_ty = function_type (i32_type context) [||] in
  let func = declare_function "main" func_ty llmod in
  set_params func [];
  Hashtbl.clear named_values;
  let bb = append_block context "main" func in
  position_at_end bb builder;
  try
    codegen_exp llmod exp;
    Llvm_analysis.assert_valid_function func
  with e -> delete_function func; raise e

let codegen (modid : string) ((heaps, exp) : S.prog) : llmodule =
  let llmod = create_module context modid in
  List.iter
    (function
      | S.Code {var; params; _} -> codegen_proto llmod var params
      | Tuple _ -> () )
    heaps;
  List.iter (codegen_func llmod) heaps;
  codegen_main llmod exp;
  llmod

let format (path : string) (llmod : llmodule) : unit =
  Llvm.print_module (Filename.remove_extension path ^ ".ll") llmod
