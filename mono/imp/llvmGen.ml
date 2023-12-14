open Syntax
open Llvm

let context : llcontext = global_context ()

let builder : llbuilder = builder context

let pointer_t = pointer_type

let int_type : lltype = i32_type context

let rec llvm_type : ty -> lltype = function
  | I1Ty -> i1_type context
  | I32Ty -> i32_type context
  | PtrTy -> pointer_type (i8_type context) (* see the comment of Malloc *)
  | FunTy (ret_ty, arg_tys) ->
      function_type (llvm_type ret_ty)
        (Array.of_list (List.map llvm_type arg_tys))
  | StrctTy tys -> struct_type context (Array.of_list (List.map llvm_type tys))

let named_values : (id, llvalue) Hashtbl.t = Hashtbl.create 100

let codegen_const : const -> llvalue = function
  | I1 i -> const_int (i1_type context) i
  | I32 i -> const_int (i32_type context) i

let codegen_val (llmod : llmodule) : value -> llvalue = function
  | Const c -> codegen_const c
  | Var x -> (
    try Hashtbl.find named_values x
    with _ -> failwith ("no such variable " ^ Id.unique_name x) )
  | Glb x -> (
    match lookup_function (Id.unique_name x) llmod with
    | Some func -> func
    | None -> failwith ("no such function " ^ Id.unique_name x) )

let rec codegen_exp (llmod : llmodule) : exp -> unit = function
  | Let {dec; body} -> codegen_dec llmod dec; codegen_exp llmod body
  | App {fcn; args} ->
      let fcn' = codegen_val llmod fcn in
      let args' = Array.of_list (List.map (codegen_val llmod) args) in
      let call_val = build_call fcn' args' "calltmp" builder in
      build_ret call_val builder |> ignore
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
  | Halt val_ -> build_ret (codegen_val llmod val_) builder |> ignore

and codegen_dec (llmod : llmodule) : dec -> unit = function
  | ValDec {name; val_} ->
      let val' = codegen_val llmod val_ in
      Hashtbl.add named_values name val'
  | PrimDec {name; left; oper; right} ->
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
      Hashtbl.add named_values name prim_val
  | SubscrDec {name; val_; idx} ->
      let tuple_val = codegen_val llmod val_ in
      let elm_ptr = build_struct_gep tuple_val (idx - 1) "elmptr" builder in
      let elm_val = build_load elm_ptr "elmtmp" builder in
      Hashtbl.add named_values name elm_val
  | MallocDec {name; len} ->
      let strct_val =
        (** Malloc: build_malloc creates malloc prototype of type i8*
            https://llvm.org/doxygen/IRBuilder_8cpp_source.html#l00301 *)
        build_malloc (i32_type context) "envtmp"
          builder
      in
      Hashtbl.add named_values name strct_val
  | UpdateDec {name; var; idx; val_} ->
      let tuple_val = Hashtbl.find named_values var in
      let elm_ptr = build_struct_gep tuple_val (idx - 1) "elmptr" builder in
      build_store (codegen_val llmod val_) elm_ptr builder |> ignore;
      Hashtbl.add named_values name tuple_val

let set_params (func : llvalue) (params : id list) : unit =
  Array.iteri
    (fun i v ->
      let id = List.nth params i in
      set_value_name (Id.unique_name id) v;
      Hashtbl.add named_values id v )
    (Llvm.params func)

let codegen_proto (llmod : llmodule) (name : id) (params : (id * ty) list)
    (ret_ty : ty) : unit =
  let name = Id.unique_name name in
  let param_tys =
    Array.of_list (List.map (fun (_, ty) -> llvm_type ty) params)
  in
  let func_ty = function_type (llvm_type ret_ty) param_tys in
  let func =
    match lookup_function name llmod with
    | None -> declare_function name func_ty llmod
    | Some _ -> failwith "redefinition of function"
  in
  set_params func (List.map fst params)

let codegen_func (llmod : llmodule) : heap -> unit = function
  | Code {name; params; body} -> (
      let name = Id.unique_name name in
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

let codegen_main (llmod : llmodule) (exp : exp) : unit =
  let func_ty = function_type (i32_type context) [||] in
  (* tmp: ret_ty *)
  let func = declare_function "main" func_ty llmod in
  set_params func [];
  Hashtbl.clear named_values;
  let bb = append_block context "main" func in
  position_at_end bb builder;
  try
    codegen_exp llmod exp;
    Llvm_analysis.assert_valid_function func
  with e -> delete_function func; raise e

let codegen (modid : string) ((heaps, exp) : prog) : llmodule =
  let llmod = create_module context modid in
  List.iter
    (function
      | Code {name; params; ret_ty; _} -> codegen_proto llmod name params ret_ty
      | Tuple _ -> () )
    heaps;
  List.iter (codegen_func llmod) heaps;
  codegen_main llmod exp;
  llmod

let format (path : string) (llmod : llmodule) : unit =
  Llvm.print_module (Filename.remove_extension path ^ ".ll") llmod
