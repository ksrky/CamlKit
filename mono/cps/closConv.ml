open Syntax

type prog = fundef list * exp

type escapes = var list

type locals = id list

let remove_dup xs =
  List.fold_right (fun x xs -> if List.mem x xs then xs else x :: xs) xs []

let ( // ) escs lcls : 'a list =
  List.fold_right (fun x -> List.filter (fun (y, _) -> x <> y)) lcls escs

let lookup_env (escs : escapes) (x : var) : int * escapes =
  let rec find_idx i = function
    | [] -> (i, escs @ [x])
    | y :: ys -> if fst x = fst y then (i, escs) else find_idx (i + 1) ys
  in
  find_idx 1 escs

let fundef_list : fundef list ref = ref []

let append_fundef fundef : unit = fundef_list := fundef :: !fundef_list

let globals : id list ref = ref []

let rec cc_val (escs : escapes) (lcls : locals) : valty -> valty * escapes =
  function
  | Const c, ty -> ((Const c, ty), escs)
  | Var x, ty when List.mem x lcls -> ((Var x, ty), escs)
  | Var f, ty when List.mem f !globals -> ((Glb f, ty), escs)
  | Var x, ty ->
      let _, escs' = lookup_env escs (x, ty) in
      ((Var x, ty), escs')
  | Glb _, _ -> failwith "unreahcble"
  | Lam {vars; body}, _ ->
      let body', escs' = cc_exp [] (List.map fst vars) body in
      let func_id = Id.from_string "func" in
      if escs' = [] then (
        let func_ty = ContTy (List.map snd vars) in
        append_fundef {var= (func_id, func_ty); params= vars; body= body'};
        ((Glb func_id, func_ty), escs) )
      else
        let env_id = Id.from_string "env" in
        let env_ty = TupleTy (List.map snd escs') in
        let func_ty = ContTy (env_ty :: List.map snd vars) in
        append_fundef
          { var= (func_id, func_ty)
          ; params= (env_id, env_ty) :: vars
          ; body= mk_let (mk_projs (Var env_id, env_ty) escs') body' };
        ( ( Tuple
              [ (Glb func_id, func_ty)
              ; (Tuple (List.map (fun (id, ty) -> (Var id, ty)) escs'), env_ty)
              ]
          , TupleTy [func_ty; env_ty] )
        , remove_dup (escs @ escs') // lcls )
  | Tuple vtys, _ ->
      let vtys', escs' = cc_val_seq escs lcls vtys in
      ((Tuple vtys', TupleTy (List.map snd vtys')), escs')

and cc_val_seq (escs : escapes) (lcls : locals) (vals : valty list) :
    valty list * escapes =
  let loop acc escs = function
    | [] -> (List.rev acc, escs)
    | val_ :: vals ->
        let val', escs' = cc_val escs lcls val_ in
        (val' :: acc, escs')
  in
  loop [] escs vals

and cc_exp (escs : escapes) (lcls : locals) : exp -> exp * escapes = function
  | Let {dec; body} ->
      let dec', escs1, lcls' = cc_dec escs lcls dec in
      let body', escs2 = cc_exp escs1 lcls' body in
      (Let {dec= dec'; body= body'}, escs2)
  | Letrec {fundefs; body} ->
      let glbs = List.map (fun {var} -> var) fundefs in
      globals := List.map fst glbs;
      let cc_fundef ({var; params; body} : fundef) : escapes =
        let body', escs' = cc_exp [] (List.map fst params) body in
        let env_id = Id.from_string "env" in
        let env_ty = TupleTy (List.map snd params) in
        append_fundef
          { var
          ; params= (env_id, env_ty) :: params
          ; body= mk_let (mk_projs (Var env_id, env_ty) escs') body' };
        escs'
      in
      let escs1 = List.concat (List.map cc_fundef fundefs) in
      let escs2 = remove_dup (escs @ escs1) // lcls in
      cc_exp escs2 lcls body
  | App {fcn; args} -> (
      let fcn', escs1 = cc_val escs lcls fcn in
      let args', escs2 = cc_val_seq escs1 lcls args in
      match fcn' with
      (** A closure is always of tuple type with two elements
          TODO: replace TupleTy with ClosTy  *)
      | _, TupleTy [code_ty; env_ty] ->
          let env_id = Id.from_string "env" in
          let code_id = Id.from_string "code" in
          let clos_id = Id.from_string "clos" in
          let clos_ty = TupleTy [code_ty; env_ty] in
          ( mk_let
              [ ValDec {var= (clos_id, clos_ty); val_= fcn'}
              ; ProjDec
                  {var= (code_id, code_ty); val_= (Var clos_id, clos_ty); idx= 1}
              ; ProjDec
                  {var= (env_id, env_ty); val_= (Var clos_id, clos_ty); idx= 2}
              ]
              (App
                 { fcn= (Var code_id, code_ty)
                 ; args= (Var env_id, env_ty) :: args' } )
          , escs2 )
      | _ -> (App {fcn= fcn'; args= args'}, escs2) )
  | If {cond; then_; else_} ->
      let cond', escs1 = cc_val escs lcls cond in
      let then', escs2 = cc_exp escs1 lcls then_ in
      let else', escs3 = cc_exp escs2 lcls else_ in
      (If {cond= cond'; then_= then'; else_= else'}, escs3)
  | Halt val_ ->
      let val', escs' = cc_val escs lcls val_ in
      (Halt val', escs')

and cc_dec (escs : escapes) (lcls : locals) : dec -> dec * escapes * locals =
  function
  | ValDec {var; val_} ->
      let val', escs' = cc_val escs lcls val_ in
      (ValDec {var; val_= val'}, escs', fst var :: lcls)
  | PrimDec {var; left; oper; right} ->
      let left', escs1 = cc_val escs lcls left in
      let right', escs2 = cc_val escs1 lcls right in
      (PrimDec {var; left= left'; oper; right= right'}, escs2, fst var :: lcls)
  | ProjDec _ -> failwith "not implemented"

let cc_prog (exp : exp) : fundef list * exp =
  fundef_list := [];
  let exp = cc_exp [] [] exp |> fst in
  (!fundef_list, exp)

open Format

let pp_print_prog ppf (fundefs, exp) =
  fprintf ppf "letrec@;<1 2>@[<v 0>%a@]@.in@;<1 2>@[<v 0>%a@]@."
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ and ") pp_print_fundef)
    fundefs pp_print_exp exp;
  print_newline ()

let print_prog = pp_print_prog std_formatter
