open Syntax

type prog = def list * expty

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

let def_list : def list ref = ref []

let append_def def : unit = def_list := def :: !def_list

let globals : var list ref = ref []

let rec cc_val (escs : escapes) (lcls : locals) : valty -> valty * escapes =
  function
  | Const c, ty -> ((Const c, ty), escs)
  | Var x, ty when List.mem x lcls -> ((Var x, ty), escs)
  | Var x, ty -> (
    match List.assoc_opt x !globals with
    | Some ty' -> ((Glb x, ty'), escs)
    | None ->
        let _, escs' = lookup_env escs (x, ty) in
        ((Var x, ty), escs') )
  | Glb _, _ -> raise Utils.Unreachable
  | Lam {vars; body}, _ ->
      let body', escs' = cc_expty [] (List.map fst vars) body in
      let func_id = Id.from_string "func" in
      if escs' = [] then (
        let func_ty = FunTy (List.map snd vars, snd body) in
        append_def {var= (func_id, func_ty); params= vars; body= body'};
        ((Glb func_id, func_ty), escs) )
      else
        let env_id = Id.from_string "env" in
        let env_ty = TupleTy (List.map snd escs') in
        let func_ty = CodeTy (env_ty, List.map snd vars, snd body) in
        let proj_decs =
          List.mapi
            (fun i var -> ProjDec {var; val_= (Var env_id, env_ty); idx= i + 1})
            escs
        in
        append_def
          { var= (func_id, func_ty)
          ; params= (env_id, env_ty) :: vars
          ; body= mk_let proj_decs body' };
        ( ( Tuple
              [ (Glb func_id, func_ty)
              ; (Tuple (List.map (fun (id, ty) -> (Var id, ty)) escs'), env_ty)
              ]
          , TupleTy [func_ty; env_ty] )
        , remove_dup (escs @ escs') // lcls )
  | Tuple _, _ -> raise Utils.Unreachable

and cc_vals escs lcls vals : valty list * escapes =
  let loop acc escs = function
    | [] -> (List.rev acc, escs)
    | val_ :: vals ->
        let val', escs' = cc_val escs lcls val_ in
        (val' :: acc, escs')
  in
  loop [] escs vals

and cc_exp escs lcls : exp -> exp * escapes = function
  | Let {dec; body} ->
      let decs, escs1, lcls' = cc_dec escs lcls dec in
      let body', escs2 = cc_expty escs1 lcls body in
      (mk_let decs body' |> fst, escs2)
  | If {cond; then_; else_} ->
      let cond', escs1 = cc_val escs lcls cond in
      let then', escs2 = cc_expty escs1 lcls then_ in
      let else', escs3 = cc_expty escs2 lcls else_ in
      (If {cond= cond'; then_= then'; else_= else'}, escs3)
  | Ret val_ ->
      let val', escs' = cc_val escs lcls val_ in
      (Ret val', escs')

and cc_expty escs lcls (exp, ty) =
  let exp', escs' = cc_exp escs lcls exp in
  ((exp', ty), escs')

and cc_dec (escs : escapes) (lcls : locals) : dec -> dec list * escapes * locals
    = function
  | ValDec {var; val_} ->
      let val', escs' = cc_val escs lcls val_ in
      ([ValDec {var; val_= val'}], escs', fst var :: lcls)
  | CallDec {var; fcn; args} -> (
      let fcn', escs1 = cc_val escs lcls fcn in
      let args', escs2 = cc_vals escs1 lcls args in
      match fcn' with
      | _, TupleTy [(CodeTy _ as code_ty); env_ty] ->
          let env_id = Id.from_string "env" in
          let code_id = Id.from_string "code" in
          let clos_id = Id.from_string "clos" in
          let clos_ty = TupleTy [code_ty; env_ty] in
          ( [ ValDec {var= (clos_id, clos_ty); val_= fcn'}
            ; ProjDec
                {var= (code_id, code_ty); val_= (Var clos_id, clos_ty); idx= 1}
            ; ProjDec
                {var= (env_id, env_ty); val_= (Var clos_id, clos_ty); idx= 2}
            ; CallDec
                { var
                ; fcn= (Var code_id, code_ty)
                ; args= (Var env_id, env_ty) :: args' } ]
          , escs2
          , fst var :: lcls )
      | _ -> ([CallDec {var; fcn= fcn'; args= args'}], escs2, fst var :: lcls) )
  | PrimDec {var; left; oper; right} ->
      let left', escs1 = cc_val escs lcls left in
      let right', escs2 = cc_val escs1 lcls right in
      ([PrimDec {var; left= left'; oper; right= right'}], escs2, fst var :: lcls)
  | ProjDec _ -> raise Utils.Unreachable

let cc_prog (expty : expty) : prog =
  def_list := [];
  let expty', _ = cc_expty [] [] expty in
  (!def_list, expty')
