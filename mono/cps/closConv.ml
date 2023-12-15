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

let rec cc_val (escs : escapes) (lcls : locals) : value -> value * escapes =
  function
  | Const c -> (Const c, escs)
  | Var x when List.mem (fst x) lcls -> (Var x, escs)
  | Var f when List.mem (fst f) !globals -> (Glb f, escs)
  | Var x ->
      let _, escs' = lookup_env escs x in
      (Var x, escs')
  | Glb _ -> failwith "unreahcble"
  | Lam {vars; body} ->
      let body', escs' = cc_exp [] (List.map fst vars) body in
      let env_var = (Id.from_string "env", TupleTy (List.map snd vars)) in
      if escs' = [] then (
        let var = (Id.from_string "func", ContTy (List.map snd vars)) in
        append_fundef
          {var; params= vars; body= mk_let (mk_projs (Var env_var) escs') body'};
        (Glb var, escs) )
      else
        let var =
          (Id.from_string "func", ContTy (List.map snd (env_var :: vars)))
        in
        append_fundef
          { var
          ; params= env_var :: vars
          ; body= mk_let (mk_projs (Var env_var) escs') body' };
        ( Tuple [Glb var; Tuple (mk_vars escs')]
        , remove_dup (escs @ escs') // lcls )
  | Tuple vals ->
      let vals', escs' = cc_val_seq escs lcls vals in
      (Tuple vals', escs')

and cc_val_seq (escs : escapes) (lcls : locals) (vals : value list) :
    value list * escapes =
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
      let cc_fundef ({var; params; body} : fundef) : escapes =
        globals := List.map fst glbs;
        let body', escs' = cc_exp [] (List.map fst params) body in
        let env_var = (Id.from_string "env", TupleTy (List.map snd params)) in
        append_fundef
          { var
          ; params= env_var :: params
          ; body= mk_let (mk_projs (Var env_var) escs') body' };
        escs'
      in
      let escs' = List.concat (List.map cc_fundef fundefs) in
      let escs' = remove_dup (escs @ escs') // lcls in
      let body', escs'' = cc_exp escs' lcls body in
      (body', escs'')
  | App {fcn; args} -> (
      let fcn', escs1 = cc_val escs lcls fcn in
      let args', escs2 = cc_val_seq escs1 lcls args in
      match fcn' with
      (* tmp: closures are always tuple *)
      | Tuple _ ->
          let env_var = (Id.from_string "env", TupleTy (List.map snd escs2)) in
          let code_var = (Id.from_string "code", ContTy [snd env_var]) in
          let clos_var =
            (Id.from_string "clos", TupleTy [snd code_var; snd env_var])
          in
          ( mk_let
              ( ValDec {var= clos_var; val_= fcn'}
              :: mk_projs (Var clos_var) [code_var; env_var] )
              (App {fcn= Var code_var; args= Var env_var :: args'})
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
