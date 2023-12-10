open Syntax

type prog = fundef list * exp

type escapes = id list

type locals = id list

let mk_vars = List.map (fun x -> Var x)

let proj_decs val_ (names : id list) : dec list =
  List.mapi (fun i name -> ProjDec {name; val_; idx= i + 1}) names

let mk_let (decs : dec list) (body : exp) : exp =
  List.fold_right (fun d e -> Let {dec= d; body= e}) decs body

let remove_dup xs =
  List.fold_right (fun x xs -> if List.mem x xs then xs else x :: xs) xs []

let ( // ) init elims : 'a list =
  List.fold_right (fun x -> List.filter (( <> ) x)) elims init

let lookup_env (escs : escapes) (x : id) : int * escapes =
  let rec find_idx i = function
    | [] -> (i, escs @ [x])
    | y :: ys -> if x = y then (i, escs) else find_idx (i + 1) ys
  in
  find_idx 1 escs

let fundef_list : fundef list ref = ref []

let append_fundef fundef : unit = fundef_list := fundef :: !fundef_list

let globals : id list ref = ref []

let rec cc_val (escs : escapes) (lcls : locals) : value -> value * escapes =
  function
  | Const c -> (Const c, escs)
  | Var x when List.mem x lcls -> (Var x, escs)
  | Var f when List.mem f !globals -> (Glb f, escs)
  | Var x ->
      let _, escs' = lookup_env escs x in
      (Var x, escs')
  | Glb _ -> failwith "unreahcble"
  | Lam {vars; body} ->
      let env_var = Id.from_string "env" in
      let body', escs' = cc_exp [] vars body in
      let name = Id.from_string "func" in
      if escs' = [] then (
        append_fundef
          {name; vars; body= mk_let (proj_decs (Var env_var) escs') body'};
        (Glb name, escs) )
      else (
        append_fundef
          { name
          ; vars= env_var :: vars
          ; body= mk_let (proj_decs (Var env_var) escs') body' };
        ( Tuple [Glb name; Tuple (mk_vars escs')]
        , remove_dup (escs @ escs') // lcls ) )
  | Tuple _ -> failwith "not implemented"

and cc_val_seq (escs : escapes) (lcls : locals) (vals : value list) :
    value list * escapes =
  let loop (acc : value list) (escs : escapes) :
      value list -> value list * escapes = function
    | [] -> (List.rev acc, escs)
    | val_ :: vals ->
        let val', escs' = cc_val escs lcls val_ in
        (val' :: acc, escs')
  in
  loop [] escs vals

and cc_exp (escs : escapes) (lcls : locals) : exp -> exp * escapes = function
  | Let {dec; body} ->
      let dec', escs1, name = cc_dec escs lcls dec in
      let body', escs2 = cc_exp escs1 (name :: lcls) body in
      (Let {dec= dec'; body= body'}, escs2)
  | Letrec {fundefs; body} ->
      let names = List.map (fun {name} -> name) fundefs in
      let cc_fundef ({name; vars; body} : fundef) : escapes =
        let env_var = Id.from_string "env" in
        globals := names;
        let body', escs' = cc_exp [] vars body in
        append_fundef
          { name
          ; vars= env_var :: vars
          ; body= mk_let (proj_decs (Var env_var) escs') body' };
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
          let clos_var = Id.from_string "clos" in
          let code_var = Id.from_string "code" in
          let env_var = Id.from_string "env" in
          ( mk_let
              ( ValDec {name= clos_var; val_= fcn'}
              :: proj_decs (Var clos_var) [code_var; env_var] )
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

and cc_dec (escs : escapes) (lcls : locals) : dec -> dec * escapes * id =
  function
  | ValDec {name; val_} ->
      let val', escs' = cc_val escs lcls val_ in
      (ValDec {name; val_= val'}, escs', name)
  | PrimDec {name; left; oper; right} ->
      let left', escs1 = cc_val escs lcls left in
      let right', escs2 = cc_val escs1 lcls right in
      (PrimDec {name; left= left'; oper; right= right'}, escs2, name)
  | ProjDec _ -> failwith "not implemented"

let cc_prog (exp : exp) : fundef list * exp =
  fundef_list := [];
  let exp = cc_exp [] [] exp |> fst in
  (!fundef_list, exp)

and ppr_prog (heaps, exp) =
  let heaps = String.concat "\n" (List.map ppr_fundef heaps) in
  let exp = ppr_exp 0 exp in
  Printf.sprintf "%s\n%s" heaps exp
