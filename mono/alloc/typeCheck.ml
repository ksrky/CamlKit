open Syntax

type tyctx = ty Id.table

let empty = Id.Table.empty

let check ty1 ty2 =
  if ty1 = ty2 then ()
  else
    match (ty1, ty2) with
    | PtrTy None, PtrTy _ -> ()
    | PtrTy _, PtrTy None -> ()
    | _ ->
        Format.eprintf "type mismatch: %a vs %a\n" pp_print_ty ty1 pp_print_ty
          ty2;
        raise Utils.Bug_error

let rec typ_val (ctx : tyctx) : value -> ty = function
  | Const (I1 _) -> I1Ty
  | Const (I32 _) -> I32Ty
  | Var (x, ty) ->
      check (Id.Table.find x ctx) ty;
      ty
  | Glb (f, ty) ->
      check (Id.Table.find f ctx) ty;
      ty

let rec check_exp (ctx : tyctx) : exp -> unit = function
  | Let {dec; body} ->
      let ctx' = check_dec ctx dec in
      check_exp ctx' body
  | If {oper; left; right; then_; else_} ->
      ( match oper with
      | Lt | Le | Gt | Ge ->
          check (typ_val ctx left) I32Ty;
          check (typ_val ctx right) I32Ty
      | Eq | Ne -> () );
      check (typ_val ctx left) I32Ty;
      check (typ_val ctx right) I32Ty;
      check_exp ctx then_;
      check_exp ctx else_
  | Return val_ -> check (typ_val ctx val_) return_type

and check_dec (ctx : tyctx) : dec -> tyctx = function
  | ValDec {var; val_} ->
      check (typ_val ctx val_) (snd var);
      Id.Table.add (fst var) (snd var) ctx
  | PrimDec {var; left; oper; right} ->
      check (snd var) I32Ty;
      check (typ_val ctx left) I32Ty;
      check (typ_val ctx right) I32Ty;
      Id.Table.add (fst var) (snd var) ctx
  | CallDec {var; fcn; args} -> (
    match deref_type (typ_val ctx fcn) with
    | FunTy (ret_ty, arg_tys) ->
        List.iter2 (fun v ty -> check (typ_val ctx v) ty) args arg_tys;
        check (snd var) ret_ty;
        Id.Table.add (fst var) (snd var) ctx
    | ty ->
        Format.eprintf "required function type, but got %a@." pp_print_ty ty;
        raise Utils.Bug_error )
  | SubscrDec {var; val_; idx} ->
      ( match deref_type (typ_val ctx val_) with
      | StrctTy tys -> (
        try check (List.nth tys (idx - 1)) (snd var)
        with _ -> Format.eprintf "struct type out of bounds" )
      | _ -> Format.eprintf "required struct type" );
      Id.Table.add (fst var) (snd var) ctx
  | MallocDec {var; len} ->
      ( match deref_type (snd var) with
      | StrctTy tys when List.length tys = len -> ()
      | _ -> Format.eprintf "required strcut type of length %i" len );
      Id.Table.add (fst var) (snd var) ctx
  | UpdateDec {var; strct; idx; val_} ->
      ( match deref_type (typ_val ctx strct) with
      | StrctTy tys -> (
          check (PtrTy (Some (StrctTy tys))) (snd var);
          try check (List.nth tys (idx - 1)) (typ_val ctx val_)
          with _ -> Format.eprintf "struct type out of bounds" )
      | _ -> Format.eprintf "required struct type" );
      Id.Table.add (fst var) (snd var) ctx

let check_heap (ctx : tyctx) : heap -> unit = function
  | Code {var; params; body} ->
      let ctx' = Id.Table.add_list params ctx in
      check_exp ctx' body
  | Tuple {name; vals} -> failwith ""
(* let tys = List.map (typ_val ctx) vals in
   check (StrctTy tys) (snd name *)

let check_prog (ctx : tyctx) ((heaps, exp) : prog) : unit =
  let vars =
    List.map (function Code {var; _} -> var | Tuple _ -> failwith "") heaps
  in
  let ctx' = Id.Table.add_list vars ctx in
  List.iter (check_heap ctx') heaps;
  check_exp ctx' exp
