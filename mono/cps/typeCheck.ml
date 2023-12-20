open Syntax
open Format

type tyctx = ty Id.table

let empty = Id.Table.empty

let check ty1 ty2 =
  if ty1 = ty2 then ()
  else
    fprintf err_formatter "type mismatch: %a vs %a\n" pp_print_ty ty1
      pp_print_ty ty2

let rec check_val (ctx : tyctx) : valty -> unit = function
  | Const (Int _), IntTy | Const (Bool _), BoolTy -> ()
  | Var x, ty -> check (Id.Table.find x ctx) ty
  | Glb f, ty -> check (Id.Table.find f ctx) ty
  | Lam {vars; body}, ty ->
      check (ContTy (List.map snd vars)) ty;
      let ctx' = Id.Table.add_seq (List.to_seq vars) ctx in
      check_exp ctx' body
  | Tuple vtys, TupleTy tys ->
      List.iter2 (fun (_, ty1) ty2 -> check ty1 ty2) vtys tys
  | _, _ -> failwith "type mismatch"

and check_exp (ctx : tyctx) : exp -> unit = function
  | Let {dec; body} ->
      let ctx' = check_dec ctx dec in
      check_exp ctx' body
  | Letrec {fundefs; body} -> ()
  | App {fcn; args} ->
      check_val ctx fcn;
      List.iter (check_val ctx) args
  | If {cond; then_; else_} ->
      check_val ctx cond; check_exp ctx then_; check_exp ctx else_
  | Halt vty -> check_val ctx vty

and check_fundef (ctx : tyctx) ({var; params; body} : fundef) : tyctx =
  check (ContTy (List.map snd params)) (snd var);
  let ctx' = Id.Table.add_seq (List.to_seq params) ctx in
  check_exp ctx' body;
  Id.Table.add (fst var) (snd var) ctx

and check_dec (ctx : tyctx) : dec -> tyctx = function
  | ValDec {var; val_} ->
      check (snd var) (snd val_);
      check_val ctx val_;
      Id.Table.add (fst var) (snd var) ctx
  | PrimDec {var; left; oper; right} ->
      ( match oper with
      | Add | Sub | Mul | Div -> check (snd var) IntTy
      | Eq | Ne | Lt | Le | Gt | Ge -> check (snd var) BoolTy );
      check_val ctx left;
      check_val ctx right;
      Id.Table.add (fst var) (snd var) ctx
  | ProjDec {var; val_; idx} ->
      ( match snd var with
      | TupleTy tys -> check (snd var) (List.nth tys idx)
      | _ -> failwith "tuple type required" );
      check_val ctx val_;
      Id.Table.add (fst var) (snd var) ctx

let check_prog : tyctx -> prog -> unit = check_exp
