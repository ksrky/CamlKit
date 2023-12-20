open Syntax

type tyctx = ty Id.table

let empty = Id.Table.empty

let check ty1 ty2 =
  if ty1 = ty2 then ()
  else
    Format.fprintf Format.err_formatter "type mismatch: %a vs %a\n"
      (pp_print_ty 0) ty1 (pp_print_ty 0) ty2

let rec check_exp (ctx : tyctx) : expty -> unit = function
  | Const (Int _), IntTy | Const (Bool _), BoolTy -> ()
  | Var (x, ty), ty' ->
      check (Id.Table.find x ctx) ty;
      check ty ty'
  | App {fcn; arg}, ty -> (
      check_exp ctx fcn;
      check_exp ctx arg;
      match snd fcn with
      | FunTy (arg_ty, res_ty) ->
          check arg_ty (snd arg);
          check res_ty ty
      | _ -> failwith "function type required" )
  | Lam {var; body}, FunTy (arg_ty, res_ty) ->
      check (snd var) arg_ty;
      check_exp (Id.Table.add (fst var) arg_ty ctx) body;
      check res_ty (snd body)
  | Prim {left; oper; right}, ty -> (
      check_exp ctx left;
      check_exp ctx right;
      match oper with
      | Add | Sub | Mul | Div -> check IntTy ty
      | Eq | Ne | Lt | Gt | Le | Ge -> check BoolTy ty )
  | If {cond; then_; else_}, ty ->
      check_exp ctx cond;
      check_exp ctx then_;
      check_exp ctx else_;
      check BoolTy (snd cond);
      check (snd then_) ty;
      check (snd else_) ty
  | Let {vars; bnds; body}, ty ->
      let ctx' =
        List.fold_left (fun ctx (x, ty) -> Id.Table.add x ty ctx) ctx vars
      in
      List.iter (check_exp ctx') bnds;
      check_exp ctx' body;
      check (snd body) ty
  | _ -> failwith "type mismatch"

let check_prog : tyctx -> prog -> unit = check_exp
