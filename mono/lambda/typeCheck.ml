open Syntax

type tyctx = ty Id.table

let empty = Id.Table.empty

let check ty1 ty2 =
  if ty1 = ty2 then ()
  else
    Format.eprintf "type mismatch: %a vs %a@." pp_print_ty0 ty1 pp_print_ty0 ty2

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
      | ty ->
          Format.eprintf "Expected function type but got %a" pp_print_ty0 ty;
          raise Utils.Bug_error )
  | Lam {var; body}, FunTy (arg_ty, res_ty) ->
      check (snd var) arg_ty;
      check_exp (Id.Table.add (fst var) arg_ty ctx) body;
      check res_ty (snd body)
  | Fix {var; body}, ty ->
      check (snd var) ty;
      check_exp (Id.Table.add (fst var) ty ctx) body;
      check ty (snd body)
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
  | Let {var= x, var_ty; bnd; body}, ty ->
      let ctx' = Id.Table.add x var_ty ctx in
      check_exp ctx' (bnd, var_ty);
      check_exp ctx' body;
      check (snd body) ty
  | exp, ty ->
      Format.eprintf "given expression '%a'@ does not have type %a@."
        pp_print_exp0 exp pp_print_ty0 ty

let check_prog : tyctx -> prog -> unit = check_exp
