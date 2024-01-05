open Syntax

type tyctx = ty Id.table

let empty = Id.Table.empty

let check ty1 ty2 =
  if ty1 = ty2 then ()
  else (
    Format.eprintf "type mismatch: %a vs %a\n" pp_print_ty ty1 pp_print_ty ty2;
    raise Utils.Bug_error )

let rec check_val (ctx : tyctx) : valty -> unit = function
  | Const (Int _), IntTy | Const (Bool _), BoolTy -> ()
  | Var x, ty -> check (Id.Table.find x ctx) ty
  | Glb f, ty -> check (Id.Table.find f ctx) ty
  | Lam {vars; body}, ty ->
      check (ContTy (List.map snd vars)) ty;
      let ctx' = Id.Table.add_list vars ctx in
      check_exp ctx' body
  | Tuple vtys, TupleTy tys ->
      List.iter2 (fun (_, ty1) ty2 -> check ty1 ty2) vtys tys
  | val_, ty ->
      Format.eprintf "given expression '%a'@ does not have type %a@."
        pp_print_val0 val_ pp_print_ty ty

and check_exp (ctx : tyctx) : exp -> unit = function
  | Let {dec; body} ->
      let ctx' = check_dec ctx dec in
      check_exp ctx' body
  | Letrec {defs; body} ->
      let ctx' =
        List.fold_right (fun {var= id, ty} -> Id.Table.add id ty) defs ctx
      in
      List.iter (check_def ctx') defs;
      check_exp ctx' body
  | App {fcn; args} ->
      check_val ctx fcn;
      List.iter (check_val ctx) args
  | If {cond; then_; else_} ->
      check_val ctx cond; check_exp ctx then_; check_exp ctx else_
  | Halt vty -> check_val ctx vty

and check_def (ctx : tyctx) ({var; params; body} : def) : unit =
  check (ContTy (List.map snd params)) (snd var);
  let ctx' = Id.Table.add_list params ctx in
  check_exp ctx' body

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
      ( match snd val_ with
      | TupleTy tys -> check (snd var) (List.nth tys (idx - 1))
      | ty ->
          Format.eprintf "Expected tuple type,@ but got %a@." pp_print_ty ty;
          raise Utils.Bug_error );
      check_val ctx val_;
      Id.Table.add (fst var) (snd var) ctx
  | UnpackDec {tyvar; var; val_} -> failwith "not implemented"

let check_prog : tyctx -> prog -> unit = check_exp

let check_prog_cc tyctx ((fundefs, exp) : ClosConv.prog) : unit =
  let ctx' =
    List.fold_right (fun {var= id, ty} -> Id.Table.add id ty) fundefs tyctx
  in
  List.iter (check_def ctx') fundefs;
  check_exp ctx' exp
