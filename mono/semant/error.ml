module A = Abstract.Syntax
module A = Abstract.Pretty
open Format

let has_error : bool ref = ref false

let scope_error id =
  has_error := true;
  fprintf std_formatter "Not in scope %a" Id.pp_print_id id;
  print_newline ()

let binop_error op ty1 ty2 =
  has_error := true;
  fprintf std_formatter "Invalid operation for %a between types %a and %a"
    A.pp_print_op op A.pp_print_ty0 ty1 A.pp_print_ty0 ty2;
  print_newline ()

let unification_error ty1 ty2 =
  has_error := true;
  fprintf std_formatter "Cannot unify types: %a with %a" A.pp_print_ty0 ty1
    A.pp_print_ty0 ty2;
  print_newline ()

let infinite_type_error tv ty =
  has_error := true;
  fprintf std_formatter "Infinite type: %a ~ %a" A.pp_print_tyvar tv
    A.pp_print_ty0 ty;
  print_newline ()
