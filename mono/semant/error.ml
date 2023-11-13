let has_error : bool ref = ref false

let error (msg : string) =
  has_error := true;
  print_endline ("error: " ^ msg)

exception Error

let impossible msg =
  print_endline ("Compiler bug: " ^ msg);
  raise Error
