exception Error

let error (msg : string) = print_string ("error: " ^ msg)

let impossible msg =
  print_endline ("Compiler bug: " ^ msg);
  raise Error
