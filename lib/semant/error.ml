let error (msg : string) = print_endline ("error: " ^ msg)

exception Error

let impossible msg =
  print_endline ("Compiler bug: " ^ msg);
  raise Error
