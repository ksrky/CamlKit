exception Error

let error (msg : string) = print_string ("error: " ^ msg ^ "\n")

let impossible msg =
  print_endline ("Compiler bug: " ^ msg);
  raise Error
