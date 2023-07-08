exception Error

let error (msg : string) = print_endline msg

let impossible msg =
  List.iter print_string [": Error: Compiler bug: "; msg; "\n"];
  flush stdout;
  raise Error