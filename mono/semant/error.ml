open Format

let has_error : bool ref = ref false

let error (fmt : ('a, Format.formatter, unit) format) : 'a =
  has_error := true;
  fprintf std_formatter fmt;
  print_newline ()

exception Error

let impossible msg =
  print_endline ("Compiler bug: " ^ msg);
  raise Error
