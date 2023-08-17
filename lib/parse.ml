let parse (filename : string) : AbsSyn.def list =
  let file = open_in filename in
  let inp = really_input_string file (in_channel_length file) in
  let linebuf = Lexing.from_string inp in
  try Parser.prog Lexer.token linebuf with _ -> ErrorMsg.error "parse error"; []

let parse_line (inp : string) : AbsSyn.exp =
  let linebuf = Lexing.from_string inp in
  try Parser.line Lexer.token linebuf with _ -> ErrorMsg.error "parse error"; AbsSyn.NilExp
