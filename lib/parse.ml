let parse (filename : string) : AbsSyn.exp =
  let file = open_in filename in
  let inp = really_input_string file (in_channel_length file) in
  let linebuf = Lexing.from_string inp in
  try Parser.prog Lexer.token linebuf with _ -> ErrorMsg.error "parse error"; AbsSyn.NilExp

let parse_line (inp : string) : AbsSyn.exp =
  let linebuf = Lexing.from_string inp in
  try Parser.prog Lexer.token linebuf with _ -> ErrorMsg.error "parse error"; AbsSyn.NilExp
