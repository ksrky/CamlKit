let parse (filename : string) : Language.Syntax.exp =
  let file = open_in filename in
  let inp = really_input_string file (in_channel_length file) in
  let linebuf = Lexing.from_string inp in
  try Parser.Grammar.prog Parser.Lexer.token linebuf
  with Parser.Grammar.Error -> ErrorMsg.error "parse error"; Language.Syntax.NilExp

let parse_line (inp : string) : Language.Syntax.exp =
  let linebuf = Lexing.from_string inp in
  try Parser.Grammar.prog Parser.Lexer.token linebuf
  with Parser.Grammar.Error -> ErrorMsg.error "parse error"; Language.Syntax.NilExp
