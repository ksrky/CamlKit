let parse (filename : string) : AbsSyn.exp =
  let file = open_in filename in
  let inp = really_input_string file (in_channel_length file) in
  let linebuf = Lexing.from_string inp in
  Parser.prog Lexer.token linebuf

let parse_line () : AbsSyn.exp =
  let inp = read_line () in
  let linebuf = Lexing.from_string inp in
  Parser.prog Lexer.token linebuf