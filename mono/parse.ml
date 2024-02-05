let has_error = ref false

let parse (filename : string) : Abstract.Syntax.exp =
  let file = open_in filename in
  let inp = really_input_string file (in_channel_length file) in
  let linebuf = Lexing.from_string inp in
  try Parser.Grammar.prog Parser.Lexer.token linebuf
  with Parser.Grammar.Error ->
    Format.print_string "parse error";
    has_error := true;
    Abstract.Syntax.NilExp

let parse_line (inp : string) : Abstract.Syntax.exp =
  let linebuf = Lexing.from_string inp in
  try Parser.Grammar.prog Parser.Lexer.token linebuf
  with Parser.Grammar.Error ->
    Format.print_string "parse error";
    has_error := true;
    Abstract.Syntax.NilExp
