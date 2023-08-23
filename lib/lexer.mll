{
open Parser
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let space = ['\t' '\r' ' ']
let newline = '\n'

let decimal = digit+
let id = (letter | '_') (letter | digit | '_' | ''')*
let integer = digit+

rule token = parse
| space+        { token lexbuf }
| newline       { Lexing.new_line lexbuf; token lexbuf }

(* comment *)
| "(*"          { block_comment lexbuf }

(* reserved keywords *)
| "let"         { LET }
| "in"          { IN }
| "if"          { IF }
| "then"        { THEN }
| "else"        { ELSE }
| "and"         { AND }
| "fun"         { FUN }
| "rec"         { REC }
| "true"        { TRUE }
| "false"       { FALSE }

(* reserved symbols *)
| "("           { LPAREN }
| ")"           { RPAREN }
| "+"           { PLUS }
| "-"           { MINUS }
| "*"           { TIMES }
| "/"           { DIVIDE }
| "="           { EQ }
| "<>"          { NEQ }
| "<"           { LT }
| "<="          { LE }
| ">"           { GT }
| ">="          { GE }
| "&&"          { LAND }
| "||"          { LOR }
| "->"          { LARROW }
| "<-"          { RARROW }
| "."           { DOT }

(* integer and identifier *)
| integer as i  { INT (int_of_string i) }
| id as s       { ID s }

| _             { ErrorMsg.error "illegal charcter"; token lexbuf }

(* end of a file *)
| eof           { EOF }

and block_comment = parse
| "*)"          { token lexbuf }
| eof           { ErrorMsg.error "unterminated comment"; token lexbuf }
| newline       { Lexing.new_line lexbuf; block_comment lexbuf }
| _             { block_comment lexbuf }