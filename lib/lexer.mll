{
open Parser

exception Error of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let space = ['\t' '\r' ' ']
let newline = '\n'

let decimal = digit+
let id = alpha (alpha | digit | '_')*

rule token = parse
| space+        { token lexbuf }
| newline       { Lexing.new_line lexbuf; token lexbuf }

(* comments *)
| "//"          { line_comment lexbuf }
| "/*"          { block_comment lexbuf }

(* reserved keywords *)
| "let"         { LET }
| "in"          { IN }
| "end"         { END }
| "if"          { IF }
| "then"        { THEN }
| "else"        { ELSE }
| "nil"         { NIL }
| "and"         { AND }
| "fun"         { FUN }

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
| "&"           { AND_ }
| "|"           { OR }

(* integer and identifier *)
| decimal as i  { INT (int_of_string i) }
| id as s       { ID s }

| _             { ErrorMsg.error "illegal charcter"; token lexbuf }

(* end of a file *)
| eof           { EOF }

and line_comment = parse
| ('\n' | eof)  { Lexing.new_line lexbuf; token lexbuf }
| _             { line_comment lexbuf }

and block_comment = parse
| "*/"          { token lexbuf }
| eof           { ErrorMsg.error "unterminated comment"; token lexbuf }
| newline       { Lexing.new_line lexbuf; block_comment lexbuf }
| _             { block_comment lexbuf }