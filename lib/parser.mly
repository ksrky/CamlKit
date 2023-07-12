%{
open AbsSyn
%}

%token EOF
%token <string> ID
%token <int> INT
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
%token LPAREN RPAREN AND_ OR ARROW
%token IF THEN ELSE LET IN END NIL AND FUN REC

%right OR
%right AND_
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start <exp> prog

%%

let prog :=
  | ~=exp; EOF;                           <>

let exp :=
  | ~=id;                                       { VarExp id }
  | NIL;                                        { NilExp }
  | int=INT;                                    { IntExp int }
  | LPAREN; ~=exp; RPAREN;                      { exp }
  | fcn=exp; arg=exp;                           { AppExp{fcn; arg} }
  | FUN; vars=list(id); ARROW; body=exp;        { LamExp{vars; body} }
  | MINUS; right=exp; %prec UMINUS              { OpExp{left=IntExp 0; oper=MinusOp; right} }
  | left=exp; PLUS; right=exp;                  { OpExp{left; oper=PlusOp; right} }
  | left=exp; MINUS; right=exp;                 { OpExp{left; oper=MinusOp; right} }
  | left=exp; TIMES; right=exp;                 { OpExp{left; oper=TimesOp; right} }
  | left=exp; DIVIDE; right=exp;                { OpExp{left; oper=DivideOp; right} }
  | left=exp; EQ; right=exp;                    { OpExp{left; oper=EqOp; right} }
  | left=exp; NEQ; right=exp;                   { OpExp{left; oper=NeqOp; right} }
  | left=exp; LT; right=exp;                    { OpExp{left; oper=LtOp; right} }
  | left=exp; LE; right=exp;                    { OpExp{left; oper=LeOp; right} }
  | left=exp; GT; right=exp;                    { OpExp{left; oper=GtOp; right} }
  | left=exp; GE; right=exp;                    { OpExp{left; oper=GeOp; right} }
  | test=exp; AND_; ~=exp;                      { IfExp{test; then'=exp; else'=IntExp 0} }
  | test=exp; OR; ~=exp;                        { IfExp{test; then'=IntExp 1; else'=exp} }
  | IF; test=exp; THEN; then_=exp; ELSE; else_=exp;
                                                { IfExp{test; then'=then_; else'=else_} }
  | LET; ~=decs; IN; body=exp; END;             { LetExp{decs; body} }
  | LET; REC; ~=decs; IN; body=exp; END;        { LetrecExp{decs; body} }

let decs ==
  | separated_nonempty_list(AND, dec)

let dec :=
  | name=id;  params=list(id); EQ; body=exp;    { {name; params; body} }

let id :=
  | ~=ID;                                       <>