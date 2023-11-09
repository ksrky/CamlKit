type t =
  | NIL
  | LDC of int
  | LD of int * int
  | CAR
  | CDR
  | CONS
  | ATOM
  | ADD
  | SUB
  | MUL
  | DIV
  | EQ
  | NE
  | LT
  | LE
  | SEL of t list * t list
  | JOIN
  | LDF of t list
  | RTN
  | AP
  | DUM
  | RAP
  | STOP
  | READI
  | PRINTI

val load_instrs : t list -> unit

val run_commands : unit -> unit

val show_instrs : t list -> string
