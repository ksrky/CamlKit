open Stack

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
  | ST
  | DIS
  | MALLOC
  | LDS

let load_command (i : int) (n : int) : int = make_cons (make_int n) i

let rec load_instr i = function
  | NIL -> load_command i 0
  | LDC x ->
      let j = make_cons (make_int x) i in
      load_command j 1
  | LD (x, y) ->
      let j = make_cons (make_cons x y) i in
      load_command j 2
  | ATOM -> load_command i 3
  | CAR -> load_command i 4
  | CDR -> load_command i 5
  | CONS -> load_command i 6
  | ADD -> load_command i 7
  | SUB -> load_command i 8
  | MUL -> load_command i 9
  | DIV -> load_command i 10
  | EQ -> load_command i 11
  | NE -> load_command i 12
  | LT -> load_command i 13
  | LE -> load_command i 14
  | SEL (ts, fs) ->
      let cf = make_cons (load_instrs fs) i in
      let ct = make_cons (load_instrs ts) cf in
      load_command ct 15
  | JOIN -> load_command i 16
  | LDF es ->
      let f = make_cons (load_instrs es) i in
      load_command f 17
  | RTN -> load_command i 18
  | AP -> load_command i 19
  | DUM -> load_command i 20
  | RAP -> load_command i 21
  | STOP -> load_command i 22
  | READI -> load_command i 23
  | PRINTI -> load_command i 24
  | ST -> load_command i 25
  | DIS -> load_command i 26
  | MALLOC -> load_command i 27
  | LDS -> load_command i 28

and load_instrs instrs = List.fold_left load_instr 0 (List.rev instrs)

let load_instrs instrs : unit = c := load_instrs instrs

let run_command () : unit =
  match get_int (pop c) with
  | 0 (* NIL *) -> push (make_int 0) s
  | 1 (* LDC *) ->
      let x = pop c in
      push x s
  | 2 (* LD *) ->
      let ij = pop c in
      push (locate ij !e) s
  | 3 (* ATOM *) ->
      let a = pop s in
      push (atom a) s
  | 4 (* CAR *) ->
      let a = pop s in
      push (car a) s
  | 5 (* CDR *) ->
      let a = pop s in
      push (cdr a) s
  | 6 (* CONS *) -> binop s make_cons
  | 7 (* ADD *) -> binop s (fun a b -> make_int (get_int a + get_int b))
  | 8 (* SUB *) -> binop s (fun a b -> make_int (get_int a - get_int b))
  | 9 (* MUL *) -> binop s (fun a b -> make_int (get_int a * get_int b))
  | 10 (* DIV *) -> binop s (fun a b -> make_int (get_int a / get_int b))
  | 11 (* EQ *) -> binop s (fun a b -> make_int (if get_int a = get_int b then 1 else 0))
  | 12 (* NE *) -> binop s (fun a b -> make_int (if get_int a <> get_int b then 1 else 0))
  | 13 (* LT *) -> binop s (fun a b -> make_int (if get_int a < get_int b then 1 else 0))
  | 14 (* LE *) -> binop s (fun a b -> make_int (if get_int a <= get_int b then 1 else 0))
  | 15 (* SEL *) ->
      let x = match get_int (pop s) with 0 -> false | _ -> true in
      let ct = pop c in
      let cf = pop c in
      push !c d;
      c := if x then ct else cf
  | 16 (* JOIN *) -> c := pop d
  | 17 (* LDF *) ->
      let f = pop c in
      push (make_cons f !e) s
  | 18 (* RTN *) ->
      let x = pop s in
      s := pop d;
      push x s;
      e := pop d;
      c := pop d
  | 19 (* AP *) ->
      let f, e' = get_cons (pop s) in
      let v = pop s in
      push !c d;
      c := f;
      push !e d;
      e := e';
      push v e;
      push !s d;
      s := 0
  | 20 (* DUM *) -> push 0 e
  | 21 (* RAP *) ->
      let f, ne = get_cons (pop s) in
      let v = pop s in
      push !c d;
      c := f;
      push (cdr !e) d;
      e := rplaca ne v;
      push !s d;
      s := 0
  | 22 (* STOP *) -> c := 0
  | 23 (* READI *) ->
      let x = read_int () in
      push (make_int x) s
  | 24 (* PRINTI *) ->
      let x = pop s in
      print_int (get_int x);
      push (make_int 0) s
  | 25 (* ST *) ->
      let i = get_int (pop s) in
      let x = get_int (pop s) in
      store i x;
      push (make_int 0) s
  | 26 (* DIS *) -> ignore (pop s)
  | 27 (* MALLOC *) ->
      let n = get_int (pop s) in
      let x = get_int (pop s) in
      push (make_int (array_alloca n x)) s
  | 28 (* LDS *) -> push (get_int (pop s)) s
  | _ -> ErrorMsg.impossible "Invalid operation"

let rec run_commands () : unit = if !c = 0 then () else (run_command (); run_commands ())

let rec show_instrs (instrs : t list) : string =
  match instrs with
  | [] -> ""
  | [instr] -> show_instr instr
  | instr :: rest -> show_instr instr ^ " " ^ show_instrs rest

and show_instr : t -> string = function
  | NIL -> "NIL"
  | LDC x -> "LDC " ^ string_of_int x
  | LD (i, j) -> "LD " ^ string_of_int i ^ "." ^ string_of_int j ^ ""
  | CAR -> "CAR"
  | CDR -> "CDR"
  | CONS -> "CONS"
  | ATOM -> "ATOM"
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | EQ -> "EQ"
  | NE -> "NE"
  | LT -> "LT"
  | LE -> "LE"
  | SEL (ct, cf) -> "SEL (" ^ show_instrs ct ^ ", " ^ show_instrs cf ^ ")"
  | JOIN -> "JOIN"
  | LDF f -> "LDF (" ^ show_instrs f ^ ")"
  | RTN -> "RTN"
  | AP -> "AP"
  | DUM -> "DUM"
  | RAP -> "RAP"
  | STOP -> "STOP"
  | READI -> "READI"
  | PRINTI -> "PRINTI"
  | ST -> "ST"
  | DIS -> "DIS"
  | MALLOC -> "MALLOC"
  | LDS -> "LDS"
