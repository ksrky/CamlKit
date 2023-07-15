open Machine

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
  | READC
  | WRITEC

let rec load_instr i = function
  | NIL -> alloc i 0
  | LDC x ->
      let j = make_cons (make_int x) i in
      alloc j 1
  | LD (x, y) ->
      let j = make_cons (make_cons x y) i in
      alloc j 2
  | ATOM -> alloc i 3
  | CAR -> alloc i 4
  | CDR -> alloc i 5
  | CONS -> alloc i 6
  | ADD -> alloc i 7
  | SUB -> alloc i 8
  | MUL -> alloc i 9
  | DIV -> alloc i 10
  | EQ -> alloc i 11
  | NE -> alloc i 12
  | LT -> alloc i 13
  | LE -> alloc i 14
  | SEL (ts, fs) ->
      let cf = make_cons (load_instrs fs) i in
      let ct = make_cons (load_instrs ts) cf in
      alloc ct 15
  | JOIN -> alloc i 16
  | LDF es ->
      let f = make_cons (load_instrs es) i in
      alloc f 17
  | RTN -> alloc i 18
  | AP -> alloc i 19
  | DUM -> alloc i 20
  | RAP -> alloc i 21
  | STOP -> alloc i 22
  | READC -> alloc i 23
  | WRITEC -> alloc i 24

and load_instrs instrs = List.fold_left load_instr 0 (List.rev instrs)

let load_instrs instrs : unit = c := load_instrs instrs

let run_instr () : unit =
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
  | 23 (* READC *) ->
      let x = read_int () in
      push (make_int x) s
  | 24 (* WRITEC *) ->
      let x = pop s in
      print_endline (string_of_int (get_int x));
      push (make_int 0) s (* tmp *)
  | _ -> ErrorMsg.impossible "Invalid operation"

let rec run_instrs () : unit = if !c == 0 then () else (run_instr (); run_instrs ())

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
  | READC -> "READC"
  | WRITEC -> "WRITEC"
