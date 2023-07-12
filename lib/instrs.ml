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
      let j = alloc i x in
      alloc j 1
  | LD (x, y) ->
      let j = makeCons (makeInt x) (makeInt y) in
      alloc j 2
  | ATOM -> alloc i 3
  | CAR -> alloc i 4
  | CDR -> alloc i 5
  | CONS -> alloc i 6
  | ADD -> alloc i 7
  | SUB -> alloc i 8
  | MUL -> alloc i 9
  | DIV -> alloc i 10
  | SEL (ct, cf) ->
      let j = load_instrs i ct in
      let k = load_instrs j cf in
      alloc k 11
  | JOIN -> alloc i 12
  | LDF f ->
      let j = load_instrs i f in
      alloc j 13
  | RTN -> alloc i 14
  | AP -> alloc i 15
  | DUM -> alloc i 16
  | RAP -> alloc i 17
  | STOP -> alloc i 18
  | READC -> alloc i 19
  | WRITEC -> alloc i 20

and load_instrs i = List.fold_left load_instr i

let load_instrs instrs : unit = c := load_instrs 0 (List.rev instrs)

let run_instr () : unit =
  match getInt (pop c) with
  | 0 (* NIL *) -> push (makeInt 0) s
  | 1 (* LDC *) ->
      let x = pop c in
      push x s
  | 2 (* LD *) ->
      let ij = pop c in
      push (locate ij !e) s
  | 3 (* ATOM *) -> failwith "Invalid operation"
  | 4 (* CAR *) ->
      let a = pop s in
      push (car a) s
  | 5 (* CDR *) ->
      let a = pop s in
      push (cdr a) s
  | 6 (* CONS *) -> binOp s makeCons
  | 7 (* ADD *) -> binOp s (fun a b -> getInt a + getInt b)
  | 8 (* SUB *) -> binOp s (fun a b -> getInt a - getInt b)
  | 9 (* MUL *) -> binOp s (fun a b -> getInt a * getInt b)
  | 10 (* DIV *) -> binOp s (fun a b -> getInt a / getInt b)
  | 11 (* SEL *) ->
      let x = match getInt (pop s) with 0 -> false | _ -> true in
      let ct = pop c in
      let cf = pop c in
      push !c d;
      c := if x then ct else cf
  | 12 (* JOIN *) -> c := pop d
  | 13 (* LDF *) ->
      let f = pop c in
      push (makeCons f !e) s
  | 14 (* RTN *) ->
      let x = pop s in
      s := pop d;
      push x s;
      e := pop d;
      c := pop d
  | 15 (* AP *) ->
      let f, e' = getCons (pop s) in
      let v = pop s in
      push !c d;
      c := f;
      push !e d;
      e := e';
      push v e;
      push !s d;
      s := 0
  | 16 (* DUM *) -> push 0 e
  | 17 (* RAP *) ->
      let closure = pop s in
      let f = car closure in
      let ne = car closure in
      let v = pop s in
      push !c d;
      c := f;
      push (cdr !e) d;
      e := rplaca ne v;
      push v e;
      push !s d;
      s := 0
  | 18 (* STOP *) -> c := 0
  | 19 (* READC *) ->
      let x = read_int () in
      push (makeInt x) s
  | 20 (* WRITEC *) ->
      let x = pop s in
      print_endline (string_of_int (getInt x))
  | _ -> failwith "Invalid operation"

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
  | SEL (ct, cf) -> "SEL " ^ show_instrs ct ^ " " ^ show_instrs cf
  | JOIN -> "JOIN"
  | LDF f -> "LDF " ^ show_instrs f
  | RTN -> "RTN"
  | AP -> "AP"
  | DUM -> "DUM"
  | RAP -> "RAP"
  | STOP -> "STOP"
  | READC -> "READC"
  | WRITEC -> "WRITEC"
