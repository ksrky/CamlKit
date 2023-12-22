(** Stack cell *)
type cell = Int of int | Cons of int * int

(** Stack model. SECD machine uses one stack for results, arguments, code and
    data but for users it can be seen as separate stacks, so this value is
    hidden from users. *)
let cells : cell array = Array.make Sys.max_array_length (Int 0)

(** Stack registers indicating a top of each stack. [s]: return stack. [e]:
    environment stack. [c]: code stack. [d]: dump stack. *)
let s, e, c, d, f = (ref 0, ref 0, ref 0, ref 0, ref 0)

(** Initialize all registers. *)
let init () =
  s := 0;
  e := 0;
  c := 0;
  d := 0;
  f := 0

(** SECD runtime error *)
exception Runtime_error of string

(** [make_int n] creates a new integer cell and returns its pointer. *)
let make_int (n : int) : int =
  let i = !f in
  cells.(i) <- Int n;
  f := i + 1;
  i

(** [get_int i] returns an integer value of [i]-th cell.
    @raise Runtime_error if referenced cell is not an Int cell.*)
let get_int (i : int) : int =
  match cells.(i) with Int n -> n | _ -> raise (Runtime_error "Int required")

(** [make_cons i j] creates a new cons cell and returns its pointer. *)
let make_cons (i : int) (j : int) : int =
  let n = !f in
  cells.(n) <- Cons (i, j);
  f := n + 1;
  n

(** [get_cons i] returns a pair of integers of [i]-th cell.
    @raise Runtime_error if referenced cell is not a Cons cell. *)
let get_cons (i : int) : int * int =
  match cells.(i) with
  | Cons (n, j) -> (n, j)
  | _ -> raise (Runtime_error "Cons required")

(** [push i r] pushes a pointer [i] to the stack of [r]. *)
let push (i : int) (r : int ref) : unit = r := make_cons i !r

(** [pop r] pops a pointer from the stack of [r].
    @raise Runtime_error if referenced cell is not a Cons cell. *)
let pop (r : int ref) : int =
  match cells.(!r) with
  | Cons (i, j) ->
      r := j;
      i
  | _ -> raise (Runtime_error "Cons required")

(** [atom i] returns 1 if [i]-th cell is an integer cell, 0 otherwise. *)
let atom (i : int) : int = match cells.(i) with Int _ -> 1 | Cons _ -> 0

(** [car i] returns a head element of [i]-th cell.
    @raise Runtime_error if referenced cell is not a Cons cell. *)
let car (i : int) : int =
  match cells.(i) with
  | Cons (i, _) -> i
  | _ -> raise (Runtime_error "Cons required")

(** [cdr i] returns a tail element of [i]-th cell.
    @raise Runtime_error if referenced cell is not a Cons cell. *)
let cdr (i : int) : int =
  match cells.(i) with
  | Cons (_, j) -> j
  | _ -> raise (Runtime_error "Cons required")

(** [locate ij r] returns a [j]-th element of [i]-th element of the stack of [r]
    when [ij]-th cell is [Cons i j].
    @raise if [car] or [cdr] fails *)
let locate (ij : int) (r : int) : int =
  let rec loc (y, z) = if y = 1 then car z else loc (y - 1, cdr z) in
  loc (cdr ij, loc (car ij, r))

(** [binop r op] pops two integers from the stack of [r], applies [op] to them
    and pushes the result to the stack of [r].
    @raise Runtime_error if [pop] fails *)
let binop (r : int ref) (op : int -> int -> int) : unit =
  let a = pop r in
  let b = pop r in
  push (op a b) r

(** [rplaca i x] replaces a head element of [i]-th cell with [x].
    @raise Runtime_error if [cdr] fails. *)
let rplaca (i : int) (x : int) : int =
  cells.(i) <- Cons (x, cdr i);
  i
