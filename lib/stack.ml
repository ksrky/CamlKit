type cell = Int of int | Cons of int * int

let cells : cell array = Array.make 1000000 (Int 0)

let s, e, c, d, f = (ref 0, ref 0, ref 0, ref 0, ref 0)

let init () =
  s := 0;
  e := 0;
  c := 0;
  d := 0;
  f := 0

exception Runtime_error of string

let make_int (n : int) : int =
  let i = !f in
  cells.(i) <- Int n;
  f := i + 1;
  i

let get_int (i : int) : int =
  match cells.(i) with Int n -> n | _ -> raise (Runtime_error "Int required")

let make_cons (i : int) (j : int) : int =
  let n = !f in
  cells.(n) <- Cons (i, j);
  f := n + 1;
  n

let get_cons (i : int) : int * int =
  match cells.(i) with Cons (n, j) -> (n, j) | _ -> raise (Runtime_error "Cons required")

let push (i : int) (r : int ref) : unit = r := make_cons i !r

let pop (r : int ref) : int =
  match cells.(!r) with
  | Cons (i, j) ->
      r := j;
      i
  | _ -> raise (Runtime_error "Cons required")

let atom (i : int) : int = match cells.(i) with Int _ -> 1 | Cons _ -> 0

let car (i : int) : int =
  match cells.(i) with Cons (i, _) -> i | _ -> raise (Runtime_error "Cons required")

let cdr (i : int) : int =
  match cells.(i) with Cons (_, j) -> j | _ -> raise (Runtime_error "Cons required")

let locate (ij : int) (r : int) =
  let rec loc (y, z) = if y == 1 then car z else loc (y - 1, cdr z) in
  loc (cdr ij, loc (car ij, r))

let binop (r : int ref) (op : int -> int -> int) : unit =
  let a = pop r in
  let b = pop r in
  push (op a b) r

let rplaca (x : int) (y : int) : int =
  cells.(x) <- Cons (y, cdr x);
  x

let alloc (i : int) (n : int) : int = make_cons (make_int n) i
