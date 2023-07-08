type cell = Int of int | Cons of int * int

let cells : cell array = Array.make 100000 (Int 0)

let s, e, c, d, f = (ref 0, ref 0, ref 0, ref 0, ref 0)

let init () =
  s := 0;
  e := 0;
  c := 0;
  d := 0;
  f := 0

let makeInt (n : int) : int =
  let i = !f in
  cells.(i) <- Int n;
  f := i + 1;
  i

let getInt (i : int) : int = match cells.(i) with Int n -> n | _ -> failwith "Int required"

let makeCons (i : int) (j : int) : int =
  let n = !f in
  cells.(n) <- Cons (i, j);
  f := n + 1;
  n

let getCons (i : int) : int * int =
  match cells.(i) with Cons (n, j) -> (n, j) | _ -> failwith "Cons required"

let push (i : int) (r : int ref) : unit = r := makeCons i !r

let pop (r : int ref) : int =
  match cells.(!r) with
  | Cons (i, j) ->
      r := j;
      i
  | _ -> failwith " Cons required"

let car (i : int) : int = match cells.(i) with Cons (i, _) -> i | _ -> failwith "Cons required"

let cdr (i : int) : int = match cells.(i) with Cons (_, j) -> j | _ -> failwith "Cons required"

let locate (ij : int) (r : int) =
  let rec loc (y, z) = if y == 1 then car z else loc (y - 1, cdr z) in
  loc (cdr ij, loc (car ij, r))

let binOp (r : int ref) (op : int -> int -> int) : unit =
  let a = pop r in
  let b = pop r in
  push (makeInt (op a b)) r

let rplaca (x : int) (y : int) : int =
  cells.(x) <- Cons (y, cdr x);
  x

let alloc (i : int) (n : int) : int = makeCons (makeInt n) i