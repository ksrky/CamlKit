open State

type prim = Add | Sub | Mul | Div

type instr =
  | PushInt of int
  | Access of int
  (* Application *)
  | Appterm
  | Apply
  | Pushmark
  (* Abstractions *)
  | Grab
  | Cur of code
  | Return
  (* Local declaratitons *)
  | Let
  | Endlet
  | Dummy
  | Update
  | Prim of prim

and code = instr list

let run_instr () : unit =
  match pop_code () with
  | PushInt i -> push_arg (Int i)
  | Access i -> push_arg (access i)
  | Appterm ->
      let c, e = get_closure (pop_arg ()) in
      env := e;
      extend (pop_arg ());
      code := c
  | Apply ->
      let c, e = get_closure (pop_arg ()) in
      env := e;
      extend (pop_arg ());
      push_ret (Closure (!code, !env));
      code := c
  | Pushmark -> push_arg Empty
  | Cur c -> push_arg (Closure (c, !env))
  | Return -> (
      let v1 = pop_arg () in
      match pop_arg () with
      | Empty ->
          let c, e = get_closure (pop_ret ()) in
          code := c;
          env := e
      | v2 ->
          let c, e = get_closure v1 in
          code := c;
          env := e;
          extend v2;
          failwith "Not implemented" )
  | _ -> failwith "Not implemented"
