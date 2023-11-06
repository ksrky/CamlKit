open State
open Code

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
  | Grab -> (
      pop_arg () |> ignore;
      match pop_arg () with
      | Empty ->
          let c, e = get_closure (pop_ret ()) in
          push_arg (Closure (!code, !env));
          code := c;
          env := e;
          ()
      | v -> extend v )
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
          extend v2 )
  | Let ->
      let v = pop_arg () in
      extend v
  | Endlet -> pop_arg () |> ignore
  | Dummy -> extend Empty
  | _ -> failwith "Not implemented"
