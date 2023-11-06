open Code
open State

let run_instr : instr -> unit = function
  | Push k -> push k
  | PushInt n -> push_int n
  | Pop k -> decr_sp k
  | Slide k ->
      let n = pop () in
      decr_sp k; push_node n
  | Update k ->
      let n = pop () in
      stack.(!sp - k) <- n
  | Alloc k ->
      for _ = 1 to k do
        push_hole ()
      done
  | MkAp -> failwith ""
  | Add ->
      let n1 = pop_int () in
      let n2 = pop_int () in
      push_int (n1 + n2)
  | Sub ->
      let n1 = pop_int () in
      let n2 = pop_int () in
      push_int (n1 - n2)
  | Mul ->
      let n1 = pop_int () in
      let n2 = pop_int () in
      push_int (n1 * n2)
  | Div ->
      let n1 = pop_int () in
      let n2 = pop_int () in
      push_int (n1 / n2)
  | _ -> ()
