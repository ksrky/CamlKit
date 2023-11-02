open State

type t =
  | Push of int
  | PushInt of int
  | PushGlb of string
  | Pop of int
  | Ap
  | Slide of int
  | Update of int
  | Alloc of int
  | Add
  | Sub
  | Mul
  | Div

type code = t list

let run_instr : t -> unit = function
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
  | Ap -> failwith ""
  | Add ->
      let _n1 = pop () in
      let _n2 = pop () in
      failwith ""
  | _ -> ()
