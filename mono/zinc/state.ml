type value = Int of int | Closure of Operation.code * env | Empty

and stack = value Stack.t

and env = value list

let code : Operation.code ref = ref []

let env : env ref = ref []

let arg_stack : stack = Stack.create ()

let ret_stack : stack = Stack.create ()

let pop_code () : Operation.instr =
  match !code with
  | [] -> failwith "no code"
  | c :: cs ->
      code := cs;
      c

let extend (v : value) : unit = env := v :: !env

let access (n : int) : value = List.nth !env n

let push_arg (v : value) : unit = Stack.push v arg_stack

let pop_arg () : value = Stack.pop arg_stack

let push_ret (v : value) : unit = Stack.push v ret_stack

let pop_ret () : value = Stack.pop ret_stack

let get_int : value -> int = function Int i -> i | _ -> failwith "not an int"

let get_closure : value -> Operation.code * env = function
  | Closure (code, env) -> (code, env)
  | _ -> failwith "not a closure"
