type value = Int of int | Closure of Code.t * env | Empty

and stack = value Stack.t

and env = value list

let code : Code.t ref = ref []

let env : env ref = ref []

let arg_stack : stack = Stack.create ()

let ret_stack : stack = Stack.create ()

let pop_code () : Code.instr =
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

let get_closure : value -> Code.t * env = function
  | Closure (code, env) -> (code, env)
  | _ -> failwith "not a closure"
