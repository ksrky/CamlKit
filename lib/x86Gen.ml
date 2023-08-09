module V = Virtual

let ilist : string list ref = ref []

let pushc (i : int) : unit = ilist := ("\tpush\t" ^ string_of_int i ) :: !ilist

let push (r : string) : unit = ilist := ("\tpush\t" ^ r ) :: !ilist

let pushm (r : string) (d : int) : unit =
  ilist := ("\tpush\t[" ^ r ^ " + " ^ string_of_int d ^ "]") :: !ilist

let pop (r : string) : unit = ilist := ("\tpop\t" ^ r ) :: !ilist

let move dst src = ilist := ("\tmov\t" ^ dst ^ ", " ^ src) :: !ilist

let binop (op : string) (r1 : string) (r2 : string) : unit =
  ilist := (op ^ "\t" ^ r1 ^ ", " ^ r2 ) :: !ilist

let jump (j : string) (l : string) : unit = ilist := ("\t" ^ j ^ "\t" ^ l) :: !ilist

let call (l : string) = ilist := ("\tcall\t" ^ l) :: !ilist

let put (l : string) : unit = ilist := l :: !ilist

let rSP, rFP, rARG, rRV = ("esp", "ebp", "ebx", "eax")

let wordSize = 4

let codegen : V.instr -> unit = function
  | V.Push (V.Const, i) -> pushc i
  | V.Push (V.Arg, i) -> pushm rARG (-i * wordSize)
  | V.Push (V.Temp, 0) -> move rRV ("[" ^ rSP ^ " + " ^ string_of_int wordSize ^ "]")
  | V.Label lab -> put lab
  | V.Goto lab -> jump "jmp" lab
  | V.IfGoto (V.Eq, lab) -> jump "je" lab
  | V.Call (lab, _) -> call lab
  | V.Arith V.Add -> put "add"
  | V.Arith V.Sub -> put "sub"
  | V.Arith V.Mul -> put "mul"
  | V.Arith V.Div -> put "div"
  | V.Return -> put "ret"
  | _ -> ()

let procEntryExit : V.frag -> string list = function
  | Proc (f, _, instrs) ->
      ilist := [];
      put f;
      push rFP;
      move rFP rSP;
      List.iter codegen instrs;
      !ilist
