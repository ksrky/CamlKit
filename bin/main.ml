let rec repl () =
  print_string "# ";
  let inp = read_line () in
  if inp = "#quit" then ()
  else (
    (try CamlKit.Interpreter.eval inp with CamlKit.ErrorMsg.Error -> ());
    repl () )

let () = repl ()
