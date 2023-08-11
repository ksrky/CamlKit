let rec repl () =
  print_string "# ";
  let inp = read_line () in
  if inp = "#quit" then ()
  else (
    (try CamlKit.Main.eval inp with CamlKit.ErrorMsg.Error -> ());
    repl () )

let () =
  try
    let filename = Sys.argv.(1) in
    CamlKit.Main.run filename
  with Invalid_argument _ -> repl ()
