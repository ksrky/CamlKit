let rec repl () =
  print_string "# ";
  let inp = read_line () in
  if inp = "#quit" then ()
  else (
    (try CamlKit.Main.eval inp with CamlKit.ErrorMsg.Error -> ());
    repl () )

let () =
  try
    for i = 1 to Array.length Sys.argv - 1 do
      CamlKit.Main.run Sys.argv.(i)
    done
  with Invalid_argument _ -> repl ()
