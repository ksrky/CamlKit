let rec repl () =
  print_string "# ";
  let inp = read_line () in
  if inp = "#quit" then ()
  else (
    (try Mono.Main.eval inp with _ -> ());
    repl () )

let () =
  if Array.length Sys.argv = 1 then repl ()
  else
    for i = 1 to Array.length Sys.argv - 1 do
      Mono.Main.run Sys.argv.(i)
    done
