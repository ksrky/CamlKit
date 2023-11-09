let () =
  if Array.length Sys.argv = 1 then print_endline "Fatal error: no input files."
  else
    for i = 1 to Array.length Sys.argv - 1 do
      Mono.Main.compile Sys.argv.(i)
    done
