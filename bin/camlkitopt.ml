let () =
  try
    for i = 1 to Array.length Sys.argv - 1 do
      CamlKit.Main.compile Sys.argv.(i)
    done
  with Invalid_argument _ -> print_endline "Error: no input files."
