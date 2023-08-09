let () =
  try
    let filename = Sys.argv.(1) in
    CamlKit.Main.compile filename
  with Invalid_argument _ -> print_endline "CamlKit: fatal error: no input file"
