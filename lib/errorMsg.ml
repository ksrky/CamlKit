exception Error

(* let error (pos : int) (msg : string) =
   let rec look = function
     | a :: rest, n ->
         if a < pos then List.iter print_string [":"; string_of_int n; "."; string_of_int (pos - a)]
         else look (rest, n - 1)
     | _ -> print_string "0.0"
   in
   anyErrors := true;
   print_string !fileName;
   look (!linePos, !lineNum);
   List.iter print_string [": "; msg; "\n"]*)

let error (msg : string) = print_endline msg

let impossible msg =
  List.iter print_string [": Error: Compiler bug: "; msg; "\n"];
  flush stdout;
  raise Error