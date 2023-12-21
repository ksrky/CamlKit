let error () = ()

exception Bug_error

let bug () = raise Bug_error
