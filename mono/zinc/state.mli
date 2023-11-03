type value = Int of int | Clos of Operation.code * env | Empty

and stack = value list

and env = value list
