exception Loop_found of string
val find : ?pp:string -> ?includes:string -> ?with_errors:bool -> string list -> string list
val find_dependants : modname:string -> string list
