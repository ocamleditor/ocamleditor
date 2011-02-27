exception Loop_found of string
val find : ?pp:string -> ?includes:string -> ?with_errors:bool -> string list -> string list
val find_dependants : targets:string list -> modname:string -> string list
