exception Not_started
type t
val channels : t -> in_channel * out_channel * in_channel
val close : t -> unit
val kill : t -> unit
val start : t -> unit
val getpid : t -> int
val cmd_line : t -> string
val create :
  ?at_exit:(unit -> unit) ->
  ?env:string array -> prog:string -> ?args:string array -> unit -> t
