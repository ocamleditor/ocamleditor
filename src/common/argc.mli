module type COMMAND =
  sig
    type t
    val string_of_command : t -> string
    val command_of_string : string -> t
  end

module Make :
  functor (C : COMMAND) ->
    sig
      type command_descr = string
      type speclist = (Arg.key * Arg.spec * Arg.doc) list

      val command : C.t option ref

      val parse_argv :
        string array ->
        global_options:speclist ->
        command_options:(C.t * speclist * command_descr) list ->
        anon_fun:(C.t -> string -> unit) ->
        ?usage_msg:Arg.usage_msg -> (C.t -> unit) -> unit

      val parse :
        global_options:speclist ->
        command_options:(C.t * speclist * command_descr) list ->
        anon_fun:(C.t -> string -> unit) ->
        ?usage_msg:Arg.usage_msg -> (C.t -> unit) -> unit
    end
