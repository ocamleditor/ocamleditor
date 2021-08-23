(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)

type command_descr = string
type command_usage = string
type speclist = (Arg.key * Arg.spec * Arg.doc) list

module type COMMAND =
sig
  type t
  val string_of_command : t -> string
  val command_of_string : string -> t
  val options : (t * speclist * command_descr * command_usage) list
  val anon_fun : t -> string -> unit
end

module Make :
  functor (C : COMMAND) ->
  sig
    exception Help_Command of C.t * (speclist * command_descr * command_usage) *
                              string
    val command : C.t option ref

    val commands : C.t list

    val help_of_commands : string

    val parse :
      global_options:speclist ->
      ?default_command:C.t ->
      ?usage_msg:Arg.usage_msg -> (C.t -> unit) -> unit

    val parse_argv :
      string array ->
      global_options:speclist ->
      ?default_command:C.t ->
      ?usage_msg:Arg.usage_msg -> (C.t -> unit) -> unit

  end
