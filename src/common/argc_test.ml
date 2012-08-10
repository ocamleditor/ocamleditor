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


open Printf

module MyCommands = struct
  type t = Show | Build | Install

  let string_of_command = function
    | Show -> "show"
    | Build -> "build"
    | Install -> "install"

  let command_of_string = function
    | "show" -> Show
    | "build" -> Build
    | "install" -> Install
    | x -> kprintf invalid_arg "`%s' is not a recognized command." x
end;;

module Argc = Argc.Make (MyCommands);;

open MyCommands;;
open Arg;;

let my_execute_command = function
  | Show -> print_endline "Command Show"
  | Build -> print_endline "Command Build"
  | Install -> print_endline "Command Install";;

try
  Argc.parse
    ~global_options:(Arg.align [
      "-C", String (printf "Set C global option with value %S\n%!"), " Change directory";
    ])
    ~command_options:[
      Show,    (Arg.align ["-a", String print_endline, " Documentation of show -a"]), "Documentation of show";
      Build,   (Arg.align ["-b", String print_endline, " Documentation of build -b"]), "Documentation of build";
      Install, (Arg.align []), "Documentation of instal"
    ]
    ~anon_fun:(fun c -> Printf.printf "%s: ANON=%S\n%!" (string_of_command c))
    (*~usage_msg:"***"*)
    my_execute_command
with
  | Arg.Help msg -> print_endline msg
  | Arg.Bad msg -> prerr_endline msg
  | Invalid_argument msg -> prerr_endline msg
  | ex -> prerr_endline (Printexc.to_string ex);;








