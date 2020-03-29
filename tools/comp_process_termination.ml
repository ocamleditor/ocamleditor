(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

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

#load "unix.cma"
#cd "src"
#use "../tools/scripting.ml"
#cd "common"

let is_mingw = List.exists ((=) "system: mingw") (get_command_output "ocamlc -config")

let compile () =
  let filename = if Sys.win32 then "terminate_process" else "terminate_process_unix" in
  sys_command ["ocamlc"; filename ^ ".c" ];
  let ext = if Sys.win32 && not is_mingw then "obj" else "o" in
  sys_command ["ocamlmklib"; (sprintf "%s.%s" filename ext); "process_termination.ml"; "-o process_termination"];
;;

let _ = main ~default_target:compile ~options:[] ()

