(*

  OCamlEditor
  Copyright (C) 2010-2013 Francesco Tovagliari

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


type t = {
  bs_filename      : string;
  bs_targets       : target list;
  bs_args          : Build_script_args.t list;
  bs_commands      : command list;
}

and target = {
  bst_target           : Target.t;
  bst_show             : bool;
}

and command = {
  bsc_name   : [`Distclean | `Install | `Uninstall | `Install_lib ];
  bsc_descr  : string;
  bsc_target : Target.t;
  bsc_task   : Task.t;
}

let string_of_command = function
  | `Distclean -> "Distclean"
  | `Install -> "Install"
  | `Uninstall -> "Uninstall"
  | `Install_lib -> "Install_lib"

let command_of_string = function
  | "Distclean" -> `Distclean
  | "Install" -> `Install
  | "Uninstall" -> `Uninstall
  | "Install_lib" -> `Install_lib
  | _ -> failwith "string_of_command"

let default_filename = "_build.ml"
