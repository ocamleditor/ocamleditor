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


let (//) = Filename.concat
let (!!) = Filename.dirname

let split sep str =
  let str = if str.[0] = sep then String.sub str 1 (String.length str - 1) else str in
  let str = if str.[(String.length str - 1)] = sep then String.sub str 0 (String.length str - 1) else str in
  let parts = ref [] in
  let i = ref (String.length str) in
  let j = ref !i in
  let str = ref str in
  while !i > 0 do
    decr i;
    if !str.[!i] = sep then begin
      parts := (String.sub !str (!i + 1) (!j - !i - 1)) :: !parts;
      str := String.sub !str 0 !i;
      j := !i;
    end else if !i = 0 then begin
      parts := (String.sub !str 0 !j) :: !parts;
    end
  done;
  !parts;;
(*
split ',' "OCAMLEDITORPARAM=debug=2,record_backtrace=1";;
split ',' ",OCAMLEDITORPARAM=debug=2,record_backtrace=1";;
split ',' ",OCAMLEDITORPARAM=debug=2,record_backtrace=1,";;
split ',' ",OCAMLEDITORPARAM=debug=2,,record_backtrace=1,,";;
#load "str.cma";;
Str.split (Str.regexp ",") "OCAMLEDITORPARAM=debug=2,record_backtrace=1";;
Str.split (Str.regexp ",") ",OCAMLEDITORPARAM=debug=2,record_backtrace=1";;
Str.split (Str.regexp ",") ",OCAMLEDITORPARAM=debug=2,record_backtrace=1,";;
Str.split (Str.regexp ",") ",OCAMLEDITORPARAM=debug=2,,record_backtrace=1,,";;
*)

let is_mingw = try ignore (Sys.getenv "OCAMLEDITOR_MINGW"); true with Not_found -> false

let application_param =
  try
    List.fold_left begin fun acc x ->
      match (*Str.split (Str.regexp "=")*) split '=' x with
        | n :: v :: [] -> (n, v) :: acc
        | n :: [] -> (n, "") :: acc
        | _ -> acc
    end [] ((*Str.split (Str.regexp ",") *) split ',' (Sys.getenv "OCAMLEDITORPARAM"))
  with Not_found -> [];;

let application_debug = try (List.assoc "debug" application_param) = "2" with Not_found -> false;;

(** Directories *)
let user_home =
  try Sys.getenv "HOME" with Not_found ->
    (try (Sys.getenv "HOMEDRIVE") // (Sys.getenv "HOMEPATH")
    with Not_found -> failwith "Please set your HOME environment variable.")

let ocamleditor_user_home =
  let dirname =
    if is_mingw then ".ocamleditor.mingw"
    else if application_debug then ".ocamleditor.test"
    else ".ocamleditor"
  in
  let ocamleditor_user_home = user_home // dirname in
  if not (Sys.file_exists ocamleditor_user_home) then (Unix.mkdir ocamleditor_user_home 509);
  ocamleditor_user_home

let launcher_filename = ocamleditor_user_home // "launcher.list"

let get_application_dir name =
  let is_app_in_cwd = !! Sys.executable_name = "." in
  let prefix =
    if is_app_in_cwd then Filename.dirname (Sys.getcwd()) else !! (!! Sys.executable_name)
  in
  let path = prefix // name in
  if Sys.file_exists path && (Sys.is_directory path) then path
  else
    let install_path = prefix // "share" // "ocamleditor" // name in
    install_path

let application_icons = get_application_dir "icons"

let application_plugins = get_application_dir "plugins"


