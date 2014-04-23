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

let application_param =
  try
    List.fold_left begin fun acc x ->
      match Str.split (Str.regexp "=") x with
        | n :: v :: [] -> (n, v) :: acc
        | n :: [] -> (n, "") :: acc
        | _ -> acc
    end [] (Str.split (Str.regexp ",") (Sys.getenv "OCAMLEDITORPARAM"))
  with Not_found -> [];;

let application_debug = try (List.assoc "debug" application_param) = "2" with Not_found -> false;;

let get_application_dir name =
  let is_app_in_cwd = !! Sys.executable_name = "." in
  let prefix =
    if is_app_in_cwd then ".." else !! (!! Sys.executable_name)
  in
  let path = prefix // name in
  if Sys.file_exists path && (Sys.is_directory path) then path
  else
    let install_path = prefix // "share" // "ocamleditor" // name in
    install_path

let application_icons = get_application_dir "icons"

let application_plugins = get_application_dir "plugins"

(** get_locale *)
let get_locale () =
  try
    if Sys.win32 then begin
      let lines = Cmd.exec_lines "reg query \"hkcu\\Control Panel\\International\" /v LocaleName" in
      let lines = List.map String.trim lines in
      let locale =
        List.find (fun l -> Str.string_match (Str.regexp "LocaleName.+") l 0) lines
      in
      Str.string_match (Str.regexp ".*[\t ]\\([a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-]\\)") locale 0 |> ignore;
      Some (Str.matched_group 1 locale)
    end else begin
      let lines = Cmd.exec_lines "locale" in
      let locale =
        List.find (fun l -> Str.string_match (Str.regexp ".*=.+") l 0) lines
      in
      Str.string_match (Str.regexp ".*=\\(.*\\)") locale 0 |> ignore;
      Some (Str.matched_group 1 locale)
    end
  with ex ->
    Printf.eprintf "File \"miscellanea.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    None

