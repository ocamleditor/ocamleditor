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

let application_pixmaps =
  let share_pixmaps = "share" // "pixmaps" in
  let base = !! (!! Sys.executable_name) in
  let path = if base = "." then ".." // share_pixmaps else base // share_pixmaps in
  let install_path = (!! (!! Sys.executable_name)) // share_pixmaps // "ocamleditor" in
  if Sys.file_exists install_path then install_path
  else path
