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


#cd "src"
#use "../tools/scripting.ml"

open Printf

let mkicons () =
  if not (Sys.file_exists "icons") then (mkdir "icons");
  let icons = ".." // "icons" in
  let files = Array.to_list (Sys.readdir icons) in
  let files = List.filter (fun x -> Filename.check_suffix x ".png") files in
  let cat = if Sys.os_type = "Win32" then "TYPE" else "cat" in
  ignore (kprintf run "%s %s > %s" cat (".."//"header") ("icons"//"icons.ml"));
  let filename = "icons/icons.ml" in
  let ochan = open_out_gen [Open_append; Open_binary] 0o644 filename in
  try
    fprintf ochan "let (//) = Filename.concat\n\nlet create pixbuf = GMisc.image ~pixbuf ()\n\n";
    List.iter begin fun file ->
      let new_name = Str.global_replace (Str.regexp "-") "_" file in
      Sys.rename (icons // file) (icons // new_name);
      let icon_name = Filename.basename (Filename.chop_extension new_name) in
      fprintf ochan "let %s = GdkPixbuf.from_file (App_config.application_icons // \"%s\")\n" icon_name new_name
    end files;
    close_out_noerr ochan;
  with _ -> close_out_noerr ochan;;

let _ = main ~default_target:mkicons ~options:[] ()

