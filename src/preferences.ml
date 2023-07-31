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

let default_values =
  let settings = Settings_j.settings_of_string "{}" in
  let default_editor_tags =
    let open Settings_t in
    [
      { name = "control"; color = "blue"; weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "define"; color = "forestgreen"; weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "structure"; color = "purple"; weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "char"; color = "firebrick3"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "infix"; color = "indianred4"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "label"; color = "saddlebrown"; weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "uident"; color = "midnightblue"; weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "number"; color = "blue"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "custom"; color = "black"; weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "lident"; color = "black"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "symbol"; color = "black"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "name_def"; color = "black"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "method_name_def"; color = "black"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "comment"; color = "#CD1076"; weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "ocamldoc"; color = "deeppink3"; weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "highlight"; color = "#ffff00"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "highlight_current_line"; color = "#c3ff96"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "record_label"; color = "#474747"; weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
      { name = "selection"; color = "#ffffff"; weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = false; bg_color = "#3584e4" };
      { name = "annotation"; color = "#444488"; weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true; bg_color = "#ffffff" };
    ]
  in
  settings.editor_tags <- default_editor_tags;
  settings.editor_tags_dark <- default_editor_tags;
  settings

let preferences = new GUtil.variable default_values

let geometry_memo = Gmisclib.Window.GeometryMemo.create ~filename:Oe_config.geometry_memo_filename ()

let filename = Filename.concat App_config.ocamleditor_user_home "settings.json"

(** save *)
let save () =
  let chan = open_out_bin filename in
  begin
    try
      let json = preferences#get |> Settings_j.string_of_settings |> Yojson.Safe.prettify in
      output_string chan json;
      close_out chan;
      preferences#set { preferences#get with Settings_t.timestamp = Unix.gettimeofday() }
    with ex ->
      begin
        Printf.printf "Failed to save settings to file \"%s\".\n%!" filename;
        close_out_noerr chan
      end;
  end

(** load *)
let load () =
  if Sys.file_exists filename then begin
    let chan = open_in_bin filename in
    begin
      try
        let json = really_input_string chan (in_channel_length  chan) in
        let settings = Settings_j.settings_of_string json in
        preferences#set settings;
        close_in chan
      with ex ->
        begin
          close_in_noerr chan;
          Printf.printf "Failed to load settings from file \"%s\", using defaults.\n%!" filename;
        end;
    end;
  end else begin
    Printf.printf "File \"%s\" not found, using defaults.\n%!" filename;
  end;
  Gmisclib.Window.GeometryMemo.set_enabled geometry_memo  preferences#get.remember_window_geometry;
  Gmisclib.Window.GeometryMemo.set_delayed geometry_memo  preferences#get.geometry_delayed;
  Otherwidgets_config.geometry_memo := (fun () -> geometry_memo);
;;

let editor_tag_color tagname =
  (`NAME (List.find (fun t -> t.Settings_t.name = tagname) preferences#get.editor_tags).color) |> GDraw.color

let editor_tag_label = function
  | "control"                -> "Control"
  | "define"                 -> "Definition"
  | "structure"              -> "Structure"
  | "char"                   -> "String"
  | "infix"                  -> "Infix operator"
  | "label"                  -> "Label"
  | "uident"                 -> "Capitalized identifier"
  | "number"                 -> "Number"
  | "custom"                 -> "Exception occurrence"
  | "lident"                 -> "Lowercase identifier"
  | "symbol"                 -> "Symbol"
  | "name_def"               -> "Name definition"
  | "method_name_def"        -> "Method name definition"
  | "comment"                -> "Comment"
  | "ocamldoc"               -> "ocamldoc"
  | "highlight"              -> "Delimiter match highlight"
  | "highlight_current_line" -> "Line highlight"
  | "record_label"           -> "Record label"
  | "selection"              -> "Selection"
  | "annotation"             -> "Annotation"
  | x -> x

(** reset_defaults *)
let reset_defaults () =
  if Sys.file_exists filename then Sys.remove filename;
  preferences#set default_values;
  save()

let _ = begin
  load();
end
