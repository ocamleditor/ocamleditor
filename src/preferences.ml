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
      { name = "control";
        color = { light = "blue"; dark = "#87CEFA" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "define";
        color = { light = "forestgreen"; dark = "#009000" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "structure";
        color = { light = "purple"; dark = "#C889C8" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "char";
        color = { light = "firebrick3"; dark = "#D2691E" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "infix";
        color = { light = "indianred4"; dark = "#ff6a6a" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "label";
        color = { light = "saddlebrown"; dark = "#B77871" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "uident";
        color = { light = "midnightblue"; dark = "#1B98C3" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "number";
        color = { light = "blue"; dark = "#add8e6" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "custom";
        color = { light = "black"; dark = "white" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 700; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "lident";
        color = { light = "black"; dark = "white" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "symbol";
        color = { light = "black"; dark = "white" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "name_def";
        color = { light = "black"; dark = "white" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "method_name_def";
        color = { light = "black"; dark = "white" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "comment";
        color = { light = "#CD1076"; dark = "#C72B7F" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "ocamldoc";
        color = { light = "deeppink3"; dark = "deeppink1" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "highlight";
        color = { light = "#ffff00"; dark = "#1e1e1e" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "highlight_current_line";
        color = { light = "#c3ff96"; dark = "#223316" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "record_label";
        color = { light = "#474747"; dark = "#d0d0d0" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true };
      { name = "selection";
        color = { light = "#ffffff"; dark = "#000000" };
        bg_color = { light = "#3584e4"; dark = "#4B81AD" };
        weight = 0; style = `NORMAL; underline = `NONE; scale = 1.0; bg_default = false };
      { name = "annotation";
        color = { light = "#444488"; dark = "#A880FF" };
        bg_color = { light = "#ffffff"; dark = "#000000" };
        weight = 0; style = `ITALIC; underline = `NONE; scale = 1.0; bg_default = true };
    ]
  in
  settings.editor_tags <- default_editor_tags;
  settings

let preferences = new GUtil.variable default_values

let geometry_memo = Gmisclib.Window.GeometryMemo.create ~filename:Oe_config.geometry_memo_filename ()

let filename = Filename.concat App_config.ocamleditor_user_home "settings.new.json"

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
        Printf.eprintf "Failed to save settings to file \"%s\".\n%s%!" filename (Printexc.to_string ex);
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

(** reset_defaults *)
let reset_defaults () =
  if Sys.file_exists filename then Sys.remove filename;
  preferences#set default_values;
  save()

let [@ inline] get_themed_color color =
  if preferences#get.Settings_t.theme_is_dark then color.Settings_t.dark else color.Settings_t.light

let (??) = get_themed_color

let set_themed_color color x =
  if preferences#get.Settings_t.theme_is_dark then color.Settings_t.dark <- x else color.Settings_t.light <- x

let new_themed_color x alt =
  if preferences#get.Settings_t.theme_is_dark
  then { Settings_t.light = alt.Settings_t.light; dark = x }
  else { Settings_t.light = x; dark = alt.Settings_t.dark }

let editor_tag_color tagname =
  let color = (List.find (fun t -> t.Settings_t.name = tagname) preferences#get.editor_tags).color in
  let color_name = get_themed_color color in
  (`NAME color_name) |> GDraw.color

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

let _ = begin
  load();
end
