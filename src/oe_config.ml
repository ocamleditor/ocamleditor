(*

  OCamlEditor
  Copyright (C) 2010, 2011 Francesco Tovagliari

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

open Miscellanea
open Printf

let ocamleditor_debug = try Sys.getenv "OCAMLEDITORDEBUG" = "1" with Not_found -> false
let is_win32 = Sys.os_type = "Win32"

(** Directories *)
let user_home =
  if Sys.os_type = "Win32" then
    try (Sys.getenv "HOMEDRIVE") // (Sys.getenv "HOMEPATH") with Not_found ->
      failwith "Please set your HOMEDRIVE and HOMEPATH environment variables."
  else
    try Sys.getenv "HOME" with Not_found -> failwith "Please set your HOME environment variable."

let ocamleditor_user_home =
  let ocamleditor_user_home = user_home // (if ocamleditor_debug then ".ocamleditor.test" else ".ocamleditor") in
  if not (Sys.file_exists ocamleditor_user_home) then (Unix.mkdir ocamleditor_user_home 509);
  ocamleditor_user_home


(** Configuration Section =================================================== *)

let pixels_above_lines                 = 0
let pixels_below_lines                 = 0
let save_all_before_compiling          = true
let autosave_enabled                   = true
let autosave_interval                  = 5_000 (* milliseconds *)
let autosave_keep_backup               = 3. *. 24. *. 60. *. 60.  (* 3 days, in milliseconds *)
let current_line_border_enabled        = true
let ocamldoc_paragraph_bgcolor_enabled = true
let fade_window_enabled                = true (* Fade effect for popup windows *)
let dot_leaders_enabled                = true
let indent_lines_solid_color           = `NAME "#e3e3e3"
let indent_lines_dashed_color          = `NAME "#c9c9c9"
let matching_delim_border_color        = `NAME "#ff0000"
let right_margin_line_color            = `NAME "#e0e0e0"
let error_popup_bg_color               = `NAME "#ffeef2"
let error_popup_border_color           = `NAME "#ff6a99"
let error_underline_color              = `NAME "#ff0000"
let warning_popup_bg_color             = `NAME "#fff4e8"
let warning_popup_border_color         = `NAME "#ffc56a"
let warning_underline_color            = warning_popup_border_color
let warning_unused_color               = "#c0c0c0"
let warning_unused_properties          = [`FOREGROUND warning_unused_color; `STYLE `ITALIC]
let warning_tootip_enabled             = false
(* Gutter colors:
  `CALC factor    : Calculated according to the bg color of the text view.
                    [darker] 0.5 <= factor <= 1.0 [same as text view]
  `THEME          : Based on the GTK theme.
  `NAME "#ffffff" : Specific color. *)
let gutter_bg_color                    = `THEME (*`CALC 0.95*)
let gutter_fg_color                    = `THEME (*`CALC 0.65*)
let gutter_border_color                = `THEME (*`CALC 0.88*)
let gutter_marker_color                = `THEME (*`CALC 0.65*)
let code_folding_scope_color           = `NAME "#e5e5e5" (* disabled *)
let code_folding_highlight_color       = "#eeeeee"
let code_folding_hightlight_gradient   = ["#f4f4f4"; "#f9f9f9"; "#fefefe"] (* [] for no gradient *)
(* Font for the "n lines" label in the fold line; it must be 10 pixels height. None for no label *)
let code_folding_font                  = Some "-*-*-medium-r-*-sans-10-*-*-*-*-*-*-*"
let global_gutter_comments_enabled     = false
let global_gutter_comments_color       = `NAME "green"
let global_gutter_no_errors            = `NAME "#daedd0"
let find_replace_history_max_length    = 100
let file_history_filename              = ocamleditor_user_home // "file_history"
let file_history_max_length            = 1000
let project_history_filename           = ocamleditor_user_home // "project_history"
let project_history_max_length         = 30
let location_history_proximity         = 80 * 5 (* characters *)
let location_history_max_length        = 30 (* hint *)
(* Adjustments according to the GTK version *)
let gtk_major, gtk_minor, _            = GMain.Main.version
let current_line_border_adjust, dash_style, dash_style_offset =
  match gtk_major, gtk_minor with
    | 2, 14 -> 2, `ON_OFF_DASH, None
    | 2, 16 -> 2, `DOUBLE_DASH, None
    | 2, 20 -> 1, `ON_OFF_DASH, (Some 2)
    | 2, 22 -> 2, `DOUBLE_DASH, None
    | _     -> 1, `DOUBLE_DASH, None

(** End of Configuration Section ============================================ *)




let title, version = "OCamlEditor", "1.6.2"

let ocaml_codeset = "ISO-8859-1"

let _ = Printexc.record_backtrace true
let _ = Unix.putenv "TERM" ""
let system_ocamllib = try Some (Sys.getenv "OCAMLLIB") with Not_found -> None

let find_best ?(param="--help") prog =
  let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null" in
  try
    List.find begin fun comp ->
      let ok =
        try
          let cmd = sprintf "%s %s%s" (Filename.quote comp) param redirect_stderr in
          if ocamleditor_debug then (printf "Checking for %s... %!" cmd);
          ignore (expand ~first_line:true cmd);
          true
        with _ -> false
      in
      if ocamleditor_debug then (printf "%b\n%!" ok);
      ok
    end prog
  with Not_found ->
    kprintf failwith "Cannot find: %s" (String.concat ", " prog)

(** Directories *)
let ocamleditor_bin = !! Sys.executable_name

let ocamleditor_pixmaps =
  let path = !! (Sys.getcwd()) // "pixmaps" in
  if Sys.file_exists path then path else begin
    let path = (!! (!! Sys.executable_name)) // "pixmaps" in
    if Sys.file_exists path then path
    else ((!! (!! Sys.executable_name)) // "share" // "pixmaps" // "ocamleditor")
  end

(** Commands *)
let oebuild_command =
  let commands = [
    begin
      let basename = "oebuild.opt" ^ (if is_win32 then ".exe" else "") in
      let filename = (Sys.getcwd()) // "oebuild" // basename in
      if Sys.file_exists filename then filename
      else begin
        let path = (!! Sys.executable_name) // basename in
        if Sys.file_exists path then path else basename;
      end
    end;
    begin
      let basename = "oebuild" ^ (if is_win32 then ".exe" else "") in
      let filename = (Sys.getcwd()) // "oebuild" // basename in
      if Sys.file_exists filename then filename
      else begin
        let path = (!! Sys.executable_name) // basename in
        if Sys.file_exists path then path else basename;
      end
    end;
  ] in
  find_best commands

let oeproc_command =
  if is_win32 then
    let commands = [
      begin
        let basename = "oeproc.opt.exe" in
        let filename = (Sys.getcwd()) // "oeproc" // basename in
        if Sys.file_exists filename then filename else ((Sys.getcwd()) // basename)
      end;
      begin
        let basename = "oeproc.exe" in
        let filename = (Sys.getcwd()) // "oeproc" // basename in
        if Sys.file_exists filename then filename else ((Sys.getcwd()) // basename)
      end;
    ] in
    find_best ~param:"" commands
  else "unused"

(*  *)
let _ = Ocaml_config.putenv_ocamllib None








