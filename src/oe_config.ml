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

open Miscellanea
open Printf


let is_win32 = Sys.os_type = "Win32"

(** Directories *)
let user_home =
  try Sys.getenv "HOME" with Not_found ->
    (try (Sys.getenv "HOMEDRIVE") // (Sys.getenv "HOMEPATH")
    with Not_found -> failwith "Please set your HOME environment variable.")

let ocamleditor_user_home =
  let dirname =
    if try ignore (Sys.getenv "OCAMLEDITOR_MINGW"); true with Not_found -> false then ".ocamleditor.mingw"
    else if App_config.application_debug then ".ocamleditor.test"
    else ".ocamleditor"
  in
  let ocamleditor_user_home = user_home // dirname in
  if not (Sys.file_exists ocamleditor_user_home) then (Unix.mkdir ocamleditor_user_home 509);
  ocamleditor_user_home

(** Configuration Section =================================================== *)

let dot_viewer : [`DEFAULT | `PDF]       = `DEFAULT
let ocp_indent_tab_key_enabled           = true
let ocp_indent_config                    = "with=2"
let ocp_indent_empty_line : [`INDENT | `ALIGN] = `INDENT
let autosave_enabled                     = true
let autosave_interval                    = 5_000 (* milliseconds *)
let autosave_keep_backup                 = 3. *. 24. *. 60. *. 60.  (* 3 days, in milliseconds *)
let ocamldoc_paragraph_border_enabled    = true
let ocamldoc_paragraph_bgcolor_enabled   = true
let fade_window_enabled                  = true (* Fade effect for popup windows *)
let matching_delim_border_color          = `NAME "#ff0000"
let error_popup_bg_color                 = `NAME "#ffeef2"
let error_popup_border_color             = `NAME "#ff6a99"
let error_underline_color                = `NAME "#ff0000"
let warning_popup_bg_color               = `NAME "#fff4e8"
let warning_popup_border_color           = `NAME "#ffc56a"
let warning_underline_color              = warning_popup_border_color
let warning_unused_color                 = "#a0a0a0"
let warning_unused_properties            = [`FOREGROUND warning_unused_color; `STYLE `ITALIC]
let warning_tootip_enabled               = false
let current_line_border_color            = fun add bgcolor -> `NAME (add bgcolor 0.1)
let current_line_width                   = 2 (* Left margin is automatically increased by current_line_width *)
let current_line_style                   = (*`ON_OFF_DASH*) `SOLID
let current_line_join                    = (*`ROUND `MITER *) `BEVEL
let cursor_aspect_ratio                  = 0.1  (*[0.0-1.0]*)
let on_off_dashes                        = [1; 2]
(* Gutter colors:
  `CALC factor    : Calculated according to the bg color of the text view.
                    [darker] 0.5 <= factor <= 1.0 [same as text view]
  `THEME          : Based on the GTK theme.
  `NAME "#ffffff" : Specific color. *)
let gutter_bg_color                      = (*`THEME*) `CALC 0.93
let gutter_fg_color                      = (*`THEME*) `CALC 0.50
let gutter_border_color                  = (*`THEME*) `CALC 0.875
let gutter_marker_color                  = (*`THEME*) `CALC 0.50
let code_folding_scope_color             = `NAME "#e5e5e5" (* disabled *)
let code_folding_highlight_color         = "#eef5f5"
let code_folding_hightlight_gradient     = ["#f4f4f4"; "#f9f9f9"; "#fefefe"] (* [] for no gradient *)
let code_folding_font                    = ref (Some "-*-*-medium-r-*-sans-10-*-*-*-*-*-*-*")
                                          (* Font for the "n lines" label in the fold line; it must be 10 pixels height. None for no label *)
let global_gutter_comments_enabled       = false
let global_gutter_comments_color         = `NAME "#fa80a5"
let global_gutter_comments_bgcolor       = `NAME "#fad0f5"
let global_gutter_no_errors              = `NAME "#daedd0"
let find_references_title_bgcolor        = "#000000"
let find_references_title_fgcolor        = "#ffffff"
let find_replace_history_max_length      = 75
(* Condensed font for the file list in the search results pane. None is default font. (`STRETCH `CONDENSED doesn't work) *)
let find_text_output_font_condensed      = Some (match Sys.os_type with "Win32" -> "Arial" | _ -> "Helvetica 9") (*None*)
let find_text_output_border_color        = current_line_border_color(*fun _ _ -> `NAME "#707070"*) (* Current line border color of the find text output pane *)
let find_text_output_highlight           = `DEFAULT, `DEFAULT (*`NAME "#ffff7e", `NONE*) (* Background and foreground colors to highlight occurrences where the pattern matches.
                                          (`NONE=do not change color; `DEFAULT=default color; `NAME=specific color)*)
let find_text_output_linenumber_fgcolor  = `FOREGROUND "#000000"
let file_history_filename                = ocamleditor_user_home // "file_history"
let file_history_max_length              = 300
let project_history_filename             = ocamleditor_user_home // "project_history"
let project_history_max_length           = 15
let location_history_proximity           = 20 (* characters *)
let location_history_max_length          = 30 (* hint *)
let location_history_max_edit            = 5
let module_browser_max_results           = 150 (* Max. number of search results to display in the search_entry as you type *)
let module_browser_secondary_title_color = "#877033"
let completion_popup_default_dimensions  = 800, 400
let odoc_tag_properties                  = [ (* These properties apply to ocamldoc comments only, not to the type descriptions. *)
                                          `PIXELS_INSIDE_WRAP 2;
                                          `PIXELS_BELOW_LINES 2;
                                          `WRAP_MODE `WORD]
let odoc_margin                          = 30
let outline_type_color                   = module_browser_secondary_title_color
let outline_selection_bg_color           = "#1F80ED"
let outline_selection_fg_color           = "#FFFFFF"
let outline_active_bg_color              = "#B1C3D8"
let outline_active_fg_color              = "#000000"
let outline_alternating_row_colors       = Some 0.93 (* like the gutter *)
let layout_find_references               = `VERTICAL
let layout_find_module_browser           = `VERTICAL
(* Path relative to the project home directory where to find custom templates. *)
let template_project_filename            = ".extensions" // "templates.cma"
let targetlist_alternating_row_colors    = Some 0.93 (* like the gutter *)

(** End of Configuration Section ============================================ *)



let ocaml_codeset = "ISO-8859-1"

(*
  THE FOLLOWING LINE IS PROCESSED BY "tools/prepare_build", DO NOT EDIT.
*)
let _ = Printexc.record_backtrace (List.mem_assoc "record_backtrace" App_config.application_param)

let _ = Unix.putenv "TERM" ""
let getenv_ocamllib = try Some (Sys.getenv "OCAMLLIB") with Not_found -> None

let _ =
  (* Check whether "code_folding_font" can be loaded. *)
  match !code_folding_font with
    | None -> ()
    | Some fontset ->
      begin
        try ignore (Gdk.Font.load_fontset fontset)
        with Gpointer.Null -> begin
          eprintf "Warning: could not load fontset \"%s\".\n%!" fontset;
          code_folding_font := None
        end
      end;;

let find_best ?(param="--help") prog =
  let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null" in
  try
    List.find begin fun comp ->
      let ok =
        try
          let cmd = sprintf "%s %s%s" (Filename.quote comp) param redirect_stderr in
          if App_config.application_debug then (printf "Checking for %s... %!" cmd);
          ignore (Cmd.expand ~first_line:true cmd);
          true
        with _ -> false
      in
      if App_config.application_debug then (printf "%b\n%!" ok);
      ok
    end prog
  with Not_found ->
    kprintf failwith "Cannot find: %s" (String.concat ", " prog)

(** Commands *)
let find_command name =
  let basename = name ^ (if is_win32 then ".exe" else "") in
  let path = (!! Sys.executable_name) // basename in
  if Sys.file_exists path && not (Sys.is_directory path) then path
  else
    let path = (!! Sys.executable_name) // name // basename in
    if Sys.file_exists path then path
    else basename

let oebuild_command =
  let commands = [
    find_command "oebuild.opt";
    find_command "oebuild";
  ] in
  find_best commands

let oeproc_command =
  if is_win32 then
    let commands = [
      find_command "oeproc.opt";
      find_command "oeproc";
    ] in
    find_best ~param:"" commands
  else "unused"

let get_version command =
  try
    let redirect_stderr = if Sys.os_type = "Win32" then "1>NUL 2>NUL" else "1>/dev/null 2>/dev/null" in
    let cmd = sprintf "%s %s" command redirect_stderr in
    let status_not_found = if Sys.os_type = "Win32" then [1; 9009] else [127] in
    let status = Sys.command cmd in
    (*Printf.printf "%s -- %d\n%!" cmd status;*)
    if status = 0 || not (List.mem status status_not_found) then
      let redirect_stderr = if Sys.os_type = "Win32" then " 2>&1" else " 2>&1" in
      let cmd = sprintf "%s %s" command redirect_stderr in
      (match kprintf Miscellanea.exec_lines "%s %s" cmd redirect_stderr with ver :: _ -> Some ver | _ -> None)
    else failwith cmd
  with Failure _ -> None

let dot_version = get_version "dot -V"
let ocp_indent_version = get_version "ocp-indent --version"
let plink_version = get_version "plink -V" (* exits with status = 1 *)
let xdg_open_version = get_version "xdg-open --version"

(** GTK config *)
(* Adjustments according to the GTK version *)
let gtk_major, gtk_minor, _ = GMain.Main.version
let current_line_border_adjust, dash_style, dash_style_offset =
  match gtk_major, gtk_minor with
    | 2, 14 -> 0, `ON_OFF_DASH, None
    | 2, 16 -> 0, `DOUBLE_DASH, None
    | 2, 20 -> 1, `ON_OFF_DASH, (Some 2)
    | 2, 22 -> 2, `DOUBLE_DASH, None
    | 2, 24 when Sys.os_type = "Win32" -> 1, `DOUBLE_DASH, None
    | 2, 24 -> 1, `ON_OFF_DASH, (Some 2)
    | _     -> 1, `DOUBLE_DASH, None

let themes_dir =
  let themes = (!! (!! Sys.executable_name)) // "share" // "themes" in
  if Sys.os_type = "Win32" && Sys.file_exists themes then Some themes else None;;

(** Clear OCAMLLIB environment variable *)
let _ = Ocaml_config.putenv_ocamllib None

(** geometry_memo_filename *)
let geometry_memo_filename = Filename.concat ocamleditor_user_home "geometry_memo.ocaml"
let _ =
  let old = Filename.concat ocamleditor_user_home "message_window_positions" in
  if Sys.file_exists old then Sys.remove old












