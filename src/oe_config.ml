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

open Miscellanea
open Printf
open Settings_t

type dcolor = GDraw.color color

(** Configuration     Section =================================================== *)

let dot_viewer : [`DEFAULT | `PDF]       = `DEFAULT
let dot_attributes                       = " -Glabelloc=t -Gfontsize=26pt -Gfontname=\"Helvetica\" -Nfontsize=16pt -Nfontname=\"Helvetica\""
let ocp_indent_tab_key_enabled           = true
let autosave_enabled                     = true
let autosave_interval                    = 5_000 (* milliseconds *)
let autosave_keep_backup                 = 3. *. 24. *. 60. *. 60.  (* 3 days, in milliseconds *)
let ocamldoc_paragraph_border_enabled    = true
let ocamldoc_paragraph_bgcolor_enabled   = true
let fade_window_enabled                  = true (* Fade effect for popup windows *)
let matching_delim_border_color : dcolor = { light = `NAME "#ff0000"; dark = `NAME "#ff0000" }
let error_popup_bg_color : dcolor        = { light = `NAME "#ffeef2"; dark = `NAME "#ffc6d4" }
let error_popup_border_color : dcolor    = { light = `NAME "#ff6a99"; dark = `NAME "#ff6a99" }
let error_underline_color : dcolor       = { light = `NAME "#ff0000"; dark = `NAME "#ff0000" }
let error_underline_shadow : dcolor      = { light = `NAME "#FFa0a0"; dark = `NAME "#FFa0a0" }
let error_underline_mode                 = (`CUSTOM : [`GTK | `CUSTOM])
let warning_popup_bg_color : dcolor      = { light = `NAME "#fff4e8"; dark = `NAME "#fecc8f" }
let warning_popup_border_color : dcolor  = { light = `NAME "#FFB33C"; dark = `NAME "darkorange" }
let warning_underline_color : dcolor     = { light = `NAME "#fff4e8"; dark = `NAME "#6e3d00" }
let warning_underline_shadow : dcolor    = { light = `NAME "#FFE36C"; dark = `NAME "#FFE36C" }
let warning_unused_color                 = { light = "#a0a0a0"; dark = "#a0a0a0" }
let warning_tootip_enabled               = false
let current_line_width                   = 2 (* Must be >= 1. Left margin is automatically increased by current_line_width *)
let current_line_style                   = (*`ON_OFF_DASH*) `SOLID
let current_line_join                    = (*`ROUND `MITER `BEVEL *) `BEVEL
let on_off_dashes                        = [3; 3]
(* Gutter colors:
   `CALC factor    : Calculated according to the bg color of the text view.
                    [darker] 0.5 <= factor <= 1.0 [same as text view]
   `THEME          : Based on the GTK theme.
   `NAME "#ffffff" : Specific color. *)
let gutter_bg_color                      = `NAME "#424201" (* temporarily *) (*`THEME*)
let gutter_fg_color                      = `THEME (*`CALC 0.50*) (*`NAME "#6070ff"*)
let gutter_border_color                  = `THEME (*`CALC 0.875*) (*`CALC 0.97*)
let gutter_marker_color                  = `THEME (*`CALC 0.50*)
let gutter_marker_bg_color               = `THEME (*`CALC 0.80*)
let gutter_diff_size                     = 50 (* 0 = disabled *)
let code_folding_scope_color             = `NAME "#e5e5e5" (* disabled *)
let code_folding_highlight_color         = { light = "#d0d0d0"; dark = "#303030" }
let code_folding_hightlight_gradient     = [ (* [] for no gradient *)
  { light = "#f4f4f4"; dark = "#202020" };
  { light = "#f9f9f9"; dark = "#1b1b1b" };
  { light = "#fefefe"; dark = "#161616" };
]
let global_gutter_comments_enabled       = false
let global_gutter_size                   = 13
let global_gutter_comments_color         = `NAME "#fa80a5"
let global_gutter_comments_bgcolor       = `NAME "#fad0f5"


let global_gutter_diff_color_add         = { light = "#60b060"; dark = "#13401e" }
let global_gutter_diff_color_del         = { light = "#ff6060"; dark = "#3e181d" }
let global_gutter_diff_color_change      = { light = "#0079FF"; dark = "#0079FF" }
let global_gutter_diff_style             = (`COLOR false : [`BW | `COLOR of bool])
let global_gutter_diff_tooltips          = false
let global_gutter_no_errors              = `NAME "#daedd0"
let find_replace_history_max_length      = 75
let find_text_output_highlight           = `DEFAULT, `DEFAULT (*`NAME "#ffff7e", `NONE*) (* Background and foreground colors to highlight occurrences where the pattern matches.
                                                                                            (`NONE=do not change color; `DEFAULT=default color; `NAME=specific color)*)
let find_text_output_linenumber_fgcolor  = { light = "#000000"; dark = "#e1e1e1" }
let file_history_filename                = App_config.ocamleditor_user_home // "file_history"
let file_history_max_length              = 300
let project_history_filename             = App_config.ocamleditor_user_home // "project_history"
let project_history_max_length           = 15
let location_history_proximity           = 20 (* characters *)
let location_history_max_length          = 30 (* hint *)
let location_history_max_edit            = 5
let module_browser_max_results           = 150 (* Max. number of search results to display in the search_entry as you type *)
let module_browser_secondary_title_color = { light = "#877033"; dark = "#707070" }
let completion_popup_default_dimensions  = 900, 350
let odoc_tag_properties                  = [ (* These properties apply to ocamldoc comments only, not to the type descriptions. *)
  `PIXELS_INSIDE_WRAP 2;
  `PIXELS_BELOW_LINES 2;
  `WRAP_MODE `WORD]
let odoc_margin                          = 8
let layout_find_references               = `VERTICAL
let layout_find_module_browser           = `VERTICAL
(* Path relative to the project home directory where to find custom templates. *)
let template_project_filename            = ".extensions" // "templates.cma"
let targetlist_alternating_row_colors    = None
let editor_tab_color_alt_active          = `NAME "#a7a2ae"
let editor_tab_color_alt_normal          = `NAME "#310080"
let use_merlin_completion                = true
let use_merlin_quick_info                = true
let colored_statusbar                    = true
let unify_statusbars                     = false


(** End of Configuration Section ============================================ *)



let ocaml_codeset = "ISO-8859-1"

(*
  THE FOLLOWING LINE IS PROCESSED BY "tools/prepare_build", DO NOT EDIT.
*)
let _ = Printexc.record_backtrace (List.mem_assoc "record_backtrace" App_config.application_param)

let _ = App_config.ensure_ocamleditor_user_home ()
let _ = Unix.putenv "TERM" ""
let getenv_ocamllib = try Some (Sys.getenv "OCAMLLIB") with Not_found -> None

(** Commands *)
let find_command name =
  let basename = name ^ (if Sys.win32 then ".exe" else "") in
  let path = (!! Sys.executable_name) // basename in
  if Sys.file_exists path && not (Sys.is_directory path) then path
  else
    let path = (!! Sys.executable_name) // name // basename in
    if Sys.file_exists path then path
    else basename

let oebuild_command = App_config.get_oebuild_command ()

let get_version ?(ok_status=0) command =
  try
    let redirect_stderr = if Sys.os_type = "Win32" then "1>NUL 2>NUL" else "1>/dev/null 2>/dev/null" in
    let cmd = sprintf "%s %s" command redirect_stderr in
    let status_not_found = if Sys.win32 then [1; 9009] else [127] in
    let status = Sys.command cmd in
    (*Printf.printf "%s -- %d -- %b\n%!" cmd status (status = ok_status);*)
    if status = ok_status || not (List.mem status status_not_found) then
      let redirect_stderr = if Sys.win32 then " 2>&1" else " 2>&1" in
      let cmd = sprintf "%s %s" command redirect_stderr in
      (match Shell.get_command_output cmd with ver :: _ -> Some ver | _ -> None)
    else failwith cmd
  with Failure _ -> None

let dot_version = get_version "dot -V"
let plink_version = get_version "plink -V" (* exits with status = 1 *)
let xdg_open_version = get_version "xdg-open --version"
let git_version = get_version ~ok_status:1 "git --version"
let ml = if Sys.win32 then get_version ~ok_status:0 "ml" else None
let cl = if Sys.win32 then get_version ~ok_status:0 "cl" else None
let rc = if Sys.win32 then get_version ~ok_status:1 "rc" else None
let cvtres = if Sys.win32 then get_version ~ok_status:0 "cvtres" else None


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

(** Clear OCAMLLIB environment variable *)
let _ = Ocaml_config.putenv_ocamllib None

(** geometry_memo_filename *)
let geometry_memo_filename = Filename.concat App_config.ocamleditor_user_home "geometry_memo.ocaml"
let _ =
  let old = Filename.concat App_config.ocamleditor_user_home "message_window_positions" in
  if Sys.file_exists old then Sys.remove old
