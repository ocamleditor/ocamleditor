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

type t = {
  mutable pref_general_theme                : string option;
  mutable pref_general_font                 : string;
  mutable pref_general_menubar_buttons      : int list;
  mutable pref_general_splashscreen_enabled : bool;
  mutable pref_timestamp                    : float;
  mutable pref_base_font                    : string;
  mutable pref_tab_pos                      : Gtk.Tags.position;
  mutable pref_check_updates                : bool;
  mutable pref_tab_label_type               : int;
  mutable pref_tab_vertical_text            : bool;
  mutable pref_bg_color                     : string * bool;
  mutable pref_bg_color_popup               : string;
  mutable pref_fg_color_popup               : string;
  mutable pref_tags                         : (string * text_properties) list;
  mutable pref_editor_tab_width             : int;
  mutable pref_editor_tab_spaces            : bool;
  mutable pref_editor_bak                   : bool;
  mutable pref_editor_wrap                  : bool;
  mutable pref_editor_trim_lines            : bool;
  mutable pref_editor_format_on_save        : bool;
  mutable pref_editor_custom_templ_filename : string;
  mutable pref_editor_mark_occurrences      : bool * string;
  mutable pref_editor_left_margin           : int;
  mutable pref_editor_pixels_lines          : int * int;
  mutable pref_editor_save_all_bef_comp     : bool;
  mutable pref_editor_dot_leaders           : bool;
  mutable pref_editor_current_line_border   : bool;
  mutable pref_editor_indent_config         : string;
  mutable pref_editor_indent_empty_line     : bool;
  mutable pref_editor_cursor_aspect_ratio   : float;
  mutable pref_compl_font                   : string;
  mutable pref_compl_greek                  : bool;
  mutable pref_compl_decorated              : bool;
  mutable pref_compl_opacity                : float option;
  mutable pref_output_font                  : string;
  mutable pref_output_bg                    : string;
  mutable pref_output_fg_stdin              : string;
  mutable pref_output_fg_stdout             : string;
  mutable pref_output_fg_err                : string;
  mutable pref_output_fg_warn               : string;
  mutable pref_smart_keys_home              : int;
  mutable pref_smart_keys_end               : int;
  mutable pref_annot_type_tooltips_enabled  : bool;
  mutable pref_annot_type_tooltips_delay    : int;
  mutable pref_annot_type_tooltips_impl     : int;
  mutable pref_search_word_at_cursor        : bool;
  mutable pref_highlight_current_line       : bool;
  mutable pref_show_line_numbers            : bool;
  mutable pref_editor_indent_lines          : bool;
  mutable pref_editor_indent_lines_color_s  : string;
  mutable pref_editor_indent_lines_color_d  : string;
  mutable pref_right_margin_visible         : bool;
  mutable pref_right_margin                 : int;
  mutable pref_right_margin_color           : string;
  mutable pref_max_view_1_menubar           : bool;
  mutable pref_max_view_1_toolbar           : bool;
  mutable pref_max_view_1_tabbar            : bool;
  mutable pref_max_view_1_messages          : bool;
  mutable pref_max_view_1_fullscreen        : bool;
  mutable pref_max_view_2                   : bool;
  mutable pref_max_view_2_menubar           : bool;
  mutable pref_max_view_2_toolbar           : bool;
  mutable pref_max_view_2_tabbar            : bool;
  mutable pref_max_view_2_messages          : bool;
  mutable pref_max_view_2_fullscreen        : bool;
  mutable pref_max_view_fullscreen          : bool;
  mutable pref_ocamldoc_paragraph_bgcolor_1 : string option;
  mutable pref_ocamldoc_paragraph_bgcolor_2 : string option;
  mutable pref_code_folding_enabled         : bool;
  mutable pref_show_global_gutter           : bool;
  mutable pref_err_underline                : bool;
  mutable pref_err_tooltip                  : bool;
  mutable pref_err_gutter                   : bool;
  mutable pref_show_whitespace_chars        : bool;
  mutable pref_outline_show_types           : bool;
  mutable pref_outline_width                : int;
  mutable pref_outline_color_types          : string;
  mutable pref_outline_color_nor_bg         : string;
  mutable pref_outline_color_nor_fg         : string;
  mutable pref_outline_color_sel_bg         : string;
  mutable pref_outline_color_sel_fg         : string;
  mutable pref_outline_color_act_bg         : string;
  mutable pref_outline_color_act_fg         : string;
  mutable pref_outline_color_alt_rows       : float option;
  mutable pref_hmessages_width              : int;
  mutable pref_vmessages_height             : int;
  mutable pref_odoc_font                    : string;
  mutable pref_program_pdf_viewer           : string;
  mutable pref_program_diff                 : string;
  mutable pref_program_diff_graphical       : string;
  mutable pref_remember_window_geometry     : bool;
  mutable pref_detach_message_panes_separately : bool;
  mutable pref_geometry_delayed             : bool;
  mutable pref_build_parallel               : int option;
  mutable pref_build_verbosity              : int;
}
and text_properties =
  GDraw.color *
  Pango.Tags.weight *
  Pango.Tags.style *
  Pango.Tags.underline *
  Pango.Tags.scale *
  (bool * GDraw.color)

let pref_filename = Filename.concat App_config.ocamleditor_user_home "preferences.xml"

let default_tags = [
    "control";
    "define";
(*    "define2";*)
    "structure";
    "char";
    "infix";
    "label";
    "uident";
    "number";
    "custom";
    "lident";
    "symbol";
    "name_def";
    "method_name_def";
    "comment";
    "ocamldoc";
    "highlight";
    "highlight_current_line";
    "record_label";
    "selection"
  ]

let tag_labels = List.combine default_tags [
    "Control";
    "Definition";
(*    "define2";*)
    "Structure";
    "String";
    "Infix operator";
    "Label";
    "Capitalized identifier";
    "Number";
    "Exception occurrence";
    "Lowercase identifier";
    "Symbol";
    "Name definition";
    "Method name definition";
    "Comment";
    "ocamldoc";
    "Delimiter match highlight";
    "Line highlight";
    "Record label";
    "Selection";
  ]

let default_colors : text_properties list = [
  (`NAME "blue"),         `BOLD,   `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "forestgreen"),  `BOLD,   `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
(*    (`NAME "forestgreen"), `BOLD, `NORMAL, `NONE, `MEDIUM;*)
  (`NAME "purple"),       `BOLD,   `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "firebrick3"),   `NORMAL, `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "indianred4"),   `NORMAL, `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "saddlebrown"),  `BOLD,   `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "midnightblue"), `BOLD,   `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "blue"),         `NORMAL, `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "black"),        `BOLD,   `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "black"),        `NORMAL, `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "black"),        `NORMAL, `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "black"),        `NORMAL, `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "black"),        `NORMAL, `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "deeppink3"),    `NORMAL, `ITALIC, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "deeppink3"),    `NORMAL, `ITALIC, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "#FFFF00"),      `NORMAL, `NORMAL, `LOW,  `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "#C3FF96"),      `NORMAL, `NORMAL, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");(* #E8F2FF *) (* #F7F7D7 *) (*"#F9F9CA"*) (* #EBF9FF *)
  (`NAME "#474747"),      `NORMAL, `ITALIC, `NONE, `MEDIUM, (true,  `NAME "#FFFFFF");
  (`NAME "#FFFFFF"),      `NORMAL, `NORMAL, `NONE, `MEDIUM, (false, `NAME "#128C4C"); (* #1F80ED *)
]

let create_defaults () = {
  pref_general_theme                = (match Oe_config.themes_dir with Some _ -> Some "MurrinaCandido" | _ -> None);
  pref_general_font                 = "";
  pref_general_menubar_buttons      = [];
  pref_general_splashscreen_enabled = true;
  pref_timestamp                    = (Unix.gettimeofday());
  pref_base_font                    = "monospace 9";
  pref_tab_pos                      = `TOP;
  pref_check_updates                = false;
  pref_tab_vertical_text            = false;
  pref_tab_label_type               = 1;
  pref_bg_color                     = ("#ffffff", false);
  pref_bg_color_popup               = "#FFE375";  (* #EDE1B4 #f1edbd F0F4FF #F7F7F7 #E8ECFF #EAEAFF #F0F4FF *)
  pref_fg_color_popup               = "#000000";
  pref_tags                         = List.combine default_tags default_colors;
  pref_editor_tab_width             = 2;
  pref_editor_tab_spaces            = true;
  pref_editor_bak                   = true;
  pref_editor_wrap                  = false;
  pref_editor_trim_lines            = false;
  pref_editor_format_on_save        = false;
  pref_editor_custom_templ_filename = "";
  pref_editor_mark_occurrences      = true, "#00FF00";
  pref_editor_left_margin           = 1;
  pref_editor_pixels_lines          = 0,2;
  pref_editor_save_all_bef_comp     = true;
  pref_editor_dot_leaders           = false;
  pref_editor_current_line_border   = true;
  pref_editor_indent_config         = "";
  pref_editor_indent_empty_line     = true;
  pref_editor_cursor_aspect_ratio   = 0.1;
  pref_compl_font                   = "Sans 8";
  pref_compl_greek                  = true;
  pref_compl_decorated              = true;
  pref_compl_opacity                = None;
  pref_output_font                  = "monospace 8";
  pref_output_bg                    = "#FFFFFF";
  pref_output_fg_stdin              = "#0000FF";
  pref_output_fg_stdout             = "#000000";
  pref_output_fg_err                = "#FF0000";
  pref_output_fg_warn               = "darkorange";
  pref_smart_keys_home              = 0;
  pref_smart_keys_end               = 0;
  pref_annot_type_tooltips_enabled  = false;
  pref_annot_type_tooltips_delay    = 0;
  pref_annot_type_tooltips_impl     = 0;
  pref_search_word_at_cursor        = true;
  pref_highlight_current_line       = true;
  pref_show_line_numbers            = true;
  pref_editor_indent_lines          = true;
  pref_editor_indent_lines_color_s  = "#e6e6e6";
  pref_editor_indent_lines_color_d  = "#d0d0d0";
  pref_right_margin_visible         = true;
  pref_right_margin                 = 80;
  pref_right_margin_color           = "#e0e0e0";
  pref_max_view_1_menubar           = true;
  pref_max_view_1_toolbar           = false;
  pref_max_view_1_tabbar            = false;
  pref_max_view_1_messages          = false;
  pref_max_view_1_fullscreen        = false;
  pref_max_view_2                   = true;
  pref_max_view_2_menubar           = true;
  pref_max_view_2_toolbar           = true;
  pref_max_view_2_tabbar            = true;
  pref_max_view_2_messages          = true;
  pref_max_view_2_fullscreen        = true;
  pref_max_view_fullscreen          = true;
  pref_ocamldoc_paragraph_bgcolor_1 = Some "#FAF7FA" (*"#F5F0FF"*);
  pref_ocamldoc_paragraph_bgcolor_2 = Some "#FAF7FA" (*"#F8F5FF"*);
  pref_code_folding_enabled         = true;
  pref_show_global_gutter           = true;
  pref_err_underline                = true;
  pref_err_tooltip                  = true;
  pref_err_gutter                   = true;
  pref_show_whitespace_chars        = false;
  pref_outline_show_types           = true;
  pref_outline_width                = 250;
  pref_outline_color_types          = Oe_config.module_browser_secondary_title_color;
  pref_outline_color_nor_bg         = "#ffffff";
  pref_outline_color_nor_fg         = "#000000";
  pref_outline_color_sel_bg         = "#1F80ED";
  pref_outline_color_sel_fg         = "#FFFFFF";
  pref_outline_color_act_bg         = "#B1C3D8";
  pref_outline_color_act_fg         = "#000000";
  pref_outline_color_alt_rows       = None; (*Some 0.95;*) (* like the gutter *)
  pref_hmessages_width              = 1000;
  pref_vmessages_height             = 300;
  pref_odoc_font                    = "Serif 9";
  pref_program_pdf_viewer                   =
    if Sys.os_type = "Win32" then ""
    else (match Oe_config.xdg_open_version with None -> "evince" | _ -> "xdg-open");
  pref_program_diff                 = "diff";
  pref_program_diff_graphical       =
    if Sys.os_type = "Win32" then "%ProgramFiles%\\Diffuse\\diffuse.exe"
    else "kompare";
  pref_remember_window_geometry     = true;
  pref_detach_message_panes_separately = false;
  pref_geometry_delayed             = false;
  pref_build_parallel               = Some 0;
  pref_build_verbosity              = 2;
}

let preferences = new GUtil.variable (create_defaults ())

let string_of_pos = function
  | `TOP -> "TOP"
  | `BOTTOM -> "BOTTOM"
  | `LEFT -> "LEFT"
  | `RIGHT -> "RIGHT"

let string_of_weight = function
  | `NORMAL -> "NORMAL"
  | `BOLD -> "BOLD"
  | _ -> assert false

let string_of_style = function
  | `NORMAL -> "NORMAL"
  | `ITALIC -> "ITALIC"
  | _ -> assert false

let string_of_underline = function
  | `NONE -> "NONE"
  | `LOW -> "LOW"
  | `DOUBLE -> "DOUBLE"
  | `SINGLE -> "SINGLE"

let string_of_color = function
  | `NAME name -> name
  | _ -> assert false

let pos_of_string = function
  | "TOP" -> `TOP
  | "BOTTOM" -> `BOTTOM
  | "LEFT" -> `LEFT
  | "RIGHT" -> `RIGHT
  | _ -> assert false

let weight_of_string = function
  | "NORMAL" -> `NORMAL
  | "BOLD" -> `BOLD
  | _ -> assert false

let style_of_string = function
  | "NORMAL" -> `NORMAL
  | "ITALIC" -> `ITALIC
  | _ -> assert false

let underline_of_string = function
  | "NONE" -> `NONE
  | "LOW" -> `LOW
  | "DOUBLE" -> `DOUBLE
  | "SINGLE" -> `SINGLE
  | _ -> assert false

let scale_of_string = function
  | "SMALL" -> `SMALL
  | "MEDIUM" -> `MEDIUM
  | "LARGE" -> `LARGE
  | _ -> assert false

let color_of_string name = `NAME name

let to_xml pref =
  let xml =
    Xml.Element ("preferences", [], [
      Xml.Element ("pref_general_theme", [], [Xml.PCData (Option.value pref.pref_general_theme ~default:"")]);
      Xml.Element ("pref_general_font", [], [Xml.PCData pref.pref_general_font]);
      Xml.Element ("pref_general_menubar_buttons", [],
                   [Xml.PCData (String.concat "," (List.map string_of_int pref.pref_general_menubar_buttons))]);
      Xml.Element ("pref_general_splashscreen_enabled", [], [Xml.PCData (string_of_bool pref.pref_general_splashscreen_enabled)]);
      Xml.Element ("pref_base_font", [], [Xml.PCData pref.pref_base_font]);
      Xml.Element ("pref_check_updates", [], [Xml.PCData (string_of_bool pref.pref_check_updates)]);
      Xml.Element ("pref_tab_pos", [], [Xml.PCData (string_of_pos pref.pref_tab_pos)]);
      Xml.Element ("pref_tab_vertical_text", [], [Xml.PCData (string_of_bool pref.pref_tab_vertical_text)]);
      Xml.Element ("pref_tab_label_type", [], [Xml.PCData (string_of_int pref.pref_tab_label_type)]);
      Xml.Element ("pref_bg_color", [], [Xml.PCData (fst pref.pref_bg_color)]);
      Xml.Element ("pref_bg_color_theme", [], [Xml.PCData (string_of_bool (snd pref.pref_bg_color))]);
      Xml.Element ("pref_bg_color_popup", [], [Xml.PCData pref.pref_bg_color_popup]);
      Xml.Element ("pref_fg_color_popup", [], [Xml.PCData pref.pref_fg_color_popup]);
      Xml.Element ("pref_tags", [],
        List.map begin fun (id, (color, weight, style, uline, scale, (bg_default, bg_color))) ->
          Xml.Element ("tag", [
            ("name", id);
            ("color", string_of_color color);
            ("weight", string_of_weight weight);
            ("style", string_of_style style);
            ("underline", string_of_underline uline);
            ("scale", "MEDIUM");
            ("bg_default", string_of_bool bg_default);
            ("bg_color", string_of_color bg_color);
          ], []);
        end pref.pref_tags
      );
      Xml.Element ("pref_editor_tab_width", [], [Xml.PCData (string_of_int pref.pref_editor_tab_width)]);
      Xml.Element ("pref_editor_tab_spaces", [], [Xml.PCData (string_of_bool pref.pref_editor_tab_spaces)]);
      Xml.Element ("pref_editor_wrap", [], [Xml.PCData (string_of_bool pref.pref_editor_wrap)]);
      Xml.Element ("pref_editor_trim_lines", [], [Xml.PCData (string_of_bool pref.pref_editor_trim_lines)]);
      Xml.Element ("pref_editor_format_on_save", [], [Xml.PCData (string_of_bool pref.pref_editor_format_on_save)]);
      Xml.Element ("pref_editor_bak", [], [Xml.PCData (string_of_bool pref.pref_editor_bak)]);
      begin
        let enabled, color = pref.pref_editor_mark_occurrences in
        Xml.Element ("pref_editor_mark_occurrences", ["enabled", (string_of_bool enabled)], [Xml.PCData color]);
      end;
      Xml.Element ("pref_editor_custom_templ_filename", [], [Xml.PCData (pref.pref_editor_custom_templ_filename)]);
      Xml.Element ("pref_editor_left_margin", [], [Xml.PCData (string_of_int pref.pref_editor_left_margin)]);
      begin
        let above, below = pref.pref_editor_pixels_lines in
        Xml.Element ("pref_editor_pixels_lines", ["above", string_of_int above; "below", string_of_int below], []);
      end;
      Xml.Element ("pref_editor_save_all_bef_comp", [], [Xml.PCData (string_of_bool pref.pref_editor_save_all_bef_comp)]);
      Xml.Element ("pref_editor_dot_leaders", [], [Xml.PCData (string_of_bool pref.pref_editor_dot_leaders)]);
      Xml.Element ("pref_editor_current_line_border", [], [Xml.PCData (string_of_bool pref.pref_editor_current_line_border)]);
      Xml.Element ("pref_editor_indent_config", [], [Xml.PCData pref.pref_editor_indent_config]);
      Xml.Element ("pref_editor_indent_empty_line", [], [Xml.PCData (string_of_bool pref.pref_editor_indent_empty_line)]);
      Xml.Element ("pref_editor_cursor_aspect_ratio", [], [Xml.PCData (string_of_float pref.pref_editor_cursor_aspect_ratio)]);
      Xml.Element ("pref_compl_font", [], [Xml.PCData pref.pref_compl_font]);
      Xml.Element ("pref_compl_greek", [], [Xml.PCData (string_of_bool pref.pref_compl_greek)]);
      Xml.Element ("pref_compl_decorated", [], [Xml.PCData (string_of_bool pref.pref_compl_decorated)]);
      Xml.Element ("pref_compl_opacity", [], [Xml.PCData (match pref.pref_compl_opacity with Some x -> string_of_float x | _ -> "")]);
      Xml.Element ("pref_output_font", [], [Xml.PCData pref.pref_output_font]);
      Xml.Element ("pref_output_bg", [], [Xml.PCData pref.pref_output_bg]);
      Xml.Element ("pref_output_fg_stdin", [], [Xml.PCData pref.pref_output_fg_stdin]);
      Xml.Element ("pref_output_fg_stdout", [], [Xml.PCData pref.pref_output_fg_stdout]);
      Xml.Element ("pref_output_fg_err", [], [Xml.PCData pref.pref_output_fg_err]);
      Xml.Element ("pref_output_fg_warn", [], [Xml.PCData pref.pref_output_fg_warn]);
      Xml.Element ("pref_smart_keys_home", [], [Xml.PCData (string_of_int pref.pref_smart_keys_home)]);
      Xml.Element ("pref_smart_keys_end", [], [Xml.PCData (string_of_int pref.pref_smart_keys_end)]);
      Xml.Element ("pref_annot_type_tooltips_enabled", [], [Xml.PCData (string_of_bool pref.pref_annot_type_tooltips_enabled)]);
      Xml.Element ("pref_annot_type_tooltips_delay", [], [Xml.PCData (string_of_int pref.pref_annot_type_tooltips_delay)]);
      Xml.Element ("pref_annot_type_tooltips_impl", [], [Xml.PCData (string_of_int pref.pref_annot_type_tooltips_impl)]);
      Xml.Element ("pref_search_word_at_cursor", [], [Xml.PCData (string_of_bool pref. pref_search_word_at_cursor)]);
      Xml.Element ("pref_highlight_current_line", [], [Xml.PCData (string_of_bool pref.pref_highlight_current_line)]);
      Xml.Element ("pref_show_line_numbers", [], [Xml.PCData (string_of_bool pref.pref_show_line_numbers)]);
      Xml.Element ("pref_editor_indent_lines", [
        "solid_lines_color", pref.pref_editor_indent_lines_color_s;
        "dashed_lines_color", pref.pref_editor_indent_lines_color_d;
      ], [Xml.PCData (string_of_bool pref.pref_editor_indent_lines)]);
      Xml.Element ("pref_right_margin_visible", [], [Xml.PCData (string_of_bool pref.pref_right_margin_visible)]);
      Xml.Element ("pref_right_margin", [], [Xml.PCData (string_of_int pref.pref_right_margin)]);
      Xml.Element ("pref_right_margin_color", [], [Xml.PCData pref.pref_right_margin_color]);
      Xml.Element ("pref_max_view_1_menubar", [], [Xml.PCData (string_of_bool pref.pref_max_view_1_menubar)]);
      Xml.Element ("pref_max_view_1_toolbar", [], [Xml.PCData (string_of_bool pref.pref_max_view_1_toolbar)]);
      Xml.Element ("pref_max_view_1_tabbar", [], [Xml.PCData (string_of_bool pref.pref_max_view_1_tabbar)]);
      Xml.Element ("pref_max_view_1_messages", [], [Xml.PCData (string_of_bool pref.pref_max_view_1_messages)]);
      Xml.Element ("pref_max_view_1_fullscreen", [], [Xml.PCData (string_of_bool pref.pref_max_view_1_fullscreen)]);
      Xml.Element ("pref_max_view_2", [], [Xml.PCData (string_of_bool pref.pref_max_view_2)]);
      Xml.Element ("pref_max_view_2_menubar", [], [Xml.PCData (string_of_bool pref.pref_max_view_2_menubar)]);
      Xml.Element ("pref_max_view_2_toolbar", [], [Xml.PCData (string_of_bool pref.pref_max_view_2_toolbar)]);
      Xml.Element ("pref_max_view_2_tabbar", [], [Xml.PCData (string_of_bool pref.pref_max_view_2_tabbar)]);
      Xml.Element ("pref_max_view_2_messages", [], [Xml.PCData (string_of_bool pref.pref_max_view_2_messages)]);
      Xml.Element ("pref_max_view_2_fullscreen", [], [Xml.PCData (string_of_bool pref.pref_max_view_2_fullscreen)]);
      Xml.Element ("pref_max_view_fullscreen", [], [Xml.PCData (string_of_bool pref.pref_max_view_fullscreen)]);
      Xml.Element ("pref_ocamldoc_paragraph_bgcolor_1", [], [Xml.PCData (match pref.pref_ocamldoc_paragraph_bgcolor_1 with None -> "" | Some x -> x)]);
      Xml.Element ("pref_ocamldoc_paragraph_bgcolor_2", [], [Xml.PCData (match pref.pref_ocamldoc_paragraph_bgcolor_2 with None -> "" | Some x -> x)]);
      Xml.Element ("pref_code_folding_enabled", [], [Xml.PCData (string_of_bool pref.pref_code_folding_enabled)]);
      Xml.Element ("pref_show_global_gutter", [], [Xml.PCData (string_of_bool pref.pref_show_global_gutter)]);
      Xml.Element ("pref_err_underline", [], [Xml.PCData (string_of_bool pref.pref_err_underline)]);
      Xml.Element ("pref_err_tooltip", [], [Xml.PCData (string_of_bool pref.pref_err_tooltip)]);
      Xml.Element ("pref_err_gutter", [], [Xml.PCData (string_of_bool pref.pref_err_gutter)]);
      Xml.Element ("pref_show_whitespace_chars", [], [Xml.PCData (string_of_bool pref.pref_show_whitespace_chars)]);
      Xml.Element ("pref_outline_show_types", [], [Xml.PCData (string_of_bool pref.pref_outline_show_types)]);
      Xml.Element ("pref_outline_width", [], [Xml.PCData (string_of_int pref.pref_outline_width)]);
      Xml.Element ("pref_outline_color_types", [], [Xml.PCData pref.pref_outline_color_types]);
      Xml.Element ("pref_outline_color_nor_bg", [], [Xml.PCData pref.pref_outline_color_nor_bg]);
      Xml.Element ("pref_outline_color_nor_fg", [], [Xml.PCData pref.pref_outline_color_nor_fg]);
      Xml.Element ("pref_outline_color_sel_bg", [], [Xml.PCData pref.pref_outline_color_sel_bg]);
      Xml.Element ("pref_outline_color_sel_fg", [], [Xml.PCData pref.pref_outline_color_sel_fg]);
      Xml.Element ("pref_outline_color_act_bg", [], [Xml.PCData pref.pref_outline_color_act_bg]);
      Xml.Element ("pref_outline_color_act_fg", [], [Xml.PCData pref.pref_outline_color_act_fg]);
      Xml.Element ("pref_outline_color_alt_rows", [], [Xml.PCData (match pref.pref_outline_color_alt_rows with Some x -> string_of_float x | _ -> "")]);
      Xml.Element ("pref_hmessages_width", [], [Xml.PCData (string_of_int pref.pref_hmessages_width)]);
      Xml.Element ("pref_vmessages_height", [], [Xml.PCData (string_of_int pref.pref_vmessages_height)]);
      Xml.Element ("pref_odoc_font", [], [Xml.PCData (pref.pref_odoc_font)]);
      Xml.Element ("pref_program_pdf_viewer", [], [Xml.PCData (pref.pref_program_pdf_viewer)]);
      Xml.Element ("pref_program_diff_graphical", [], [Xml.PCData (pref.pref_program_diff_graphical)]);
      Xml.Element ("pref_program_diff", [], [Xml.PCData (pref.pref_program_diff)]);
      Xml.Element ("pref_remember_window_geometry", [], [Xml.PCData (string_of_bool pref.pref_remember_window_geometry)]);
      Xml.Element ("pref_detach_message_panes_separately", [], [Xml.PCData (string_of_bool pref.pref_detach_message_panes_separately)]);
      Xml.Element ("pref_geometry_delayed", [], [Xml.PCData (string_of_bool pref.pref_geometry_delayed)]);
      Xml.Element ("pref_build_parallel", [], [Xml.PCData (Option.fold ~none:"" ~some:string_of_int pref.pref_build_parallel)]);
      Xml.Element ("pref_build_verbosity", [], [Xml.PCData (string_of_int pref.pref_build_verbosity)]);

    ])
  in
  let xml = Xml.to_string_fmt xml in
  "<!-- OCamlEditor XML Preferences -->\n" ^ xml
;;


let from_file filename =
  try
    let xml = Xml.parse_file filename in
    let default_pref = create_defaults () in
    let pref = create_defaults () in
    let value xml =
      match Xml.children xml with
        | [] -> ""
        | x :: [] -> Xml.pcdata x
        | _ -> assert false
    in
    let bg_color = ref "" in
    let bg_color_theme = ref false in
    Xml.iter begin fun node ->
      match Xml.tag node with
        | "pref_general_theme" -> pref.pref_general_theme <- (if value node = "" then None else Some (value node))
        | "pref_general_font" when value node <> "" -> pref.pref_general_font <- value node;
        | "pref_general_menubar_buttons" ->
          pref.pref_general_menubar_buttons <-
            List.map int_of_string (Str.split (Miscellanea.regexp ",") (value node))
        | "pref_general_splashscreen_enabled" -> pref.pref_general_splashscreen_enabled <- bool_of_string (value node)
        | "pref_check_updates" -> pref.pref_check_updates <- bool_of_string (value node)
        | "pref_base_font" -> pref.pref_base_font <- value node
        | "pref_tab_pos" -> pref.pref_tab_pos <- pos_of_string (value node)
        | "pref_tab_vertical_text" -> pref.pref_tab_vertical_text <- bool_of_string (value node)
        | "pref_tab_label_type" -> pref.pref_tab_label_type <- int_of_string (value node)
        | "pref_bg_color" -> bg_color := value node
        | "pref_bg_color_theme" -> bg_color_theme := (bool_of_string (value node))
        | "pref_bg_color_popup" -> pref.pref_bg_color_popup <- value node
        | "pref_fg_color_popup" -> pref.pref_fg_color_popup <- value node
        | "pref_tags" ->
          pref.pref_tags <- [];
          Xml.iter begin fun tp ->
            pref.pref_tags <- (
              (Xml.attrib tp "name"), (
              (color_of_string (Xml.attrib tp "color")),
              (weight_of_string (Xml.attrib tp "weight")),
              (style_of_string (Xml.attrib tp "style")),
              (underline_of_string (Xml.attrib tp "underline")),
              (try scale_of_string (Xml.attrib tp "scale") with Xml.No_attribute _ -> `MEDIUM),
              ((try bool_of_string (Xml.attrib tp "bg_default") with Xml.No_attribute _ -> true),
                (try color_of_string (Xml.attrib tp "bg_color") with Xml.No_attribute _ -> `NAME "#FFFFFF"))
            )) :: pref.pref_tags
          end node;
          List.iter begin fun (tag, _) ->
            if try ignore (List.assoc tag pref.pref_tags); false with Not_found -> true then begin
              let defaults = List.assoc tag default_pref.pref_tags in
              pref.pref_tags <- (tag, defaults) :: pref.pref_tags
            end
          end default_pref.pref_tags
        | "pref_editor_tab_width" -> pref.pref_editor_tab_width <- int_of_string (value node)
        | "pref_editor_tab_spaces" -> pref.pref_editor_tab_spaces <- bool_of_string (value node)
        | "pref_editor_wrap" -> pref.pref_editor_wrap <- bool_of_string (value node)
        | "pref_editor_trim_lines" -> pref.pref_editor_trim_lines <- bool_of_string (value node)
        | "pref_editor_format_on_save" -> pref.pref_editor_format_on_save <- bool_of_string (value node)
        | "pref_editor_bak" -> pref.pref_editor_bak <- bool_of_string (value node)
        | "pref_editor_custom_templ_filename" -> pref.pref_editor_custom_templ_filename <- value node
        | "pref_editor_mark_occurrences" -> pref.pref_editor_mark_occurrences <- ((bool_of_string (Xml.attrib node "enabled")), value node)
        | "pref_editor_left_margin" -> pref.pref_editor_left_margin <- int_of_string (value node)
        | "pref_editor_pixels_lines" -> pref.pref_editor_pixels_lines <- (int_of_string (Xml.attrib node "above")), (int_of_string (Xml.attrib node "below"))
        | "pref_editor_save_all_bef_comp" -> pref.pref_editor_save_all_bef_comp <- (bool_of_string (value node))
        | "pref_editor_dot_leaders" -> pref.pref_editor_dot_leaders <- (bool_of_string (value node))
        | "pref_editor_current_line_border" -> pref.pref_editor_current_line_border <- (bool_of_string (value node));
        | "pref_editor_indent_config" -> pref.pref_editor_indent_config <- value node
        | "pref_editor_indent_empty_line" -> pref.pref_editor_indent_empty_line <- bool_of_string (value node)
        | "pref_editor_cursor_aspect_ratio" -> pref.pref_editor_cursor_aspect_ratio <- float_of_string (value node)
        | "pref_compl_font" -> pref.pref_compl_font <- value node
        | "pref_compl_greek" -> pref.pref_compl_greek <- bool_of_string (value node)
        | "pref_compl_decorated" -> pref.pref_compl_decorated <- bool_of_string (value node)
        | "pref_compl_opacity" -> pref.pref_compl_opacity <- (match value node with "" -> None | n -> Some (float_of_string n))
        | "pref_output_font" -> pref.pref_output_font <- value node
        | "pref_output_bg" -> pref.pref_output_bg <- value node
        | "pref_output_fg_stdin" -> pref.pref_output_fg_stdin <- value node
        | "pref_output_fg_stdout" -> pref.pref_output_fg_stdout <- value node
        | "pref_output_fg_err" -> pref.pref_output_fg_err <- value node
        | "pref_output_fg_warn" -> pref.pref_output_fg_warn <- value node
        | "pref_smart_keys_home" -> pref.pref_smart_keys_home <- int_of_string (value node)
        | "pref_smart_keys_end" -> pref.pref_smart_keys_end <- int_of_string (value node)
        | "pref_annot_type_tooltips_enabled" -> pref.pref_annot_type_tooltips_enabled <- bool_of_string (value node)
        | "pref_annot_type_tooltips_delay" -> pref.pref_annot_type_tooltips_delay <- int_of_string (value node)
        | "pref_annot_type_tooltips_impl" -> pref.pref_annot_type_tooltips_impl <- int_of_string (value node)
        | "pref_search_word_at_cursor" -> pref.pref_search_word_at_cursor <- bool_of_string (value node)
        | "pref_highlight_current_line" -> pref.pref_highlight_current_line <- bool_of_string (value node)
        | "pref_show_line_numbers" -> pref.pref_show_line_numbers <- bool_of_string (value node)
        | "pref_editor_indent_lines" ->
          pref.pref_editor_indent_lines <- bool_of_string (value node);
          pref.pref_editor_indent_lines_color_s <-
            (try Xml.attrib node "solid_lines_color" with Xml.No_attribute _ -> default_pref.pref_editor_indent_lines_color_s);
          pref.pref_editor_indent_lines_color_d <-
            (try Xml.attrib node "solid_lines_dashed" with Xml.No_attribute _ -> default_pref.pref_editor_indent_lines_color_d);
        | "pref_right_margin_visible" -> pref.pref_right_margin_visible <- bool_of_string (value node)
        | "pref_right_margin" -> pref.pref_right_margin <- int_of_string (value node)
        | "pref_right_margin_color" -> pref.pref_right_margin_color <- (value node)
        | "pref_max_view_1_menubar" -> pref.pref_max_view_1_menubar <- bool_of_string (value node)
        | "pref_max_view_1_toolbar" -> pref.pref_max_view_1_toolbar <- bool_of_string (value node)
        | "pref_max_view_1_tabbar" -> pref.pref_max_view_1_tabbar <- bool_of_string (value node)
        | "pref_max_view_1_messages" -> pref.pref_max_view_1_messages <- bool_of_string (value node)
        | "pref_max_view_1_fullscreen" -> pref.pref_max_view_1_fullscreen <- bool_of_string (value node)
        | "pref_max_view_2" -> pref.pref_max_view_2 <- bool_of_string (value node)
        | "pref_max_view_2_menubar" -> pref.pref_max_view_2_menubar <- bool_of_string (value node)
        | "pref_max_view_2_toolbar" -> pref.pref_max_view_2_toolbar <- bool_of_string (value node)
        | "pref_max_view_2_tabbar" -> pref.pref_max_view_2_tabbar <- bool_of_string (value node)
        | "pref_max_view_2_messages" -> pref.pref_max_view_2_messages <- bool_of_string (value node)
        | "pref_max_view_2_fullscreen" -> pref.pref_max_view_2_fullscreen <- bool_of_string (value node)
        | "pref_max_view_fullscreen" -> pref.pref_max_view_fullscreen <- bool_of_string (value node)
        | "pref_ocamldoc_paragraph_bgcolor" ->
          pref.pref_ocamldoc_paragraph_bgcolor_1 <- Some (value node);
          pref.pref_ocamldoc_paragraph_bgcolor_2 <- default_pref.pref_ocamldoc_paragraph_bgcolor_2;
        | "pref_ocamldoc_paragraph_bgcolor_1" -> pref.pref_ocamldoc_paragraph_bgcolor_1 <- (match value node with "" -> None | x -> Some x)
        | "pref_ocamldoc_paragraph_bgcolor_2" -> pref.pref_ocamldoc_paragraph_bgcolor_2 <- (match value node with "" -> None | x -> Some x)
        | "pref_code_folding_enabled" -> pref.pref_code_folding_enabled <- bool_of_string (value node)
        | "pref_show_global_gutter" -> pref.pref_show_global_gutter <- bool_of_string (value node)
        | "pref_err_underline" -> pref.pref_err_underline <- bool_of_string (value node)
        | "pref_err_tooltip" -> pref.pref_err_tooltip <- bool_of_string (value node)
        | "pref_err_gutter" -> pref.pref_err_gutter <- bool_of_string (value node)
        | "pref_show_whitespace_chars" -> pref.pref_show_whitespace_chars <- bool_of_string (value node)
        | "pref_outline_show_types" -> pref.pref_outline_show_types <- bool_of_string (value node)
        | "pref_outline_width" -> pref.pref_outline_width <- int_of_string (value node)
        | "pref_outline_color_types" -> pref.pref_outline_color_types <- value node
        | "pref_outline_color_nor_bg" -> pref.pref_outline_color_nor_bg <- value node
        | "pref_outline_color_nor_fg" -> pref.pref_outline_color_nor_fg <- value node
        | "pref_outline_color_sel_bg" -> pref.pref_outline_color_sel_bg <- value node
        | "pref_outline_color_sel_fg" -> pref.pref_outline_color_sel_fg <- value node
        | "pref_outline_color_act_bg" -> pref.pref_outline_color_act_bg <- value node
        | "pref_outline_color_act_fg" -> pref.pref_outline_color_act_fg <- value node
        | "pref_outline_color_alt_rows" -> pref.pref_outline_color_alt_rows <- (match value node with "" -> None | x -> Some (float_of_string x))
        | "pref_hmessages_width" -> pref.pref_hmessages_width <- int_of_string (value node)
        | "pref_vmessages_height" -> pref.pref_vmessages_height <- int_of_string (value node)
        | "pref_odoc_font" -> pref.pref_odoc_font <- value node
        | "pref_pdf_viewer" | "pref_program_pdf_viewer" -> pref.pref_program_pdf_viewer <- value node
        | "pref_program_diff" -> pref.pref_program_diff <- value node
        | "pref_program_diff_graphical" -> pref.pref_program_diff_graphical <- value node
        | "pref_remember_window_geometry" -> pref.pref_remember_window_geometry <- bool_of_string (value node)
        | "pref_detach_message_panes_separately" -> pref.pref_detach_message_panes_separately <- bool_of_string (value node)
        | "pref_geometry_delayed" -> pref.pref_geometry_delayed <- bool_of_string (value node)
        | "pref_build_parallel" -> pref.pref_build_parallel <- (match (value node) with "" -> None | n -> Some (int_of_string n))
        | "pref_build_verbosity" -> pref.pref_build_verbosity <- int_of_string (value node)

       | _ -> ()
    end xml;
    pref.pref_bg_color <- (!bg_color, !bg_color_theme);
    pref
  with Xml.File_not_found _ -> create_defaults()
;;

(** geometry_memo *)
let geometry_memo = Gmisclib.Window.GeometryMemo.create ~filename:Oe_config.geometry_memo_filename ()

(** save *)
let save () =
  let xml = to_xml preferences#get in
  let chan = open_out_bin pref_filename in
  lazy (output_string chan xml) @$ lazy (close_out chan);
  preferences#set {preferences#get with pref_timestamp = Unix.gettimeofday()}

(** load *)
let load () =
  let pref = try from_file pref_filename with ex -> begin
    Printf.eprintf "File \"preferences.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    create_defaults ()
  end in
  Gmisclib.Window.GeometryMemo.set_enabled geometry_memo  pref.pref_remember_window_geometry;
  Gmisclib.Window.GeometryMemo.set_delayed geometry_memo  pref.pref_geometry_delayed;
  Otherwidgets_config.geometry_memo := (fun () -> geometry_memo);
  preferences#set pref;
;;

(** tag_color *)
let tag_color tagname =
  match List.assoc tagname preferences#get.pref_tags with
    | color, _, _, _, _, _ -> GDraw.color color

(** tag_underline *)
let tag_underline tagname =
  match List.assoc tagname preferences#get.pref_tags with
    | _, _, _, underline, _, _ -> underline

(** tag_colorname *)
let tag_colorname tagname =
  match List.assoc tagname preferences#get.pref_tags with
    | `NAME color, _, _, _, _, _ -> color
    | _ -> assert false

(** reset_defaults *)
let reset_defaults () =
  if Sys.file_exists pref_filename then Sys.remove pref_filename;
  preferences#set (create_defaults());
  save()

let _ = begin
  load();
end



