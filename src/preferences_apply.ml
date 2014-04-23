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


open Printf

let apply (view : Text.view) pref =
  view#set_left_margin (pref.Preferences.pref_editor_left_margin + Oe_config.current_line_width);
  view#set_current_line_border_x1 (view#left_margin - (max 1 (Oe_config.current_line_width / 2)) - 1);
  view#set_current_line_border_x2 (view#left_margin - Oe_config.current_line_width / 2 + Oe_config.current_line_border_adjust + 1);
  kprintf GtkMain.Rc.parse_string "
style \"s1\" {
  GtkTextView::cursor_aspect_ratio = %.1f
}
class \"GtkTextView\" style \"s1\"
" pref.Preferences.pref_editor_cursor_aspect_ratio;
  let above, below = pref.Preferences.pref_editor_pixels_lines in
  view#set_pixels_above_lines above;
  view#set_pixels_below_lines below;
  let color = if snd pref.Preferences.pref_bg_color
    then `COLOR (view#misc#style#base `NORMAL)
    else (`NAME (fst pref.Preferences.pref_bg_color))
  in
  view#mark_occurrences_manager#mark();
  view#options#set_mark_occurrences pref.Preferences.pref_editor_mark_occurrences;
  view#mark_occurrences_manager#mark();
  view#options#set_show_indent_lines pref.Preferences.pref_editor_indent_lines;
  view#options#set_indent_lines_color_solid (`NAME pref.Preferences.pref_editor_indent_lines_color_s);
  view#options#set_indent_lines_color_dashed (`NAME pref.Preferences.pref_editor_indent_lines_color_d);
  view#options#set_show_line_numbers pref.Preferences.pref_show_line_numbers;
  view#options#set_line_numbers_font pref.Preferences.pref_base_font;
  view#modify_font pref.Preferences.pref_base_font;
  view#options#set_word_wrap pref.Preferences.pref_editor_wrap;
  view#options#set_show_dot_leaders pref.Preferences.pref_editor_dot_leaders;
  view#options#set_current_line_border_enabled pref.Preferences.pref_editor_current_line_border;
  view#options#set_text_color (Color.name_of_gdk (Preferences.tag_color "lident"));
  let default_bg_color =
    if snd pref.Preferences.pref_bg_color then begin
      (* "Use theme color" option removed *)
      let color = (*`NAME*) (fst ((Preferences.create_defaults()).Preferences.pref_bg_color)) in
      (*view#misc#modify_bg [`NORMAL, (Oe_config.gutter_color_bg color)];*)
      view#misc#modify_base [`NORMAL, `NAME color];
      color;
    end else begin
      let color = (*`NAME*) (fst pref.Preferences.pref_bg_color) in
      view#misc#modify_base [`NORMAL, `NAME color];
      color;
    end;
  in
  view#options#set_base_color default_bg_color;
  if pref.Preferences.pref_highlight_current_line then begin
    view#options#set_highlight_current_line
      (Some (match (List.assoc "highlight_current_line" pref.Preferences.pref_tags)
        with ((`NAME c), _, _, _, _, _) -> c | _ -> assert false));
  end else (view#options#set_highlight_current_line None);
  view#tbuffer#set_tab_width pref.Preferences.pref_editor_tab_width;
  view#tbuffer#set_tab_spaces pref.Preferences.pref_editor_tab_spaces;
  view#options#set_smart_home (pref.Preferences.pref_smart_keys_home = 0);
  view#options#set_smart_end (pref.Preferences.pref_smart_keys_end = 1);
  if pref.Preferences.pref_right_margin_visible then begin
    view#options#set_visible_right_margin (Some
      (pref.Preferences.pref_right_margin, `NAME pref.Preferences.pref_right_margin_color))
  end else (view#options#set_visible_right_margin None);
  match List_opt.assoc "selection" pref.Preferences.pref_tags with
    | Some (fg_color, _, _, _, _, (bg_default, bg_color)) ->
      let bg_color = if bg_default then view#options#text_color else bg_color in
      view#misc#modify_base [`SELECTED, bg_color; `ACTIVE, bg_color];
      let fg_color = if bg_default then `NAME default_bg_color else fg_color in
      view#misc#modify_text [`SELECTED, fg_color; `ACTIVE, fg_color];
    | _ -> assert false


