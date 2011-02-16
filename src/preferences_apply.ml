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


let apply (view : Text.view) pref =
  let color = if snd pref.Preferences.pref_bg_color
    then `COLOR (view#misc#style#base `NORMAL)
    else (`NAME (fst pref.Preferences.pref_bg_color))
  in
  view#set_show_indent_lines pref.Preferences.pref_indent_lines;
  view#set_show_line_numbers pref.Preferences.pref_show_line_numbers;
  view#set_line_numbers_font pref.Preferences.pref_base_font;
  view#modify_font pref.Preferences.pref_base_font;
  view#set_word_wrap pref.Preferences.pref_editor_wrap;
  view#set_base_color begin
    if snd pref.Preferences.pref_bg_color then begin
      (* "Use theme color" option removed *)
      let color = `NAME (fst ((Preferences.create_defaults()).Preferences.pref_bg_color)) in
      (*view#misc#modify_bg [`NORMAL, (Oe_config.gutter_color_bg color)];*)
      view#misc#modify_base [`NORMAL, color];
      color;
    end else begin
      let color = `NAME (fst pref.Preferences.pref_bg_color) in
      view#misc#modify_base [`NORMAL, color];
      color;
    end;
  end;
  if pref.Preferences.pref_highlight_current_line then begin
    view#set_highlight_current_line
      (Some (match (List.assoc "highlight_current_line" pref.Preferences.pref_tags)
        with ((`NAME c), _, _, _) -> c | _ -> assert false));
  end else (view#set_highlight_current_line None);
  view#tbuffer#set_tab_width pref.Preferences.pref_editor_tab_width;
  view#tbuffer#set_tab_spaces pref.Preferences.pref_editor_tab_spaces;
  view#set_smart_home (pref.Preferences.pref_smart_keys_home = 0);
  view#set_smart_end (pref.Preferences.pref_smart_keys_end = 1);
  if pref.Preferences.pref_right_margin_visible then begin
    view#set_visible_right_margin (Some
      (pref.Preferences.pref_right_margin, Oe_config.right_margin_line_color))
  end else (view#set_visible_right_margin None);


