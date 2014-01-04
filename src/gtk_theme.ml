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


open Printf
open Miscellanea
open Preferences

let avail_themes =
  match Oe_config.themes_dir with
    | Some dir -> List.sort compare (Array.to_list (Sys.readdir dir))
    | _ -> []

(* Condensed font for the file list in the search results pane. None is default font. (`STRETCH `CONDENSED doesn't work) *)
let find_text_output_font_condensed : string option ref = ref None
let set_find_text_output_font_condensed context =
  find_text_output_font_condensed :=
    try Some (List.find (Gtk_util.try_font context) ["Arial"; "Helvetica"; "Sans"])
    with Not_found -> None

let set_theme ?theme ~context () =
  let pref = Preferences.preferences#get in
  let style_smallbutton, apply_smallbutton = "\
      style \"small-button\" {
        GtkButton::child-displacement-x = 0
        GtkButton::child-displacement-y = 0
        GtkButton::inner-border = { 0, 0, 0, 0 }
        xthickness = 0
        ythickness = 0
      }", "widget \"*.smallbutton\" style \"small-button\""
  in
  let style_outline, apply_outline =
    match Oe_config.outline_alternating_row_colors with
      | None -> "", ""
      | Some x ->
        let base_color = fst pref.Preferences.pref_bg_color in
        sprintf "
          style \"outline-treestyle\" {
            GtkTreeView::even-row-color = \"%s\"
            GtkTreeView::odd-row-color = \"%s\"
          }" base_color (Color.name (Color.set_value x (`NAME base_color))),
        "widget \"*.outline_treeview\" style \"outline-treestyle\""
  in
  let style_targetlist, apply_targetlist =
    match Oe_config.targetlist_alternating_row_colors with
      | None -> "", ""
      | Some x ->
        let base_color = fst pref.Preferences.pref_bg_color in
        sprintf "
          style \"targetlist-treestyle\" {
            GtkTreeView::even-row-color = \"%s\"
            GtkTreeView::odd-row-color = \"%s\"
          }" base_color (Color.name (Color.set_value x (`NAME base_color))),
        "widget \"*.targetlist_treeview\" style \"targetlist-treestyle\""
  in
  let gtk_theme =
    match Oe_config.themes_dir with
      | Some _ ->
        let theme = match theme with Some _ as x -> x | _ -> preferences#get.pref_general_theme in
        Opt.map_default theme ""
          (fun theme -> sprintf "gtk-theme-name = \"%s\"" theme);
      | _ -> ""
  in
  let gtk_font_name =
    match String.trim pref.pref_general_font with
      | "" ->
        begin
          try
            let family = List.find (Gtk_util.try_font context) ["Arial"; "Helvetica"; "Sans"] in
            let font_name = sprintf "%s 9" family in
            pref.Preferences.pref_general_font <- font_name;
            Preferences.save();
            sprintf "gtk-font-name = \"%s\"" font_name;
          with Not_found -> ""
        end;
      | x -> sprintf "gtk-font-name = \"%s\"" x
  in
  let rc =
    String.concat "\n" [
      style_smallbutton; style_outline; style_targetlist;
      apply_smallbutton; apply_outline; apply_targetlist;
      "gtk-button-images = 0";
      gtk_theme;
      gtk_font_name;
    ]
  in
  GtkMain.Rc.parse_string rc;
  set_find_text_output_font_condensed context
;;

