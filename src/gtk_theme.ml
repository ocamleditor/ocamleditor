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

let set_theme ?theme () =
  Gmisclib.Idle.add begin fun () ->
    begin
      match Oe_config.themes_dir with
        | Some _ ->
          let theme = match theme with Some _ as x -> x | _ -> preferences#get.pref_general_theme in
          Opt.may theme
            (fun theme -> kprintf GtkMain.Rc.parse_string "gtk-theme-name = \"%s\"" theme);
        | _ -> ()
    end;
    (** General *)
    GtkMain.Rc.parse_string "
gtk-button-images = 0
gtk-font-name=\"Sans 8\"
";
    (** Style for the Structure Pane (Outline) *)
    begin
      match Oe_config.outline_alternating_row_colors with
        | None -> ()
        | Some x ->
          let pref = Preferences.preferences#get in
          let base_color = fst pref.Preferences.pref_bg_color in
          kprintf GtkMain.Rc.parse_string "\
style \"outline-treestyle\" {
  GtkTreeView::even-row-color = \"%s\"
  GtkTreeView::odd-row-color = \"%s\"
}
widget \"*.outline_treeview\" style \"outline-treestyle\"
" base_color (Color.name (Color.set_value x (`NAME base_color)))
    end;
    (** Style for the Target List *)
    begin
      match Oe_config.targetlist_alternating_row_colors with
        | None -> ()
        | Some x ->
          let pref = Preferences.preferences#get in
          let base_color = fst pref.Preferences.pref_bg_color in
          kprintf GtkMain.Rc.parse_string "\
style \"targetlist-treestyle\" {
  GtkTreeView::even-row-color = \"%s\"
  GtkTreeView::odd-row-color = \"%s\"
}
widget \"*.targetlist_treeview\" style \"targetlist-treestyle\"
" base_color (Color.name (Color.set_value x (`NAME base_color)))
    end;
    (** Small buttons *)
    GtkMain.Rc.parse_string "
style \"small-button\" {
  GtkButton::child-displacement-x = 0
  GtkButton::child-displacement-y = 0
  GtkButton::inner-border = { 0, 0, 0, 0 }
  xthickness = 0
  ythickness = 0
}
widget \"*.smallbutton\" style \"small-button\"
";
  end;;

