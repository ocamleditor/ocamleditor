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
    GtkMain.Rc.parse_string "
style \"small-button\" {
  GtkButton::child-displacement-x = 0
  GtkButton::child-displacement-y = 0
  GtkButton::inner-border = { 0, 0, 0, 0 }
  xthickness = 0
  ythickness = 0
}
widget \"*.smallbutton\" style \"small-button\"
gtk-button-images = 0
gtk-font-name=\"Sans 8\"
"
  end;;

