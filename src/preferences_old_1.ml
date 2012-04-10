(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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

type t = {
  mutable pref_base_font : string;
  mutable pref_tab_pos : Gtk.Tags.position;
  mutable pref_bg_color : string * bool;
  mutable pref_bg_color_popup : string;
  mutable pref_tags : (string * text_properties) list
}

and text_properties = GDraw.color * Pango.Tags.weight * Pango.Tags.style * Pango.Tags.underline * Pango.Tags.scale

let pref_filename = Filename.concat Oe_config.ocamleditor_user_home "preferences"

