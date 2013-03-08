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

(** A more compact alternative for the {!GButton.menu_tool_button}. *)
class menu_tool_button :
  toolbar:GButton.toolbar ->
  ?homogeneous:bool ->
  ?stock:GtkStock.id ->
  ?label:string ->
  ?packing:(GButton.tool_item_o -> unit) ->
  unit ->
  object
    method as_tool_item : GButton.tool_item
    method as_widget : Gtk.widget Gtk.obj
    method button : GButton.button
    method clear_menu_only : unit -> unit
    method coerce : GObj.widget
    method connect : Button_menu.signals
    method destroy : unit -> unit
    method drag : GObj.drag_ops
    method get_oid : int
    method misc : GObj.misc_ops
    method set_image : GObj.widget -> unit
    method set_menu_only : unit -> unit
  end

val create :
  toolbar:GButton.toolbar ->
  ?homogeneous:bool ->
  ?stock:GtkStock.id ->
  ?label:string ->
  ?packing:(GButton.tool_item_o -> unit) -> unit -> menu_tool_button
