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
    method coerce : GObj.widget
    method connect : signals
    method destroy : unit -> unit
    method drag : GObj.drag_ops
    method get_oid : int
    method misc : GObj.misc_ops
    method set_icon_widget : GObj.widget -> unit
  end
and signals :
  clicked:unit GUtil.signal ->
  popup:(string ref * GMenu.menu) GUtil.signal ->
  object ('a)
    method after : 'a
    method clicked : callback:(unit -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> unit
    method popup : callback:(string ref * GMenu.menu -> unit) -> GtkSignal.id
  end
val create :
  toolbar:GButton.toolbar ->
  ?homogeneous:bool ->
  ?stock:GtkStock.id ->
  ?label:string ->
  ?packing:(GButton.tool_item_o -> unit) -> unit -> menu_tool_button
