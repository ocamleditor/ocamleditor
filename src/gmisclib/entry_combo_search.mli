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


(** A widget used to choose from a list of items, like GtkComboBox,
    which allows you to select an item by searching as you type. *)
class widget :
  ?model:GTree.list_store ->
  ?width:int ->
  ?popup_width:int ->
  ?popup_height:int ->
  ?packing:(GObj.widget -> unit) ->
  unit ->
  object
    method active : string option
    method as_widget : Gtk.widget Gtk.obj
    method coerce : GObj.widget
    method connect : signals
    method destroy : unit -> unit
    method drag : GObj.drag_ops
    method get_oid : int
    method misc : GObj.misc_ops
    method set_active : string option -> bool
    method set_default_column : bool GTree.column -> unit
    method set_label_column : string GTree.column -> unit
    method set_markup_column : string GTree.column -> unit
    method set_value_column : string option GTree.column -> unit
  end
and changed :
  unit ->
  object
    method call : string option -> unit
    method callbacks : (GtkSignal.id * (string option -> unit)) list
    method connect :
      after:bool -> callback:(string option -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> bool
  end
and signals :
  changed:changed ->
  object ('a)
    method after : 'a
    method changed : callback:(string option -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> unit
  end
