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


(** A widget containing a button and a small additional button with an arrow.
    When clicked, the arrow button pops up a dropdown menu. *)
class button_menu :
  ?label:string ->
  ?relief:Gtk.Tags.relief_style ->
  ?stock:GtkStock.id ->
  ?packing:(GObj.widget -> unit) ->
  unit ->
  object
    method as_widget : Gtk.widget Gtk.obj
    method button : GButton.button
    method clear_menu_only : unit -> unit
    method coerce : GObj.widget
    method connect : signals
    method destroy : unit -> unit
    method drag : GObj.drag_ops
    method get_oid : int
    method misc : GObj.misc_ops
    method set_image : GObj.widget -> unit

    (** Set this flag if you want to make the main button to show the menu when cliked. *)
    method set_menu_only : unit -> unit
  end

and signals :
  clicked:unit GUtil.signal ->
  show_menu:(string option ref * GMenu.menu) GUtil.signal ->
  object ('a)
    val after : bool
    method disconnect : GtkSignal.id -> unit
    method after : 'a

    (** Emitted when the main button is clicked. *)
    method clicked : callback:(unit -> unit) -> GtkSignal.id

    (** Emitted before the menu is shown. *)
    method show_menu :
      callback:(string option ref * GMenu.menu -> unit) -> GtkSignal.id
  end

val create :
  ?label:string ->
  ?relief:Gtk.Tags.relief_style ->
  ?stock:GtkStock.id -> ?packing:(GObj.widget -> unit) -> unit -> button_menu
