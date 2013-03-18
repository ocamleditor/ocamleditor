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


module type REMOTE = sig
  exception Error of int * string * string
  val create : host:string -> user:string -> pwd:string -> sslkey:string -> sshpublickeyfile:string -> sslkeypasswd:string -> filename:string -> Editor_file_type.abstract_file
  class widget :
    ?packing:(GObj.widget -> unit) ->
    unit ->
    object
      val obj : Gtk.widget Gtk.obj
      method as_widget : Gtk.widget Gtk.obj
      method coerce : GObj.widget
      method connect : signals
      method destroy : unit -> unit
      method drag : GObj.drag_ops
      method get_oid : int
      method misc : GObj.misc_ops
      method apply : unit -> unit
    end
    and open_file :
      unit ->
      object
        val mutable callbacks : (GtkSignal.id * (Editor_file_type.remote_login * string -> unit)) list
        method call : Editor_file_type.remote_login * string -> unit
        method callbacks : (GtkSignal.id * (Editor_file_type.remote_login * string -> unit)) list
        method connect :
          after:bool -> callback:(Editor_file_type.remote_login * string -> unit) -> GtkSignal.id
        method disconnect : GtkSignal.id -> bool
      end
    and signals :
      open_file:open_file ->
      object ('a)
        val after : bool
        val mutable disconnectors : (GtkSignal.id -> bool) list
        method after : 'a
        method disconnect : GtkSignal.id -> unit
        method open_file : callback:(Editor_file_type.remote_login * string -> unit) -> GtkSignal.id
      end
end

let remote : (module REMOTE) option ref = ref None



