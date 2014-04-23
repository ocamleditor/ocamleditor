(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

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

  val dialog_rename :
    editor:< close : (< get_filename : string; .. > as 'a) -> unit;
             create_file : ?remote:Editor_file_type.remote_login ->
               string -> 'b;
             pages : 'a list; .. > ->
    page:< buffer : < check_lexical_coloring_enabled : string -> bool;
                      colorize : ?start:'c -> ?stop:'d -> unit -> unit;
                      end_iter : 'e; lexical_enabled : bool;
                      remove_tag : 'f -> start:'g -> stop:'e -> unit;
                      set_lexical_enabled : bool -> 'h; start_iter : 'g;
                      tag_table_lexical : 'f option list; .. >;
           error_indication : < remove_tag : unit -> unit; .. >;
           file : < remote : Editor_file_type.remote_login option;
                    rename : string -> 'i; .. >
               option;
           get_filename : string; get_title : string;
           set_file : 'b option -> 'j; .. > ->
    unit -> unit

  val dialog_save_as :
    editor:< goto_view : 'a -> 'b;
             load_page : ?scroll:'c -> (< view : 'a; .. > as 'd) -> 'e;
             open_file : active:bool ->
               scroll_offset:int ->
               offset:int -> ?remote:Editor_file_type.remote_login -> string -> 'd option;
             .. > ->
    page:< buffer : < as_text_buffer : < as_gtext_buffer : GText.buffer;
                                         .. >;
                      .. >;
           file : < remote : Editor_file_type.remote_login option;
                    .. >
               option;
           get_filename : string; get_title : string;
           revert : unit -> 'g; .. > ->
    unit -> unit

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

