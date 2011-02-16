(*

  OCamlEditor
  Copyright (C) 2010 Francesco Tovagliari

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

(** Redo stack is automatically cleared at the beginning of every interactive
    action. Inserting or deleting text programmatically requires operations to
    be enclosed in a user action, i.e. between [GText.text#begin_user_action]
    and [GText.text#end_user_action] or [Undo.manager#begin_block] and
    [Undo.manager#end_block].
  *)
class manager : 
  buffer:GText.buffer ->
  object
    method begin_block : name:string -> unit
    method can_redo : bool
    method can_undo : bool
    method connect : manager_signals
    method disable : unit -> unit
    method enable : unit -> unit
    method end_block : unit -> unit
    method redo : unit -> bool
    method undo : unit -> bool
    method func : (unit -> bool) -> inverse:(unit -> bool) -> unit
  end
and manager_signals :
  undo:string GUtil.signal ->
  redo:string GUtil.signal ->
  can_undo_changed:bool GUtil.signal ->
  can_redo_changed:bool GUtil.signal ->
  object ('a)
    method after : 'a
    method can_undo_changed : callback:(bool -> unit) -> GtkSignal.id
    method can_redo_changed : callback:(bool -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> unit
    method redo : callback:(name:string -> unit) -> GtkSignal.id
    method undo : callback:(name:string -> unit) -> GtkSignal.id
  end
