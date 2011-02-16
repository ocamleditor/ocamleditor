(*

  OCamlEditor
  Copyright (C) 2010, 2011 Francesco Tovagliari

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

open Gobject.Data

class checklist ?packing elements =
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC
      ~vpolicy:`AUTOMATIC ?packing () in
  let all = GButton.button ~label:"Select All" () in
  let none = GButton.button ~label:"None" () in
  let cols = new GTree.column_list in
  let check = cols#add boolean in
  let name = cols#add string in
  let model = GTree.list_store cols in
  let treeview = GTree.view ~model ~packing:sw#add ~height:300 () in
  object (self)
    inherit GObj.widget sw#as_widget
      method button_all = all
      method button_none = none
    method iter (f : bool -> string -> unit) =
      let row = model#get_iter (GTree.Path.create [0]) in
      let finish = ref false in
      while not !finish do
        let n = model#get ~row ~column:name in
        let c = model#get ~row ~column:check in
        f c n;
        finish := not (model#iter_next row);
      done;
    method iter_set (f : bool -> string -> (bool * string)) =
      let row = model#get_iter (GTree.Path.create [0]) in
      let finish = ref false in
      while not !finish do
        let c = model#get ~row ~column:check in
        let n = model#get ~row ~column:name in
        let c', n' = f c n in
        model#set ~row ~column:check c';
        model#set ~row ~column:name n';
        finish := not (model#iter_next row);
      done;
    initializer
      let renderer_text = GTree.cell_renderer_text [`XALIGN 0.] in
      let renderer_toggle = GTree.cell_renderer_toggle [`XALIGN 0.] in
      renderer_toggle#connect#toggled ~callback:begin fun path ->
        let row = model#get_iter path in
        let b = model#get ~row ~column:check in
        model#set ~row ~column:check (not b);
      end;
      List.iter begin fun (v, n) ->
        let row = model#append () in
        model#set ~row ~column:check v;
        model#set ~row ~column:name n;
      end elements;
      treeview#append_column (GTree.view_column ~renderer:(renderer_toggle, ["active", check]) ());
      treeview#append_column (GTree.view_column ~renderer:(renderer_text, ["text", name]) ());
      treeview#set_headers_visible false;

      all#connect#clicked ~callback:(fun () -> self#iter_set (fun c n -> (true, n)));
      none#connect#clicked ~callback:(fun () -> self#iter_set (fun c n -> (false, n)));
      ()
  end
