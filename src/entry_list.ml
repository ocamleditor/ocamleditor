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


class widget ?(height=150) ?packing () =
  let changed = new changed () in
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ?packing () in
  let cols = new GTree.column_list in
  let col_bind  = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [`EDITABLE true; `CELL_BACKGROUND "#ffffff"] in
  let vc_bind = GTree.view_column ~renderer:(renderer, ["text", col_bind]) ~title:"" () in
  let view = GTree.view ~model ~headers_visible:false ~reorderable:true ~enable_search:false ~height ~packing:sw#add () in
  let _ = view#selection#set_mode `MULTIPLE in
  let _ = view#append_column vc_bind in
  (*let _ = view#misc#set_property "enable-grid-lines" (`INT 1) in*)
object (self)
  inherit GObj.widget sw#as_widget
  val mutable sign = None;

  method entries =
    let res = ref [] in
    model#foreach begin fun _ row ->
      let entry = model#get ~row ~column:col_bind in
      if entry <> "" then (res := entry :: !res);
      false
    end;
    List.rev !res

  method set_entries entries =
    Gaux.may sign ~f:(GtkSignal.handler_block renderer#as_renderer);
    model#clear();
    List.iter begin fun e ->
      let row = model#append () in
      model#set ~row ~column:col_bind e
    end entries;
    ignore (self#append_empty());
    changed#call();
    Gaux.may sign ~f:(GtkSignal.handler_unblock renderer#as_renderer);

  method remove_empty_lines () =
    let rows = ref [] in
    model#foreach begin fun _ row ->
      let text = model#get ~row ~column:col_bind in
      if text = "" then (rows := row :: !rows);
      false
    end;
    List.iter (fun row -> ignore (model#remove row)) !rows;

  method count () =
    let count = ref 0 in
    model#foreach (fun _ _ -> incr count; false);
    !count

  method append_empty () =
    self#remove_empty_lines();
    let last = self#count () - 1 in
    if last < 0 || (model#get ~row:(model#get_iter (GTree.Path.create [last])) ~column:col_bind) <> "" then begin
      let row = model#append () in
      model#set ~row ~column:col_bind "";
      row
    end else (model#get_iter (GTree.Path.create [last]))

  method remove_selected () =
    try
      let removing = ref [] in
      let selection = view#selection#get_selected_rows in
      removing := List.map model#get_row_reference selection;
      List.iter (fun reference -> ignore (model#remove reference#iter)) !removing;
      changed#call();
      let last = self#append_empty() in
      view#set_cursor (model#get_path last) vc_bind;
    with Failure("GtkTree.TreeModel.get_iter") -> ()

  method model = model
  method view = view
  method view_column = vc_bind

  initializer
    sign <- Some (renderer#connect#edited ~callback:begin fun path txt ->
      let row = model#get_iter path in
      model#set ~row ~column:col_bind txt;
      ignore (self#append_empty());
      let row = model#get_iter path in
      view#set_cursor (model#get_path row) vc_bind;
      changed#call();
    end);

  method connect = new signals ~changed
end

and changed () = object (self) inherit [unit] GUtil.signal () as super end

and signals ~changed =
object (self)
  inherit GUtil.ml_signals [changed#disconnect]
  method changed = changed#connect ~after
end

let create = new widget
