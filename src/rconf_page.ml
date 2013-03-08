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


open Printf

(** create_entry *)
let create_entry ?label ~packing () =
  let vbox = GPack.vbox ~packing () in
  Gaux.may label ~f:(fun markup -> GMisc.label ~markup ~xalign:0.0 ~packing:vbox#pack ());
  GEdit.entry ~packing:vbox#pack ()

(** view *)
class view ~target_list ?packing () =
  let vbox = GPack.vbox ~spacing:8 ?packing () in
  let title = GMisc.label ~markup:"<big><b>Executable</b></big>" ~xalign:0.0 ~packing:vbox#pack () in
  let entry_name = create_entry ~packing:vbox#pack () in
  (* Build Target *)
  let box = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Target" ~xalign:0.0 ~packing:box#add () in
  let cols = new GTree.column_list in
  let col_bc = cols#add Gobject.Data.caml in
  let col_bc_pixbuf = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf") in
  let col_name = cols#add Gobject.Data.string in
  let model_bc = GTree.list_store cols in
  let combo_bc = GEdit.combo_box ~model:model_bc ~packing:box#add () in
  let rend = GTree.cell_renderer_text [`XPAD 5] in
  let rend_pixbuf = GTree.cell_renderer_pixbuf [] in
  let _ = combo_bc#pack rend_pixbuf in
  let _ = combo_bc#pack rend in
  let _ = combo_bc#add_attribute rend_pixbuf "pixbuf" col_bc_pixbuf in
  let _ = combo_bc#add_attribute rend "text" col_name in
  (* Prior Build Task *)
  let box = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Prior build task" ~xalign:0.0 ~packing:box#pack () in
  let cols = new GTree.column_list in
  let col_task_pb = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf") in
  let col_task_descr = cols#add Gobject.Data.string in
  let col_task = cols#add Gobject.Data.caml in
  let model_task = GTree.list_store cols in
  let combo_task = GEdit.combo_box ~active:0 ~model:model_task ~packing:box#pack () in
  let rend_pb = GTree.cell_renderer_pixbuf [] in
  let rend = GTree.cell_renderer_text [`XPAD 5] in
  let _ = combo_task#pack rend_pb in
  let _ = combo_task#pack rend in
  let _ = combo_task#add_attribute rend_pb "pixbuf" col_task_pb in
  let _ = combo_task#add_attribute rend "markup" col_task_descr in
  (* Command Line Arguments and Environment Variables *)
  let entry_args, entry_env = Args_env_widget.create vbox in
object (self)
  inherit GObj.widget vbox#as_widget
  val mutable rconfig = None

  method set_tasks () =
    match combo_bc#active_iter with
      | None -> ()
      | Some row ->
        let bc = model_bc#get ~row ~column:col_bc in
        let tasks = [
          `NONE, Icons.empty_16;
          `CLEAN, Icons.clear_build_16;
          `COMPILE, Icons.build_16;
          `REBUILD, Icons.empty_16
        ] @ (List.map (fun x -> (`ETASK x, Icons.etask_16)) bc.Target.external_tasks) in
        model_task#clear();
        List.iter begin fun (task, icon) ->
          let row = model_task#append () in
          model_task#set ~row ~column:col_task_pb icon;
          model_task#set ~row ~column:col_task_descr (Target.markup_of_task task);
          model_task#set ~row ~column:col_task task
        end tasks;
        Gaux.may rconfig ~f:(fun rc -> rc.Rconf.target_id <- bc.Target.id);

  initializer
    self#set_targets();
    ignore (combo_bc#connect#changed ~callback:self#set_tasks);
    ignore (combo_task#connect#changed ~callback:begin fun () ->
      Gaux.may rconfig ~f:begin fun rc ->
        Gaux.may combo_task#active_iter ~f:(fun row ->
          rc.Rconf.build_task <- model_task#get ~row ~column:col_task)
      end;
    end);
    ignore (entry_args#connect#changed ~callback:begin fun () ->
      Gaux.may rconfig ~f:(fun rc -> rc.Rconf.args <- entry_args#entries);
    end);
    ignore (entry_env#connect#changed ~callback:begin fun () ->
      Gaux.may rconfig ~f:(fun rc -> rc.Rconf.env <- entry_env#entries);
    end);
    ignore (entry_env#connect#replace_changed ~callback:begin fun is_replace ->
      Gaux.may rconfig ~f:(fun rc -> rc.Rconf.env_replace <- is_replace);
    end);
    combo_bc#set_active 0

  method set_targets () =
    model_bc#clear();
    let targets = List.filter (fun bc -> bc.Target.target_type = Target.Executable && bc.Target.files <> "") (target_list#get_targets()) in
    List.iter begin fun bc ->
      let row = model_bc#append () in
      model_bc#set ~row ~column:col_bc bc;
      model_bc#set ~row ~column:col_bc_pixbuf Icons.start_16;
      model_bc#set ~row ~column:col_name bc.Target.name;
    end targets;

  method set rc =
    rconfig <- Some rc;
    entry_name#set_text rc.Rconf.name;
    model_bc#foreach begin fun path row ->
      let bc = model_bc#get ~row ~column:col_bc in
      if bc.Target.id = rc.Rconf.target_id then begin
        combo_bc#set_active_iter (Some row);
        true
      end else false
    end;
    model_task#foreach begin fun path row ->
      let task = model_task#get ~row ~column:col_task in
      match model_task#get ~row ~column:col_task with
        | x when x = rc.Rconf.build_task ->
          combo_task#set_active_iter (Some row);
          true
        | _ -> false
    end;
    entry_args#set_entries rc.Rconf.args;
    entry_env#set_entries rc.Rconf.env;
    entry_env#set_replace rc.Rconf.env_replace;

  method entry_name = entry_name
  method combo_bc = combo_bc
  method combo_task = combo_task
end








