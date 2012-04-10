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


open Printf

(** create_entry *)
let create_entry ?label ~packing () =
  let vbox = GPack.vbox ~packing () in
  Gaux.may label ~f:(fun markup -> GMisc.label ~markup ~xalign:0.0 ~packing:vbox#pack ());
  GEdit.entry ~packing:vbox#pack ()

(** view *)
class view ~bconf_list ?packing () =
  let vbox = GPack.vbox ~spacing:8 ?packing () in
  let title = GMisc.label ~markup:"<big><b>Run Configuration</b></big>" ~xalign:0.0 ~packing:vbox#pack () in
  let entry_name = create_entry ~packing:vbox#pack () in
  (* Build configuration *)
  let box = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Build configuration" ~xalign:0.0 ~packing:box#add () in
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
  let col_rbt_pb = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf") in
  let col_rbt_descr = cols#add Gobject.Data.string in
  let col_rbt = cols#add Gobject.Data.caml in
  let model_rbt = GTree.list_store cols in
  let combo_rbt = GEdit.combo_box ~active:0 ~model:model_rbt ~packing:box#pack () in
  let rend_pb = GTree.cell_renderer_pixbuf [] in
  let rend = GTree.cell_renderer_text [`XPAD 5] in
  let _ = combo_rbt#pack rend_pb in
  let _ = combo_rbt#pack rend in
  let _ = combo_rbt#add_attribute rend_pb "pixbuf" col_rbt_pb in
  let _ = combo_rbt#add_attribute rend "markup" col_rbt_descr in
  (* Command Line Arguments *)
  let entry_args = create_entry ~label:"Command line arguments" ~packing:vbox#pack () in
  (* Environment *)
  let box = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~markup:"Environment (<small><tt>NAME=VALUE</tt></small>)" ~xalign:0.0 ~packing:box#add () in
  let entry_env = Entry_env.create ~packing:box#add () in

object (self)
  inherit GObj.widget vbox#as_widget
  val mutable rconfig = None

  method set_tasks () =
    match combo_bc#active_iter with
      | None -> ()
      | Some row ->
        let bc = model_bc#get ~row ~column:col_bc in
        let rbts = [
          `NONE, Icons.empty_16;
          `CLEAN, Icons.clear_build_16;
          `COMPILE, Icons.build_16;
          `REBUILD, Icons.empty_16
        ] @ (List.map (fun x -> (`ETASK x, Icons.etask_16)) bc.Bconf.external_tasks) in
        model_rbt#clear();
        List.iter begin fun (rbt, icon) ->
          let row = model_rbt#append () in
          model_rbt#set ~row ~column:col_rbt_pb icon;
          model_rbt#set ~row ~column:col_rbt_descr (Bconf.markup_of_rbt rbt);
          model_rbt#set ~row ~column:col_rbt rbt
        end rbts;
        Gaux.may rconfig ~f:(fun rc -> rc.Rconf.id_build <- bc.Bconf.id);

  initializer
    self#set_bconfigs();
    ignore (combo_bc#connect#changed ~callback:self#set_tasks);
    ignore (combo_rbt#connect#changed ~callback:begin fun () ->
      Gaux.may rconfig ~f:begin fun rc ->
        Gaux.may combo_rbt#active_iter ~f:begin fun row ->
          rc.Rconf.build_task <- model_rbt#get ~row ~column:col_rbt;
        end
      end;
    end);
    ignore (entry_args#connect#changed ~callback:begin fun () ->
      Gaux.may rconfig ~f:begin fun rc ->
        rc.Rconf.args <- entry_args#text
      end;
    end);
    ignore (entry_env#connect#changed ~callback:begin fun () ->
      Gaux.may rconfig ~f:begin fun rc ->
        rc.Rconf.env <- entry_env#entries
      end;
    end);
    ignore (entry_env#connect#replace_changed ~callback:begin fun is_replace ->
      Gaux.may rconfig ~f:begin fun rc ->
        rc.Rconf.env_replace <- is_replace
      end;
    end);
    combo_bc#set_active 0

  method set_bconfigs () =
    model_bc#clear();
    let bconfigs = List.filter (fun bc -> bc.Bconf.outkind = Bconf.Executable && bc.Bconf.files <> "") (bconf_list#get_bconfigs()) in
    List.iter begin fun bc ->
      let row = model_bc#append () in
      model_bc#set ~row ~column:col_bc bc;
      model_bc#set ~row ~column:col_bc_pixbuf Icons.start_16;
      model_bc#set ~row ~column:col_name bc.Bconf.name;
    end bconfigs;

  method set rc =
    rconfig <- Some rc;
    entry_name#set_text rc.Rconf.name;
    model_bc#foreach begin fun path row ->
      let bc = model_bc#get ~row ~column:col_bc in
      if bc.Bconf.id = rc.Rconf.id_build then begin
        combo_bc#set_active_iter (Some row);
        true
      end else false
    end;
    model_rbt#foreach begin fun path row ->
      let rbt = model_rbt#get ~row ~column:col_rbt in
      match model_rbt#get ~row ~column:col_rbt with
        | x when x = rc.Rconf.build_task ->
          combo_rbt#set_active_iter (Some row);
          true
        | _ -> false
    end;
    entry_args#set_text rc.Rconf.args;
    entry_env#set_entries rc.Rconf.env;
    entry_env#set_replace rc.Rconf.env_replace;

  method entry_name = entry_name
  method combo_bc = combo_bc
  method combo_rbt = combo_rbt
end








