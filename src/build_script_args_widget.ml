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


open Project
open Bconf
open Task
open Printf

let cols_type  = new GTree.column_list
let col_type   = cols_type#add Gobject.Data.string
let model_type = GTree.list_store cols_type
let _ =
  let column = col_type in
  let row = model_type#append () in model_type#set ~row ~column "Flag.Set";
  let row = model_type#append () in model_type#set ~row ~column "Flag.Clear";
  let row = model_type#append () in model_type#set ~row ~column "Bool";
  let row = model_type#append () in model_type#set ~row ~column "String";
;;

let cols_pass  = new GTree.column_list
let col_pass   = cols_pass#add Gobject.Data.string
let model_pass = GTree.list_store cols_pass
let _ =
  let column = col_pass in
  let row = model_pass#append () in model_pass#set ~row ~column "-key";
  let row = model_pass#append () in model_pass#set ~row ~column "value";
  let row = model_pass#append () in model_pass#set ~row ~column "-key value";
;;

let cols_bool  = new GTree.column_list
let col_bool   = cols_bool#add Gobject.Data.string
let model_bool = GTree.list_store cols_bool
let _ =
  let column = col_bool in
  let row = model_bool#append () in model_bool#set ~row ~column "true";
  let row = model_bool#append () in model_bool#set ~row ~column "false";
;;

class widget ~project ?packing () =
  let hbox            = GPack.hbox ~spacing:5 ?packing () in
  (* Model for external tasks *)
  let cols_et         = new GTree.column_list in
  let col_et_name     = cols_et#add Gobject.Data.string in
  let col_et_args     = cols_et#add Gobject.Data.caml in
  let model_et        = GTree.list_store cols_et in
  let _               =
    List.iter begin fun bc ->
      List.iter begin fun et ->
        let row             = model_et#append () in
        kprintf (model_et#set ~row ~column:col_et_name) "%s\n%s" bc.name et.et_name;
        model_et#set ~row ~column:col_et_args et.et_args;
      end bc.external_tasks
    end project.build
  in
  (* Model for task arguments *)
  let cols_arg        = new GTree.column_list in
  let col_arg         = cols_arg#add Gobject.Data.string in
  let model_arg       = GTree.list_store cols_arg in
  (*  *)
  let sw              = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add () in
  let cols            = new GTree.column_list in
  let col_opt_type    = cols#add Gobject.Data.string in
  let col_opt_name    = cols#add Gobject.Data.string in
  let col_opt_doc     = cols#add Gobject.Data.string in
  let col_opt_et_name = cols#add Gobject.Data.string in
  let col_opt_arg     = cols#add Gobject.Data.string in
  let col_opt_pass    = cols#add Gobject.Data.string in
  let col_opt_def_check = cols#add Gobject.Data.boolean in
  let col_opt_def_value = cols#add Gobject.Data.string in
  let model           = GTree.list_store cols in
  let rend_type       = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_opt_type; `MODEL (Some model_type#coerce)] in
  let rend_name       = GTree.cell_renderer_text [`EDITABLE true] in
  let rend_doc        = GTree.cell_renderer_text [`EDITABLE true] in
  let rend_et_name    = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_et_name; `MODEL (Some model_et#coerce)] in
  let rend_args       = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_arg; `MODEL (Some model_arg#coerce)] in
  let rend_pass       = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_arg; `MODEL (Some model_pass#coerce)] in
  let rend_def_check  = GTree.cell_renderer_toggle [`ACTIVATABLE true] in
  let rend_def_value  = GTree.cell_renderer_text [`EDITABLE true] in
  let rend_def_bool   = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_bool; `MODEL (Some model_bool#coerce)] in
  let vc_opt_type     = GTree.view_column ~renderer:(rend_type, ["text", col_opt_type]) ~title:"Type" () in
  let vc_opt_name     = GTree.view_column ~renderer:(rend_name, ["text", col_opt_name]) ~title:"Key" () in
  let vc_opt_doc      = GTree.view_column ~renderer:(rend_doc, ["text", col_opt_doc]) ~title:"Description" () in
  let vc_opt_def      = GTree.view_column ~title:"Default" () in
  let _               = vc_opt_def#pack ~expand:false rend_def_check in
  let _               = vc_opt_def#pack ~expand:true rend_def_value in
  let _               = vc_opt_def#pack ~expand:true rend_def_bool in
  let _               = vc_opt_def#add_attribute rend_def_check "active" col_opt_def_check in
  let _               = vc_opt_def#add_attribute rend_def_value "text" col_opt_def_value in
  let _               = vc_opt_def#add_attribute rend_def_bool "text" col_opt_def_value in
  let vc_opt_et_name  = GTree.view_column ~renderer:(rend_et_name, ["text", col_opt_et_name]) ~title:"For task..." () in
  let vc_opt_arg      = GTree.view_column ~renderer:(rend_args, ["text", col_opt_arg]) ~title:"Add/Replace" () in
  let vc_opt_pass     = GTree.view_column ~renderer:(rend_pass, ["text", col_opt_pass]) ~title:"Pass as..." () in
  let view            = GTree.view ~model ~headers_visible:true ~reorderable:true ~enable_search:false ~packing:sw#add () in
  let _               = view#selection#set_mode `SINGLE in
  let _               = view#set_headers_clickable true in
  let _               = view#misc#set_property "enable-grid-lines" (`INT 2) in
  let _               = view#append_column vc_opt_type in
  let _               = view#append_column vc_opt_name in
  let _               = view#append_column vc_opt_doc in
  let _               = view#append_column vc_opt_def in
  let _               = view#append_column vc_opt_et_name in
  let _               = view#append_column vc_opt_arg in
  let _               = view#append_column vc_opt_pass in
  let _               = vc_opt_type#set_min_width 75 in
  let _               = vc_opt_name#set_min_width 100 in
  let _               = vc_opt_def#set_min_width 100 in
  let _               = vc_opt_doc#set_min_width 250 in
  let _               = vc_opt_name#set_resizable true in
  let _               = vc_opt_doc#set_resizable true in
  let _               = vc_opt_def#set_resizable true in
  let _               = vc_opt_et_name#set_resizable true in
  let _               = vc_opt_arg#set_resizable true in
  let _               = vc_opt_pass#set_resizable true in
  (* Button Box *)
  let bbox            = GPack.button_box `VERTICAL ~layout:`START ~spacing:5 ~packing:hbox#pack () in
  let button_add      = GButton.button ~stock:`ADD ~packing:bbox#pack () in
  let _               = button_add#set_focus_on_click false in
  let _               = button_add#set_image (GMisc.image ~icon_size:`MENU ~stock:`ADD ())#coerce in
  let button_remove   = GButton.button ~stock:`REMOVE ~packing:bbox#pack () in
  let _               = button_remove#set_focus_on_click false in
  let _               = button_remove#set_image (GMisc.image ~icon_size:`MENU ~stock:`REMOVE ())#coerce in
object (self)
  inherit GObj.widget hbox#as_widget

  initializer
    ignore (button_add#connect#clicked ~callback:self#add);
    ignore (button_remove#connect#clicked ~callback:self#remove);
    ignore (rend_type#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model#set ~row ~column:col_opt_type text;
      self#set_editable_pass row;
      Gmisclib.Idle.add view#misc#grab_focus
    end);
    ignore (rend_name#connect#edited ~callback:begin fun path text ->
      model#set ~row:(model#get_iter path) ~column:col_opt_name text;
    end);
    ignore (rend_doc#connect#edited ~callback:begin fun path text ->
      model#set ~row:(model#get_iter path) ~column:col_opt_doc text;
    end);
    ignore (rend_et_name#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model#set ~row ~column:col_opt_arg "<ADD>";
      model#set ~row ~column:col_opt_et_name text;
      self#set_editable_pass row;
      Gmisclib.Idle.add view#misc#grab_focus
    end);
    ignore (rend_args#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model_arg#clear();
      model#set ~row ~column:col_opt_arg text;
      self#set_editable_pass row;
      Gmisclib.Idle.add view#misc#grab_focus;
    end);
    ignore (rend_pass#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model#set ~row ~column:col_opt_pass text;
      Gmisclib.Idle.add view#misc#grab_focus
    end);
    ignore (view#selection#connect#after#changed ~callback:self#fill_model_arg);
    ignore (view#connect#cursor_changed ~callback:self#fill_model_arg);

  method private set_editable_pass row = ()

  method private cell_data_func row =
    let is_arg_add = (model#get ~row ~column:col_opt_arg) = "<ADD>" in
    let is_bool = (model#get ~row ~column:col_opt_type) = "Bool" in
    let arg_type = model#get ~row ~column:col_opt_type in
    let is_flag = Str.first_chars arg_type 4 = "Flag" in
    if is_flag then (model#set ~row ~column:col_opt_pass "-key")
    else if is_arg_add then (model#set ~row ~column:col_opt_pass "-key value");
    rend_pass#set_properties [`EDITABLE (not is_flag && not is_arg_add)];
    rend_def_value#set_properties [`EDITABLE (not is_flag && not is_bool); `VISIBLE (not is_flag && not is_bool)];
    rend_def_bool#set_properties [`EDITABLE is_bool; `VISIBLE is_bool];
    rend_def_check#set_properties [`ACTIVATABLE is_flag; `VISIBLE is_flag];

  method private fill_model_arg' args =
    let row = model_arg#append () in
    model_arg#set ~row ~column:col_arg "<ADD>";
    List.iter begin fun (enabled, arg) ->
      if enabled then
        let row = model_arg#append () in
        model_arg#set ~row ~column:col_arg arg
    end args

  method private fill_model_arg () =
    model_arg#clear();
    match view#selection#get_selected_rows with
      | path :: [] ->
        let row = model#get_iter path in
        let et_name = model#get ~row ~column:col_opt_et_name in
        let args = ref None in
        model_et#foreach begin fun path row ->
          let name = model_et#get ~row ~column:col_et_name in
          if name = et_name then (args := Some (model_et#get ~row ~column:col_et_args); true)
          else false
        end;
        Gaux.may !args ~f:self#fill_model_arg';
        self#set_editable_pass row;
      | _ -> model_arg#clear();

  method add () =
    let row = model#append () in
    let default_type = model_type#get
      ~row:(model_type#get_iter (GTree.Path.create [0]))
      ~column:col_type
    in
    model#set ~row ~column:col_opt_type default_type;
    model#set ~row ~column:col_opt_name "";
    model#set ~row ~column:col_opt_doc "";
    model#set ~row ~column:col_opt_def_check false;
    model#set ~row ~column:col_opt_def_value "";
    view#set_cursor (model#get_path row) vc_opt_type;
    self#set_editable_pass row;
    view#misc#grab_focus()

  method remove () = ()

end


