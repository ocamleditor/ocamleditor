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


open Prj
open Target
open Task
open Printf
open Build_script
open Build_script_args

let cols_type  = new GTree.column_list
let col_type   = cols_type#add Gobject.Data.string
let model_type = GTree.list_store cols_type
let _ =
  let column = col_type in
  let g x = let row = model_type#append () in model_type#set ~row ~column x in
  List.iter (fun x -> g (string_of_type x)) [Flag; Bool; String]
;;

let cols_pass  = new GTree.column_list
let col_pass   = cols_pass#add Gobject.Data.string
let model_pass = GTree.list_store cols_pass
let model_pass_fill values =
  let column = col_pass in
  model_pass#clear();
  let g x = let row = model_pass#append () in model_pass#set ~row ~column x in
  List.iter (fun x -> g (string_of_pass x)) values;;
let _ = model_pass_fill [`key_value; `value];;

let cols_bool  = new GTree.column_list
let col_bool   = cols_bool#add Gobject.Data.string
let model_bool = GTree.list_store cols_bool
let _ =
  let column = col_bool in
  let row = model_bool#append () in model_bool#set ~row ~column "true";
  let row = model_bool#append () in model_bool#set ~row ~column "false";
;;

let taskname_of_task = function
  | Some (bc, et) -> sprintf "%s\n%s" bc.name et.et_name
  | _ -> ""

class widget ~project ?packing () =
  let hbox              = GPack.hbox ~spacing:5 ?packing () in
  (* Model for external tasks *)
  let cols_et           = new GTree.column_list in
  let col_et_name       = cols_et#add Gobject.Data.string in
  let col_et            = cols_et#add Gobject.Data.caml in
  let model_et          = GTree.list_store cols_et in
  let _                 =
    List.iter begin fun bc ->
      List.iter begin fun et ->
        let row               = model_et#append () in
        let bc_et             = bc, et in
        model_et#set ~row ~column:col_et_name (taskname_of_task (Some bc_et));
        model_et#set ~row ~column:col_et bc_et;
      end bc.external_tasks
    end project.targets
  in
  (* Model for task arguments *)
  let cols_arg          = new GTree.column_list in
  let col_arg           = cols_arg#add Gobject.Data.string in
  let model_arg         = GTree.list_store cols_arg in
  (*  *)
  let sw                = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add () in
  let cols              = new GTree.column_list in
  let col_opt_type      = cols#add Gobject.Data.string in
  let col_opt_key       = cols#add Gobject.Data.string in
  let col_opt_doc       = cols#add Gobject.Data.string in
  let col_opt_et_name   = cols#add Gobject.Data.string in
  let col_opt_arg       = cols#add Gobject.Data.string in
  let col_opt_pass      = cols#add Gobject.Data.string in
  let col_opt_def_flag  = cols#add Gobject.Data.boolean in
  let col_opt_def_value = cols#add Gobject.Data.string in
  let model             = GTree.list_store cols in
  let rend_type         = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_opt_type; `MODEL (Some model_type#coerce)] in
  let rend_name         = GTree.cell_renderer_text [`EDITABLE true] in
  let rend_doc          = GTree.cell_renderer_text [`EDITABLE true] in
  let rend_et_name      = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_et_name; `MODEL (Some model_et#coerce)] in
  let rend_args         = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_arg; `MODEL (Some model_arg#coerce)] in
  let rend_pass_flag    = GTree.cell_renderer_text [`EDITABLE false] in
  let rend_pass         = GTree.cell_renderer_combo [`EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_arg; `MODEL (Some model_pass#coerce)] in
  let rend_def_flag     = GTree.cell_renderer_toggle [`ACTIVATABLE true] in
  let rend_def_string   = GTree.cell_renderer_text [`XALIGN 0.5; `EDITABLE true] in
  let rend_def_bool     = GTree.cell_renderer_combo [`XALIGN 0.5; `EDITABLE true; `HAS_ENTRY false; `TEXT_COLUMN col_bool; `MODEL (Some model_bool#coerce)] in
  let vc_opt_type       = GTree.view_column ~renderer:(rend_type, ["text", col_opt_type]) ~title:"Type" () in
  let vc_opt_name       = GTree.view_column ~renderer:(rend_name, ["text", col_opt_key]) ~title:"Key" () in
  let vc_opt_doc        = GTree.view_column ~renderer:(rend_doc, ["text", col_opt_doc]) ~title:"Description" () in
  let vc_opt_def        = GTree.view_column ~title:"Default value" () in
  let _                 = vc_opt_def#pack ~expand:true rend_def_flag in
  let _                 = vc_opt_def#pack ~expand:true rend_def_string in
  let _                 = vc_opt_def#pack ~expand:true rend_def_bool in
  let _                 = vc_opt_def#add_attribute rend_def_flag "active" col_opt_def_flag in
  let _                 = vc_opt_def#add_attribute rend_def_string "text" col_opt_def_value in
  let _                 = vc_opt_def#add_attribute rend_def_bool "text" col_opt_def_value in
  let vc_opt_et_name    = GTree.view_column ~renderer:(rend_et_name, ["text", col_opt_et_name]) ~title:"For task..." () in
  let vc_opt_arg        = GTree.view_column ~renderer:(rend_args, ["text", col_opt_arg]) ~title:"Add/Replace" () in
  let vc_opt_pass       = GTree.view_column (*~renderer:(rend_pass, ["text", col_opt_pass])*) ~title:"Pass as..." () in
  let _                 = vc_opt_pass#pack ~expand:true rend_pass_flag in
  let _                 = vc_opt_pass#pack ~expand:true rend_pass in
  let _                 = vc_opt_pass#add_attribute rend_pass_flag "text" col_opt_pass in
  let _                 = vc_opt_pass#add_attribute rend_pass "text" col_opt_pass in
  let width             = min 1000 ((Gdk.Screen.width ()) * 13 / 20) in
  let view              = GTree.view ~model ~headers_visible:true ~reorderable:true ~width ~height:300 ~enable_search:false ~packing:sw#add () in
  let _                 = view#selection#set_mode `SINGLE in
  let _                 = view#set_headers_clickable true in
  let _                 = view#misc#set_property "enable-grid-lines" (`INT 2) in
  let _                 = view#append_column vc_opt_type in
  let _                 = view#append_column vc_opt_name in
  let _                 = view#append_column vc_opt_doc in
  let _                 = view#append_column vc_opt_def in
  let _                 = view#append_column vc_opt_et_name in
  let _                 = view#append_column vc_opt_arg in
  let _                 = view#append_column vc_opt_pass in
  let _                 = vc_opt_type#set_min_width 75 in
  let _                 = vc_opt_name#set_min_width 100 in
  let _                 = vc_opt_def#set_min_width 100 in
  let _                 = vc_opt_doc#set_min_width 250 in
  let _                 = vc_opt_name#set_resizable true in
  let _                 = vc_opt_doc#set_resizable true in
  let _                 = vc_opt_def#set_resizable true in
  let _                 = vc_opt_et_name#set_resizable true in
  let _                 = vc_opt_arg#set_resizable true in
  let _                 = vc_opt_pass#set_resizable true in
  (* Button Box *)
  let bbox              = GPack.button_box `VERTICAL ~layout:`START ~spacing:5 ~packing:hbox#pack () in
  let button_add        = GButton.button ~stock:`ADD ~packing:bbox#pack () in
  let _                 = button_add#set_focus_on_click false in
  let _                 = button_add#set_image (GMisc.image ~icon_size:`MENU ~stock:`ADD ())#coerce in
  let button_remove     = GButton.button ~stock:`REMOVE ~packing:bbox#pack () in
  let _                 = button_remove#set_focus_on_click false in
  let _                 = button_remove#set_image (GMisc.image ~icon_size:`MENU ~stock:`REMOVE ())#coerce in
object (self)
  inherit GObj.widget hbox#as_widget

  val mutable n_rows = 0

  initializer
    ignore (button_add#connect#clicked ~callback:self#add);
    ignore (button_remove#connect#clicked ~callback:self#remove);
    ignore (rend_type#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model#set ~row ~column:col_opt_type text;
      self#update_pass row;
      let is_flag = self#is_flag row in
      let is_bool = self#is_bool row in
      let default_value = model#get ~row ~column:col_opt_def_value in

      if is_bool && not (List.mem default_value ["true"; "false"])
      then model#set ~row ~column:col_opt_def_value "true";

      if is_flag && default_value <> ""
      then model#set ~row ~column:col_opt_def_value "";

      if is_flag then model#set ~row ~column:col_opt_def_flag false;

      Gmisclib.Idle.add view#misc#grab_focus
    end);
    ignore (rend_name#connect#edited ~callback:begin fun path text ->
      model#set ~row:(model#get_iter path) ~column:col_opt_key text;
    end);
    ignore (rend_doc#connect#edited ~callback:begin fun path text ->
      model#set ~row:(model#get_iter path) ~column:col_opt_doc text;
    end);
    ignore (rend_et_name#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model#set ~row ~column:col_opt_arg string_of_add ;
      model#set ~row ~column:col_opt_et_name text;
      self#update_pass row;
      Gmisclib.Idle.add view#misc#grab_focus
    end);
    ignore (rend_args#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model_arg#clear();
      model#set ~row ~column:col_opt_arg text;
      self#update_pass row;
      Gmisclib.Idle.add view#misc#grab_focus;
    end);
    ignore (rend_pass#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model#set ~row ~column:col_opt_pass text;
      Gmisclib.Idle.add view#misc#grab_focus
    end);
    ignore (rend_def_flag#connect#toggled ~callback:begin fun path ->
      let row = model#get_iter path in
      model#set ~row ~column:col_opt_def_flag (not (model#get ~row ~column:col_opt_def_flag));
    end);
    ignore (rend_def_bool#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model#set ~row ~column:col_opt_def_value text;
    end);
    ignore (rend_def_string#connect#edited ~callback:begin fun path text ->
      let row = model#get_iter path in
      model#set ~row ~column:col_opt_def_value text;
    end);
    (*  *)
    ignore (view#selection#connect#after#changed ~callback:self#fill_model_arg);
    ignore (view#connect#cursor_changed ~callback:self#fill_model_arg);
    (* Cell Data Func *)
    vc_opt_def#set_cell_data_func rend_def_flag begin fun _ row ->
      let is_flag = self#is_flag row in
      let is_bool = self#is_bool row in
      rend_def_flag#set_properties [`ACTIVATABLE is_flag; `VISIBLE is_flag];
      rend_def_string#set_properties [`EDITABLE (not is_flag && not is_bool); `VISIBLE (not is_flag && not is_bool)];
      rend_def_bool#set_properties [`EDITABLE is_bool; `VISIBLE is_bool];
    end;
    vc_opt_pass#set_cell_data_func rend_pass_flag begin fun _ row ->
      let is_flag = self#is_flag row in
      rend_pass_flag#set_properties [`VISIBLE is_flag];
      rend_pass#set_properties [`VISIBLE (not is_flag); `EDITABLE (not is_flag); `MODE (if is_flag then `INERT else `EDITABLE)];
    end;
    self#set project.build_script.bs_args;

  method private is_flag row = model#get ~row ~column:col_opt_type = "Flag"

  method private is_bool row = (model#get ~row ~column:col_opt_type) = (string_of_type Bool)

  method private is_arg_add row = (model#get ~row ~column:col_opt_arg) = string_of_add

  method private update_pass row =
    let is_flag = self#is_flag row in
    let is_arg_add = self#is_arg_add row in
    if is_flag then begin
      model#set ~row ~column:col_opt_pass (string_of_pass `key)
    end else begin
      if model#get ~row ~column:col_opt_pass = (string_of_pass `key) then model#set ~row ~column:col_opt_pass "";
    end;
    (*if is_arg_add then (model#set ~row ~column:col_opt_pass (string_of_pass `key_value));*)

  method find_task_by_name et_name =
    let res = ref None in
    model_et#foreach begin fun path row ->
      let name = model_et#get ~row ~column:col_et_name in
      if name = et_name then (res := Some (model_et#get ~row ~column:col_et); true)
      else false
    end;
    !res

  method private fill_model_arg' args =
    let row = model_arg#append () in
    model_arg#set ~row ~column:col_arg string_of_add ;
    List.iter begin fun (enabled, arg) ->
      if enabled then
        let row = model_arg#append () in
        model_arg#set ~row ~column:col_arg arg;
        n_rows <- n_rows + 1;
    end args

  method private fill_model_arg () =
    model_arg#clear();
    match view#selection#get_selected_rows with
      | path :: [] ->
        let row = model#get_iter path in
        let et_name = model#get ~row ~column:col_opt_et_name in
        begin
          match self#find_task_by_name et_name with
            | Some (_, et) -> self#fill_model_arg' et.et_args
            | None -> self#fill_model_arg' []
        end;
      | _ -> model_arg#clear();

  method add () =
    let row = model#append () in
    n_rows <- n_rows + 1;
    let default_type = model_type#get
      ~row:(model_type#get_iter (GTree.Path.create [0]))
      ~column:col_type
    in
    model#set ~row ~column:col_opt_type default_type;
    model#set ~row ~column:col_opt_key "";
    model#set ~row ~column:col_opt_doc "";
    model#set ~row ~column:col_opt_def_value "";
    view#set_cursor (model#get_path row) vc_opt_type;
    view#misc#grab_focus();

  method remove () =
    let paths = view#selection#get_selected_rows in
    let remove_path path =
      let next =
        if GTree.Path.to_string path = GTree.Path.to_string (GTree.Path.create [n_rows - 1])
        then let next = GTree.Path.copy path in ignore ((GTree.Path.prev next)); next
        else path
      in
      ignore (model#remove (model#get_iter path));
      n_rows <- n_rows - 1;
      view#selection#select_path next;
      view#scroll_to_cell ~align:(0.5, 0.0) next vc_opt_type;
    in
    List.iter remove_path paths

  method set args =
    List.iter begin fun arg ->
      let row = model#append () in
      model#set ~row ~column:col_opt_type (string_of_type arg.bsa_type);
      model#set ~row ~column:col_opt_key arg.bsa_key;
      model#set ~row ~column:col_opt_doc arg.bsa_doc;
      begin
        match arg.bsa_default with
          | `flag x -> model#set ~row ~column:col_opt_def_flag x;
          | `bool x -> model#set ~row ~column:col_opt_def_value (string_of_bool x);
          | `string x -> model#set ~row ~column:col_opt_def_value x;
      end;
      model#set ~row ~column:col_opt_et_name (taskname_of_task arg.bsa_task);
      model#set ~row ~column:col_opt_arg begin
        match arg.bsa_mode with
          | `add -> string_of_add
          | `replace arg -> arg
      end;
      model#set ~row ~column:col_opt_pass (string_of_pass arg.bsa_pass);
    end args;

  method get () =
    let arguments = ref [] in
    model#foreach begin fun path row ->
      let bc_et = self#find_task_by_name (model#get ~row ~column:col_opt_et_name) in
      (*match self#find_task_by_name (model#get ~row ~column:col_opt_et_name) with
        | Some bc_et ->*)
          let bsa_type = type_of_string (model#get ~row ~column:col_opt_type) in
          let bsa_default =
            match bsa_type with
              | Flag -> `flag (model#get ~row ~column:col_opt_def_flag)
              | Bool -> `bool (bool_of_string (model#get ~row ~column:col_opt_def_value))
              | String -> `string (model#get ~row ~column:col_opt_def_value)
          in
          let bsa_mode =
            let arg = model#get ~row ~column:col_opt_arg in
            if arg = string_of_add then `add else (`replace arg)
          in
          arguments := {
            bsa_type    = bsa_type;
            bsa_key     = (model#get ~row ~column:col_opt_key);
            bsa_doc     = (model#get ~row ~column:col_opt_doc);
            bsa_default = bsa_default;
            bsa_task    = bc_et;
            bsa_mode    = bsa_mode;
            bsa_pass    = (pass_of_string (model#get ~row ~column:col_opt_pass))
          } :: !arguments;
          false
        (*| _-> false*)
    end;
    List.rev !arguments
end


