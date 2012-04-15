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
open Bconf
open GUtil

type c = ROOT | BCONF of Bconf.t | ETASK of Task.t

let (//) = Filename.concat

(** view *)
class view ~editor ~project ?packing () =
  (* "editor#project" is different from "project" when "project" is a new (i.e. you are in "New project" dialog window) *)
  let selection_changed = new selection_changed () in
  let removed = new removed () in
  let add_bconf = new add_bconf () in
  let add_etask = new add_etask () in
  let vbox = GPack.vbox ~spacing:5 ?packing () in
  let cols = new GTree.column_list in
  let col_data  = cols#add Gobject.Data.caml in
  let col_name  = cols#add Gobject.Data.string in
  let col_default = cols#add Gobject.Data.boolean in
  let model = GTree.tree_store cols in
  let renderer = GTree.cell_renderer_text [`XPAD 5] in
  let renderer_pixbuf = GTree.cell_renderer_pixbuf [] in
  let renderer_default = GTree.cell_renderer_toggle [`RADIO false; `ACTIVATABLE true; `WIDTH 50] in
  let vc = GTree.view_column ~title:"Name" () in
  let _ = vc#pack ~expand:false renderer_pixbuf in
  let _ = vc#pack ~expand:true renderer in
  let _ = vc#add_attribute renderer "text" col_name in
  let _ = vc#set_min_width 220 in
  let _ = vc#set_max_width 220 in
  let vc_default = GTree.view_column ~title:"Default" () in
  let _ = vc_default#pack ~expand:true renderer_default in
  let _ = vc_default#add_attribute renderer_default "active" col_default in
  let _ = vc_default#set_sizing `FIXED in
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GTree.view ~model:model ~headers_visible:true ~reorderable:false ~width:285 (*~height:385*) ~packing:sw#add () in
  let _ = view#misc#set_property "enable-tree-lines" (`BOOL true) in
  let _ = view#append_column vc in
  let _ = view#append_column vc_default in
  let _ = vc_default#set_visible true in
  let _ = view#selection#set_mode `SINGLE in
  let tooltips = GData.tooltips () in
  (* Buttons *)
  let bbox = GPack.button_box `HORIZONTAL ~layout:`SPREAD ~child_width:25 ~packing:vbox#pack () in
  let b_add = GButton.button ~packing:bbox#add () in
  let _ = tooltips#set_tip ~text:"Create a new build configuration" b_add#coerce in
  let _ = b_add#set_image (Icons.create Icons.bconf_24)#coerce in
  let b_etask = GButton.button ~packing:bbox#add () in
  let _ = tooltips#set_tip ~text:"Add an external build task" b_etask#coerce in
  let _ = b_etask#set_image (Icons.create Icons.etask_24)#coerce in
  let b_remove = GButton.button ~packing:bbox#add () in
  let _ = tooltips#set_tip ~text:"Delete selected items" b_remove#coerce in
  let _ = b_remove#set_image (GMisc.image ~stock:`DELETE ~icon_size:`BUTTON ())#coerce in
  let b_clean = GButton.button ~packing:bbox#add () in
  let _ = tooltips#set_tip ~text:"Clean" b_clean#coerce in
  let _ = b_clean#set_image (Icons.create Icons.clear_build_24)#coerce in
  let b_compile = GButton.button ~packing:bbox#add () in
  let _ = tooltips#set_tip ~text:"Build" b_compile#coerce in
  let _ = b_compile#set_image (Icons.create Icons.build_24)#coerce in
  let b_run = GButton.button ~packing:bbox#add () in
  let _ = tooltips#set_tip ~text:"Run external task/Install library" b_run#coerce in
  let _ = b_run#set_image (Icons.create Icons.start_16)#coerce in
object (self)
  inherit GObj.widget vbox#as_widget

  method model = model
  method view = view
  method column_name = col_name
  method has_errors bconf = bconf.Bconf.files = "";

  method button_add = b_add

  method get path =
    let row = model#get_iter path in
    model#get ~row ~column:col_data

  method private to_list () =
    let lst = ref [] in
    model#foreach begin fun path row ->
      lst := (model#get ~row ~column:col_data) :: !lst;
      false
    end;
    List.rev !lst

  method get_bconfigs () = Miscellanea.Xlist.filter_map (function BCONF x -> Some x | _ -> None) (self#to_list())

  method length = List.length (self#to_list())

  method current_path () =
    match List.rev view#selection#get_selected_rows with
      | path :: _ -> Some path
      | [] -> None

  method private current () =
    let path = self#current_path () in
    begin
      match path with None -> None | Some path ->
        (match self#get path with
          | BCONF bc -> Some (path, bc)
          | _ -> None)
    end

  method append bcs =
(*    let row = model#append() in
    model#set ~row ~column:col_data ROOT;
    model#set ~row ~column:col_name "Build Configurations";
    model#set ~row ~column:col_default false;*)
    List.iter begin fun bconf ->
      GtkThread2.sync begin fun () ->
        let bconf = {bconf with id = bconf.id} in
        let row = model#append (*~parent:row*) () in
        model#set ~row ~column:col_data (BCONF bconf);
        model#set ~row ~column:col_name bconf.name;
        model#set ~row ~column:col_default bconf.default;
        List.iter begin fun task ->
          GtkThread2.sync begin fun () ->
            ignore (self#append_task ~parent:row ~task)
          end ()
        end bconf.Bconf.external_tasks;
        view#selection#select_iter row
      end ()
    end bcs;
    view#expand_all()

  method append_task ~parent ~task =
    let row = model#append ~parent () in
    model#set ~row ~column:col_data (ETASK task);
    model#set ~row ~column:col_name task.Task.et_name;
    model#set ~row ~column:col_default false;
    view#expand_row (model#get_path parent);
    row

  method select_default_configuration () =
    view#selection#unselect_all();
    model#foreach begin fun path row ->
      let default_conf = model#get ~row ~column:col_default in
      if default_conf then begin
        view#selection#select_path path;
        true
      end else false
    end;

  method reset () =
    let current_selection =
      match view#selection#get_selected_rows with
        | [] -> None
        | path :: _ -> Some path
    in
    model#clear();
    self#append project.Project.build;
    match current_selection with
      | Some path -> view#selection#select_path path
      | _ -> self#select_default_configuration()

  initializer
    self#reset();
    ignore (view#selection#connect#changed ~callback:begin fun () ->
      if List.length view#selection#get_selected_rows = 0 then begin
        selection_changed#call None;
      end else begin
        match view#selection#get_selected_rows with
          | path :: _ ->
            begin
              match self#get path with
                | BCONF bc ->
                  b_clean#misc#set_sensitive true;
                  b_compile#misc#set_sensitive true;
                  b_etask#misc#set_sensitive true;
                  b_run#misc#set_sensitive (bc.Bconf.outkind <> Executable);
                | _ ->
                  b_clean#misc#set_sensitive false;
                  b_compile#misc#set_sensitive false;
                  b_etask#misc#set_sensitive true;
                  b_run#misc#set_sensitive true;
            end;
            selection_changed#call (Some path);
          | [] -> assert false
      end
    end);
    (* b_add#connect#clicked *)
    let _ = b_add#connect#clicked ~callback:begin fun () ->
      let bconfigs = Miscellanea.Xlist.filter_map (function BCONF x -> Some x | _ -> None) (self#to_list()) in
      let id = (List.fold_left (fun acc t -> max acc t.id) (-1) bconfigs) + 1 in
      let name = sprintf "New Build Configuration %d" id in
      self#append [Bconf.create ~name ~id];
      add_bconf#call();
    end in
    (*b_clean#connect#clicked*)
    let _ = b_clean#connect#clicked ~callback:begin fun () ->
      Gaux.may (self#current()) ~f:(fun (_, bconf) -> Bconf_console.exec ~editor `CLEAN bconf)
    end in
    (* b_etask#connect#clicked *)
    let _ = b_etask#connect#clicked ~callback:begin fun () ->
      let rec f path =
        let task = Task.create ~name:"External Build Task" ~env:[] ~dir:"" ~cmd:"" ~args:[] () in
        let parent = model#get_iter path in
        match self#get path with
          | BCONF bconf ->
            let row = self#append_task ~parent ~task in
            view#set_cursor (model#get_path row) vc;
            bconf.Bconf.external_tasks <- bconf.Bconf.external_tasks @ [task];
            add_etask#call();
          | (*ETASK*) _ ->
            if GTree.Path.up path then (f path)
      in
      match view#selection#get_selected_rows with
        | path :: [] -> f path
        | _ -> ()
    end in
    (*b_compile#connect#clicked*)
    let _ = b_compile#connect#clicked ~callback:begin fun () ->
      Gaux.may (self#current()) ~f:begin fun (_, bconf) ->
        Bconf_console.exec ~editor `COMPILE bconf;
      end
    end in
    (*b_run#connect#clicked*)
    let _ = b_run#connect#clicked ~callback:begin fun () ->
      Gaux.may (self#current_path ()) ~f:begin fun path ->
        match self#get path with
          | BCONF bconf -> Bconf_console.exec ~editor `INSTALL_LIBRARY bconf
          | ETASK etask -> Bconf_console.exec_sync ~editor [`OTHER, etask]
          | _ -> ()
      end
    end in
    (* b_remove#connect#clicked *)
    let _ = b_remove#connect#clicked ~callback:begin fun () ->
      try
        let last_path_index = (GTree.Path.get_indices (List.hd view#selection#get_selected_rows)).(0) in
        let paths = view#selection#get_selected_rows in
        List.iter begin fun path ->
          match self#get path with
            | ETASK task ->
              let parent = GTree.Path.copy path in
              if GTree.Path.up parent then begin
                match self#get parent with
                  | BCONF bc ->
                    bc.Bconf.external_tasks <- List.filter ((<>) task) bc.Bconf.external_tasks;
                  | _ -> assert false
              end
            | _ -> ()
        end paths;
        let data_removed = List.map self#get paths in
        let rows = List.map model#get_iter paths in
        List.iter (fun row -> ignore (model#remove row)) rows;
        let bconfigs = self#to_list() in
        let index = min last_path_index (List.length bconfigs - 1) in
        view#selection#select_path (GTree.Path.create [index]);
        removed#call data_removed;
      with Failure "hd" -> ()
    end in
    (* Default check toggled *)
    let _ = renderer_default#connect#toggled ~callback:begin fun row ->
      let row = model#get_iter row in
      let active = model#get ~row ~column:col_default in
      if not active then begin
        model#foreach begin fun _ row ->
          (model#set ~row ~column:col_default false);
          begin
            match model#get ~row ~column:col_data with
              | BCONF bc -> bc.default <- false
              | _ -> ()
          end;
          false
        end;
        model#set ~row ~column:col_default true;
        match model#get ~row ~column:col_data with
          | BCONF bc -> bc.default <- true
          | _ -> ()
      end
    end in
    (* Description (byt, opt) *)
    vc#set_cell_data_func renderer begin fun model row ->
      try
        begin
          match model#get ~row ~column:col_data with
            | ROOT ->
              renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.bconf_16; `XALIGN 0.0];
            | BCONF bconf ->
              renderer_pixbuf#set_properties [
                `VISIBLE (self#has_errors bconf);
                `PIXBUF ((GMisc.image ())#misc#render_icon ~size:`MENU `DIALOG_WARNING);
                `XALIGN 0.0];
              let name = model#get ~row ~column:col_name in
              let descr = if bconf.byt then ["Bytecode"] else [] in
              let descr = descr @ (if bconf.opt then ["Native-code"] else []) in
              let descr = String.concat ", " descr in
              let descr = (string_of_outkind bconf.outkind) ^ " &#8226; " ^ descr in
              renderer#set_properties [`MARKUP (sprintf
                "<b>%s</b>\n<span weight='light' size='smaller' style='italic'>%s</span>" name descr)];
              if bconf.outkind = Executable then begin
                renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.start_16; `XALIGN 0.0]
              end else if bconf.outkind = Plugin then begin
                renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.plugin; `XALIGN 0.0]
              end else begin
                renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.library; `XALIGN 0.0]
              end
            | ETASK _ ->
              renderer_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.etask_16; `XALIGN 0.0]
          end
      with Not_found -> ()
    end;
    (* Show hide column "default" *)
    vc_default#set_cell_data_func renderer_default begin fun model row ->
      let id = model#get ~row ~column:col_data in
      renderer_default#set_properties [`VISIBLE (match id with BCONF _ -> true | _ -> false)]
    end;

  method connect = new signals ~selection_changed ~removed ~add_bconf ~add_etask
end

and selection_changed () = object (self) inherit [Gtk.tree_path option] signal () as super end
and removed () = object (self) inherit [c list] signal () as super end
and add_bconf () = object (self) inherit [unit] signal () as super end
and add_etask () = object (self) inherit [unit] signal () as super end
and signals ~selection_changed ~removed ~add_bconf ~add_etask =
object (self)
  inherit ml_signals [selection_changed#disconnect; removed#disconnect; add_bconf#disconnect; add_etask#disconnect]
  method selection_changed = selection_changed#connect ~after
  method removed = removed#connect ~after
  method add_bconf = add_bconf#connect ~after
  method add_etask = add_etask#connect ~after
end








