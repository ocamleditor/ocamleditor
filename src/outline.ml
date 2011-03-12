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


open Printf
open Miscellanea
open Odoc_info

class widget ~project ~page ~tmp =
  let vbox            = GPack.vbox () in
  let toolbar         = GButton.toolbar ~packing:vbox#pack ~style:`ICONS ~show:false () in
  let _               = toolbar#set_icon_size `MENU in
  let button_sort     = GButton.toggle_tool_button ~homogeneous:true ~stock:`SORT_ASCENDING ~packing:toolbar#insert () in
  let button_types    = GButton.toggle_tool_button ~homogeneous:false ~label:"Types" ~packing:toolbar#insert () in
  (*  *)
  let source_filename = page#get_filename in
  let dump_filename   =
    match project.Project.in_source_path source_filename with
      | Some rel ->
        let tmp             = Project.path_tmp project in
        tmp // ((Filename.chop_extension rel) ^ ".outline")
      | _ -> Filename.temp_file "outline" ""
  in
  let includes        = Project.get_includes project in
  let sw              = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let cols            = new GTree.column_list in
  let col_markup      = cols#add Gobject.Data.string in
  let col_kind        = cols#add Gobject.Data.caml_option in
  let view            = GTree.view ~headers_visible:false (*~width:200*) ~packing:sw#add () in
  let renderer        = GTree.cell_renderer_text [`YPAD 0] in
  let renderer_pixbuf = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
  let vc              = GTree.view_column ~title:"" () in
  let _               = vc#pack ~expand:false renderer_pixbuf in
  let _               = vc#pack ~expand:true renderer in
  let _               = vc#add_attribute renderer "markup" col_markup in
  let _               = view#append_column vc in
  let _               = view#misc#set_property "enable-tree-lines" (`BOOL true) in
object (self)
  inherit GObj.widget vbox#as_widget
  val mutable locations = []
  val mutable tooltips = []
  val mutable current_model : GTree.tree_store option = None
  val mutable signal_selection_changed = None

  initializer
    self#parse ();
    (** Events *)
    let callback () =
      try
        let path = List.hd view#selection#get_selected_rows in
        let _, (offset, callback) = List.find (fun (rr, _) -> rr#path = path) locations in
        let where = page#buffer#get_iter (`OFFSET offset) in
        page#view#scroll_lazy where;
        page#buffer#place_cursor ~where;
        ignore (callback());
      with Not_found | Gpointer.Null | Failure "hd" -> ()
    in
    signal_selection_changed <- Some (view#selection#connect#after#changed ~callback);
    ignore (view#connect#after#row_activated ~callback:begin fun path _ ->
      page#view#misc#grab_focus();
    end);
    (** Tooltips *)
    view#misc#set_has_tooltip true;
    ignore (view#misc#connect#query_tooltip ~callback: begin fun ~x ~y ~kbd tooltip ->
      try
        begin
          match GtkTree.TreeView.Tooltip.get_context view#as_tree_view ~x ~y ~kbd with
            | (x, y, Some (model, path, row)) ->
              begin
                match view#get_path_at_pos ~x ~y with
                  | Some (tpath, vc, _, _) ->
                    let _, markup = List.find (fun (rr, _) -> rr#path = path) tooltips in
                    GtkBase.Tooltip.set_markup tooltip markup;
                    GtkTree.TreeView.Tooltip.set_row view#as_tree_view tooltip tpath;
                    true
                  | _ -> false
              end
            | _ -> false
        end
      with Not_found | Gpointer.Null -> false
    end);

  method find (model : GTree.tree_store) kind =
    let result = ref None in
    model#foreach begin fun path row ->
      match model#get ~row ~column:col_kind with
        | Some k when k = kind ->
          result := Some row;
          true
        | _ -> false
    end;
    !result

  method add_markers ~(kind : [`Warning | `Error | `All]) () =
    match current_model with
      | Some model ->
        (* Remove previous marks *)
        let old_folder_errors = Gaux.may_map (self#find model `Folder_errors)
          ~f:(model#get_path |- model#get_row_reference) in
        let old_folder_warnings = Gaux.may_map (self#find model `Folder_warnings)
          ~f:(model#get_path |- model#get_row_reference) in
        Gaux.may old_folder_errors ~f:(fun rr -> model#remove rr#iter);
        Gaux.may old_folder_warnings ~f:(fun rr -> model#remove rr#iter);
        (*  *)
        let markers = List.rev page#view#gutter.Gutter.markers in
        List.iter begin fun marker ->
          let row =
            match marker.Gutter.kind with
              | `Warning msg when kind = `All || kind = `Warning ->
                let parent = self#get_node_markers ~model ~kind:`Folder_warnings ~msg in
                let message = Miscellanea.replace_first ["Warning \\([0-9]+\\):", "\\1:"] msg in
                let row = model#append ?parent () in
                model#set ~row ~column:col_markup message;
                model#set ~row ~column:col_kind (Some `Warning);
                tooltips <- ((model#get_row_reference (model#get_path row)), msg) :: tooltips;
                Some row
              | `Error msg when kind = `All || kind = `Error ->
                let parent = self#get_node_markers ~model ~kind:`Folder_errors ~msg in
                let message = Miscellanea.replace_first ["Error: ", ""] msg in
                let row = model#append ?parent () in
                model#set ~row ~column:col_markup message;
                model#set ~row ~column:col_kind (Some `Errors);
                tooltips <- ((model#get_row_reference (model#get_path row)), msg) :: tooltips;
                Some row
              (*| `Bookmark num ->
                let row = model#append () in
                model#set ~row ~column:col_markup (string_of_int num);
                model#set ~row ~column:col_kind None;
                Some row*)
              | _ -> None
          in
          match row with
            | Some row ->
              let iter = page#buffer#get_iter (`MARK marker.Gutter.mark) in
              let callback =
                match marker.Gutter.callback with
                  | Some f -> (fun () -> f marker.Gutter.mark)
                  | None -> (fun () -> false)
              in
              locations <-
                ((model#get_row_reference (model#get_path row)), (iter#offset, callback)) :: locations
            | _ -> ()
        end markers;
        if kind = `Error then begin
          match self#find model `Folder_errors with
            | None -> ()
            | Some row -> view#scroll_to_cell ~align:(0., 0.) (model#get_path row) vc
        end
      | None -> ()

  method select (mark : Gtk.text_mark) =
    match current_model with
      | None -> ()
      | _ ->
        begin
          try
            let iter = page#buffer#get_iter_at_mark (`MARK mark) in
            let rr, _ = List.find begin fun (_, (offset, _)) ->
              offset <= iter#offset
            end locations in
            Gaux.may signal_selection_changed ~f:view#selection#misc#handler_block;
            view#set_cursor rr#path vc;
            Gaux.may signal_selection_changed ~f:view#selection#misc#handler_unblock;
          with Not_found -> ()
        end;

  method parse () =
    let cmd             = sprintf "%s -dump %s %s%s %s%s"
      (Ocaml_config.ocamldoc())
      (Quote.arg dump_filename)
      (" -I +threads")
      (*(if project.Project.thread then " -thread" else if project.Project.vmthread then " -vmthread" else "")*)
      (if includes = [] then "" else (" -I " ^ (String.concat " -I " (List.map Quote.arg includes))))
      (Quote.arg tmp (*source_filename*))
      (if Oe_config.ocamleditor_debug then "" else Miscellanea.redirect_stderr)
    in
    ignore begin
      Oebuild_util.exec ~echo:true ~join:false ~at_exit:begin fun () ->
        let module_list = Odoc_info.load_modules dump_filename in
        tooltips <- [];
        locations <- [];
        GtkThread2.async begin fun () ->
          let model = GTree.tree_store cols in
          vc#unset_cell_data_func renderer;
          self#append ~model module_list;
          current_model <- Some model;
          self#add_markers ~kind:`All ();
          view#set_model (Some (model :> GTree.model));
          locations <- List.sort (fun (_, (o1, _)) (_, (o2, _)) -> Pervasives.compare o2 o1) locations;
          (** Cell data func *)
          vc#set_cell_data_func renderer begin fun model row ->
            let kind = model#get ~row ~column:col_kind in
            let pixbuf =
              match kind with
                | Some `Exception -> Icons.exc
                | Some `Type -> Icons.typ
                | Some `Module -> Icons.module_impl
                | Some `Class -> Icons.classe
                | Some `Class_virtual -> Icons.class_virtual
                | Some `Class_type -> Icons.class_type
                | Some `Class_inherit -> Icons.class_inherit
                | Some `Function -> Icons.func
                | Some `Simple -> Icons.simple
                | Some `Attribute -> Icons.attribute
                | Some `Attribute_mutable -> Icons.attribute_mutable
                | Some `Attribute_mutable_virtual -> Icons.attribute_mutable_virtual
                | Some `Attribute_virtual -> Icons.attribute_virtual
                | Some `Method -> Icons.met
                | Some `Method_private -> Icons.met_private
                | Some `Method_virtual -> Icons.met_virtual
                | Some `Method_private_virtual -> Icons.met_private_virtual
                | Some `Type_abstract -> Icons.type_abstract
                | Some `Type_variant -> Icons.type_variant
                | Some `Type_record -> Icons.type_record
                | Some `Errors -> Icons.error_14
                | Some `Warning -> Icons.warning_14
                | Some `Folder_warnings -> Icons.folder_warning
                | Some `Folder_errors -> Icons.folder_error
                | Some `Dependencies -> Icons.none_14
                | None -> Icons.none_14
            in
            renderer_pixbuf#set_properties [ `VISIBLE (kind <> None); `PIXBUF pixbuf];
          end;
          (** Expanded and collapsed nodes *)
          view#expand_all ();
          Gaux.may (self#find model `Dependencies) ~f:(fun iter -> view#collapse_row (model#get_path iter));
          Gaux.may (self#find model `Folder_warnings) ~f:(fun iter -> view#collapse_row (model#get_path iter));
        end ();
      end cmd;
    end;

  method private set_location ~model row loc =
    let loc_filename, loc_pos =
      match loc.Odoc_types.loc_impl with Some (a, b) -> a, b | _ -> "", 0
    in
    locations <- ((model#get_row_reference (model#get_path row)), (loc_pos, (fun () -> false))) :: locations

  method private set_tooltip ~(model : GTree.tree_store) ~row ~name ~typ =
    model#set ~row ~column:col_markup (sprintf "%s <span color='#877033'>%s</span>"
      (Glib.Markup.escape_text name)
      (if typ = "" then "" else (sprintf ": %s"
        (Print_type.markup2 (Miscellanea.replace_all ~regexp:true ["\n", ""; " +", " "] typ)))));
    tooltips <- (model#get_row_reference (model#get_path row), (Print_type.markup3 typ)) :: tooltips;

  method private get_node_markers ~model ~kind ~msg =
    let name =
      match kind with
        | `Folder_warnings -> "Warnings"
        | `Folder_errors -> "Errors"
        | _ -> assert false
    in
    match self#find model kind with
      | None ->
        let row = model#prepend () in
        model#set ~row ~column:col_markup name;
        model#set ~row ~column:col_kind (Some kind);
        Some row
      | x -> x

  method private append ~model ?parent module_list =
    List.iter begin fun modu ->
      let get_relative = Name.get_relative modu.Module.m_name in
      (*(** Dependencies *)
      if modu.Module.m_top_deps <> [] then begin
        let row_dep = model#append ?parent () in
        model#set ~row:row_dep ~column:col_markup "Dependencies";
        model#set ~row:row_dep ~column:col_kind (Some `Dependencies);
        List.iter begin fun elem ->
          let row = model#append ~parent:row_dep () in
          model#set ~row ~column:col_markup elem
        end modu.Module.m_top_deps;
      end;*)
      (** Types *)
      if Module.module_types modu <> [] then begin
        let row_types = model#append ?parent () in
        model#set ~row:row_types ~column:col_markup "Types";
        model#set ~row:row_types ~column:col_kind (Some `Type);
        List.iter begin fun elem ->
          let row = model#append ~parent:row_types () in
          let name = get_relative elem.Odoc_type.ty_name in
          self#set_location ~model row elem.Type.ty_loc;
          let typ =
            match elem.Odoc_type.ty_kind with
              | Type.Type_abstract ->
                model#set ~row ~column:col_kind (Some `Type_abstract);
                let manifest =
                  match elem.Type.ty_manifest with
                  | Some typ -> string_of_type_expr typ
                  | _ -> ""
                in manifest
              | Type.Type_variant constr ->
                model#set ~row ~column:col_kind (Some `Type_variant);
                String.concat " | "
                  (List.map (fun vc -> vc.Type.vc_name) constr)
              | Type.Type_record _ ->
                model#set ~row ~column:col_kind (Some `Type_record);
                ""
          in
          self#set_tooltip ~model ~row ~name ~typ;
        end (Module.module_types modu);
      end;
      (** Elements *)
      List.iter begin fun me ->
        let row = model#append ?parent () in
        match me with
          | Module.Element_module elem ->
            self#set_location row ~model elem.Module.m_loc;
            let name = get_relative elem.Module.m_name in
            model#set ~row ~column:col_markup (sprintf "<b>%s</b>" name);
            self#append ~model ~parent:row [elem];
            model#set ~row ~column:col_kind (Some `Module)
          | Module.Element_module_type elem ->
            self#set_location ~model row elem.Module.mt_loc;
            let name = get_relative elem.Module.mt_name in
            model#set ~row ~column:col_markup name;
          | Module.Element_included_module elem ->
            model#set ~row ~column:col_markup elem.Module.im_name
          | Module.Element_class elem ->
            model#set ~row ~column:col_kind (Some (if elem.Class.cl_virtual then `Class_virtual else `Class));
            let name = get_relative elem.Class.cl_name in
            model#set ~row ~column:col_markup (sprintf "<b>%s</b>" name);
            self#set_location ~model row elem.Class.cl_loc;
            (** Class_structure *)
            begin
              match elem.Class.cl_kind with
                | Class.Class_structure (inherited, elems) ->
                  List.iter begin fun inher ->
                    let row = model#append ~parent:row () in
                    let name = get_relative inher.Class.ic_name in
                    model#set ~row ~column:col_markup name;
                    model#set ~row ~column:col_kind (Some `Class_inherit);
                  end inherited;
                  List.iter begin function
                    | Class.Class_attribute attr ->
                      let attr_name = attr.Odoc_value.att_value.Odoc_value.val_name in
                      if Name.father attr_name = elem.Class.cl_name then begin
                        let row = model#append ~parent:row () in
                        let name = Name.get_relative elem.Class.cl_name attr_name in
                        self#set_location ~model row attr.Odoc_value.att_value.Odoc_value.val_loc;
                        let typ = string_of_type_expr attr.Odoc_value.att_value.Odoc_value.val_type in
                        let name =
                          if attr.Odoc_value.att_virtual then (sprintf "<i>%s</i>" name)
                          else name
                        in
                        self#set_tooltip ~model ~row ~name ~typ;
                        model#set ~row ~column:col_kind (Some
                          (if attr.Odoc_value.att_virtual && attr.Odoc_value.att_mutable then `Attribute_mutable_virtual
                          else if attr.Odoc_value.att_virtual then `Attribute_virtual
                          else if attr.Odoc_value.att_mutable then `Attribute_mutable
                          else `Attribute))
                      end
                    | Class.Class_method met ->
                      let row = model#append ~parent:row () in
                      let name = Name.get_relative elem.Class.cl_name met.Odoc_value.met_value.Odoc_value.val_name in
                      let name =
                        if met.Odoc_value.met_virtual then (sprintf "<i>%s</i>" name)
                        else if met.Odoc_value.met_private then name
                        else name
                      in
                      self#set_location row ~model met.Odoc_value.met_value.Odoc_value.val_loc;
                      let typ = string_of_type_expr met.Odoc_value.met_value.Odoc_value.val_type in
                      self#set_tooltip ~model ~row ~name ~typ;
                      model#set ~row ~column:col_kind (Some
                        (if met.Odoc_value.met_private && met.Odoc_value.met_virtual then `Method_private_virtual
                        else if met.Odoc_value.met_virtual then `Method_virtual
                        else if met.Odoc_value.met_private then `Method_private
                        else `Method));
                    | Class.Class_comment text -> ()
                  end elems;
                | _ -> ()
            end;
          | Module.Element_class_type elem ->
            self#set_location ~model row elem.Class.clt_loc;
            let name = get_relative elem.Class.clt_name in
            model#set ~row ~column:col_markup (sprintf "<b>%s</b>" name);
            model#set ~row ~column:col_kind (Some `Class_type);
          | Module.Element_value elem ->
            let name = get_relative elem.Value.val_name in
            let typ = string_of_type_expr elem.Odoc_value.val_type in
            self#set_tooltip ~model ~row ~name ~typ;
            self#set_location ~model row elem.Value.val_loc;
            model#set ~row ~column:col_kind (Some (if Value.is_function elem then `Function else `Simple));
          | Module.Element_exception elem ->
            self#set_location ~model row elem.Exception.ex_loc;
            let name = get_relative elem.Exception.ex_name in
            model#set ~row ~column:col_markup name;
            model#set ~row ~column:col_kind (Some `Exception)
          | Module.Element_type elem ->
            ignore (model#remove row);
            (*self#set_location row elem.Type.ty_loc;
            model#set ~row ~column elem.Type.ty_name*)
          | Module.Element_module_comment elem ->
            ignore (model#remove row);
            (*let first = first_sentence_of_text elem in
            model#set ~row ~column:col_markup (string_of_text first)*)
      end (Module.module_elements modu);
    end module_list;


end
