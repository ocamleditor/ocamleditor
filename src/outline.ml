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
open Miscellanea
open Odoc_info

let string_rev str =
  let len = String.length str in
  let rts = String.make len ' ' in
  for i = 0 to len - 1 do rts.[len - i - 1] <- str.[i] done;
  rts;;

let string_rev = Miscellanea.Memo.fast ~f:string_rev;;

type kind =
  | Function
  | Simple
  | Class
  | Class_virtual
  | Class_type
  | Class_inherit
  | Attribute
  | Attribute_mutable
  | Attribute_mutable_virtual
  | Attribute_virtual
  | Method
  | Method_private
  | Method_virtual
  | Method_private_virtual
  | Type
  | Type_abstract
  | Type_variant
  | Type_record
  | Module
  | Exception
  | Error
  | Warning
  | Folder_warnings
  | Folder_errors
  | Dependencies
  | Bookmark of GdkPixbuf.pixbuf
  | Unknown

let pixbuf_of_kind = function
  | Function -> Icons.func
  | Simple -> Icons.simple
  | Method -> Icons.met
  | Method_private -> Icons.met_private
  | Method_virtual -> Icons.met_virtual
  | Method_private_virtual -> Icons.met_private_virtual
  | Attribute -> Icons.attribute
  | Attribute_mutable -> Icons.attribute_mutable
  | Attribute_mutable_virtual -> Icons.attribute_mutable_virtual
  | Attribute_virtual -> Icons.attribute_virtual
  | Type -> Icons.typ
  | Type_abstract -> Icons.type_abstract
  | Type_variant -> Icons.type_variant
  | Type_record -> Icons.type_record
  | Class -> Icons.classe
  | Class_virtual -> Icons.class_virtual
  | Class_type -> Icons.class_type
  | Class_inherit -> Icons.class_inherit
  | Module -> Icons.module_impl
  | Exception -> Icons.exc
  | Error -> Icons.error_14
  | Warning -> Icons.warning_14
  | Folder_warnings -> Icons.folder_warning
  | Folder_errors -> Icons.folder_error
  | Dependencies -> Icons.none_14
  | Bookmark pixbuf -> pixbuf
  | Unknown -> Icons.none_14;;

class widget ~project ~page ~tmp =
  let show_types           = Preferences.preferences#get.Preferences.pref_outline_show_types in
  let buffer               = (page#buffer :> Ocaml_text.buffer) in
  let vbox                 = GPack.vbox () in
  let toolbar              = GPack.hbox ~spacing:0 ~packing:vbox#pack ~show:true () in
  let button_refresh       = GButton.button ~relief:`NONE ~packing:toolbar#pack () in
  let button_show_types    = GButton.toggle_button ~active:show_types ~relief:`NONE ~packing:toolbar#pack () in
  let button_sort          = GButton.toggle_button ~relief:`NONE ~packing:toolbar#pack () in
  let button_sort_rev      = GButton.toggle_button ~relief:`NONE ~packing:toolbar#pack () in
  let button_select_struct = GButton.button ~relief:`NONE ~packing:toolbar#pack () in
  let button_select_buf    = GButton.button ~relief:`NONE ~packing:toolbar#pack () in
  let _                    = button_refresh#set_image (GMisc.image ~stock:`REFRESH ~icon_size:`MENU ())#coerce in
  let _                    = button_sort#set_image (GMisc.image ~stock:`SORT_ASCENDING ~icon_size:`MENU ())#coerce in
  let _                    = button_sort_rev#set_image (GMisc.image ~stock:`SORT_DESCENDING ~icon_size:`MENU ())#coerce in
  let _                    = button_show_types#set_image (GMisc.image ~pixbuf:Icons.typ ())#coerce in
  let _                    = button_select_buf#set_image (GMisc.image ~pixbuf:Icons.select_in_buffer ())#coerce in
  let _                    = button_select_struct#set_image (GMisc.image ~pixbuf:Icons.select_in_structure ())#coerce in
  let _                    = button_sort#misc#set_tooltip_text "Sort by name" in
  let _                    = button_sort_rev#misc#set_tooltip_text "Sort by reverse name" in
  let _                    = button_show_types#misc#set_tooltip_text "Show types" in
  let _                    = button_select_struct#misc#set_tooltip_text "Select in Structure Pane" in
  let _                    = button_select_buf#misc#set_tooltip_text "Select in Buffer" in
  let _ =
    button_show_types#misc#set_can_focus false;
    button_sort#misc#set_can_focus false;
    button_sort_rev#misc#set_can_focus false;
    button_select_struct#misc#set_can_focus false;
    button_select_buf#misc#set_can_focus false;
    button_refresh#misc#set_can_focus false;
  in
  (*  *)
  let includes          = Project.get_includes project in
  let sw                = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let cols              = new GTree.column_list in
  let col_icon          = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf") in
  let col_name          = cols#add Gobject.Data.string in
  let col_name_sort     = cols#add Gobject.Data.string in
  let col_type          = cols#add Gobject.Data.string in
  let col_markup        = cols#add Gobject.Data.string in
  let col_id            = cols#add Gobject.Data.int in
  let view              = GTree.view ~headers_visible:false ~packing:sw#add () in
  let renderer_pixbuf   = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
  let renderer_markup   = GTree.cell_renderer_text [`YPAD 0] in
  let vc                = GTree.view_column () in
  let _                 = vc#pack ~expand:false renderer_pixbuf in
  let _                 = vc#pack ~expand:false renderer_markup in
  let _                 = vc#add_attribute renderer_pixbuf "pixbuf" col_icon in
  let _                 = vc#add_attribute renderer_markup "markup" col_markup in
  let _                 = view#selection#set_mode `SINGLE in
  let _                 = view#append_column vc in
  let _                 = view#misc#set_property "enable-tree-lines" (`BOOL true) in
  let _                 = view#misc#modify_font_by_name Preferences.preferences#get.Preferences.pref_compl_font in
  let _                 = view#misc#modify_base [`SELECTED, `NAME Oe_config.outline_selection_bg_color; `ACTIVE, `NAME Oe_config.outline_active_bg_color] in
  let _                 = view#misc#modify_text [`SELECTED, `NAME Oe_config.outline_selection_fg_color; `ACTIVE, `NAME Oe_config.outline_active_fg_color] in
  let type_color        = Oe_config.outline_type_color in
  let type_color_re     = Str.regexp_string type_color in
  let type_color_sel    = Color.name_of_gdk (view#misc#style#fg `SELECTED) in
  let type_color_sel_re = Str.regexp_string type_color_sel in
  let dump_filename     =
    match project.Project_type.in_source_path page#get_filename with
      | Some rel ->
        let tmp = Project.path_tmp project in
        tmp // ((Filename.chop_extension rel) ^ ".outline")
      | _ -> Filename.temp_file "outline" ""
  in
object (self)
  inherit GObj.widget vbox#as_widget
  val mutable locations = []
  val mutable tooltips = []
  val mutable current_model : GTree.tree_store option = None
  val mutable signal_selection_changed = None
  val mutable current_dump_size = 0
  val mutable sort_column = None
  val mutable col_counter = -1;
  val mutable last_selected_path = None
  val table_markup = Hashtbl.create 17
  val table_markup_name = Hashtbl.create 17

  initializer self#init()

  method private init () =
    let replace_color_in_markup (model : GTree.tree_store) invert path =
      let row = model#get_iter path in
      let markup = model#get ~row ~column:col_markup in
      let new_markup = if invert then begin
        Str.replace_first type_color_sel_re type_color markup
      end else begin
        Str.replace_first type_color_re type_color_sel markup
      end in
      model#set ~row ~column:col_markup new_markup;
    in
    ignore (view#selection#connect#changed ~callback:begin fun () ->
      match view#selection#get_selected_rows with
        | path :: _ ->
          begin
            match current_model with
              | Some model ->
                Gaux.may last_selected_path ~f:(replace_color_in_markup model true);
                replace_color_in_markup model false path;
                last_selected_path <- Some path;
              | _ -> ()
          end;
        | _ -> ()
    end);
    (*  *)
    self#parse ();
    (** Events *)
    signal_selection_changed <- Some (view#selection#connect#after#changed ~callback:self#select_element);
    ignore (view#connect#after#row_activated ~callback:begin fun _ _ ->
      self#select_element();
      page#view#misc#grab_focus();
    end);
    ignore (view#misc#connect#realize ~callback:begin fun () ->
      let show = Preferences.preferences#get.Preferences.pref_outline_show_types in
      if show <> button_show_types#active then button_show_types#clicked()
    end);
    (** Tooltips *)
    view#misc#set_has_tooltip true;
    ignore (view#misc#connect#query_tooltip ~callback: begin fun ~x ~y ~kbd tooltip ->
      try
        begin
          match GtkTree.TreeView.Tooltip.get_context view#as_tree_view ~x ~y ~kbd with
            | (x, y, Some (_, _, row)) ->
              begin
                match view#get_path_at_pos ~x ~y with
                  | Some (tpath, _, _, _) ->
                    let col_id =
                      match current_model with
                        | Some model -> model#get ~row ~column:col_id
                        | _ -> assert false
                    in
                    let _, typ = List.find (fun (rr, _) -> rr = col_id) tooltips in
                    let markup = Print_type.markup2 typ in
                    GtkBase.Tooltip.set_markup tooltip markup;
                    (*let label = GMisc.label ~markup () in
                    GtkBase.Tooltip.set_custom tooltip label#as_widget;*) (* This flickers on some platforms *)
                    GtkTree.TreeView.Tooltip.set_row view#as_tree_view tooltip tpath;
                    true
                  | _ -> false
              end
            | _ -> false
        end
      with Not_found | Gpointer.Null -> false
    end);
    self#init_toolbar_buttons()

  method view = view

  method select ?align (mark : Gtk.text_mark) =
    match current_model with
      | None -> ()
      | Some model ->
        begin
          let finally () = Gaux.may signal_selection_changed ~f:view#selection#misc#handler_unblock in
          Gaux.may signal_selection_changed ~f:view#selection#misc#handler_block;
          try
            let iter = buffer#get_iter_at_mark (`MARK mark) in
            let rr, _ =
              List.find begin fun (_, (m, _)) ->
                if true (*rr#valid*) then begin
                  let it = buffer#get_iter_at_mark (`MARK m) in
                  it#compare iter <= 0
                end else false
              end locations
            in
            let path = ref None in
            model#foreach begin fun p row ->
              if model#get ~row ~column:col_id = rr then path := Some p;
              !path <> None
            end;
            begin
              match !path with
                | Some path ->
                  begin
                    match align with
                      | Some align ->
                        view#vadjustment#set_value (align *. view#vadjustment#upper);
                      | None when page#view#misc#get_flag `HAS_FOCUS ->
                        begin
                          if not (Gmisclib.Util.treeview_is_path_onscreen view path) then begin
                            view#scroll_to_cell ~align:(0.38, 0.) path vc;
                          end
                        end;
                      | _ -> ()
                  end;
                  view#set_cursor path vc;
                | _ -> ()
            end;
            finally()
          with Not_found | Gpointer.Null | Invalid_argument("Gobject.Value.get") -> (finally())
        end;

  method select_element ?(select_range=false) () =
    match view#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
        begin
          try
            let col_id = match current_model with Some m -> m#get ~row:(m#get_iter path) ~column:col_id | _ -> assert false in
            let _, (mark, _) = List.find (fun (rr, _) -> col_id = rr) locations in
            let where = buffer#get_iter_at_mark (`MARK mark) in
            if select_range then begin
              let tag_table = new GText.tag_table page#buffer#tag_table in
              let comment = match tag_table#lookup "comment" with Some t -> new GText.tag t | _ -> assert false in
              let ocamldoc = match tag_table#lookup "ocamldoc" with Some t -> new GText.tag t | _ -> assert false in
              let ocamldoc_paragraph = match tag_table#lookup "ocamldoc-paragraph" with Some t -> new GText.tag t | _ -> assert false in
              let start = where#set_line_index 0 in
              let stop =
                let rec find = function
                  | [] -> raise Not_found
                  | (rr, _) :: [] ->
                    if rr = col_id then buffer#end_iter#backward_char else (raise Not_found)
                  | (rr, _) :: (((_, (mark, _)) :: _) as tl) ->
                    if rr = col_id then begin
                      ((buffer#get_iter_at_mark (`MARK mark))#set_line_index 0)#backward_char
                    end else (find tl)
                in find (List.rev locations)
              in
              let stop = ref stop in
              while
                Glib.Unichar.isspace !stop#char ||
                !stop#has_tag comment ||
                !stop#has_tag ocamldoc ||
                !stop#has_tag ocamldoc_paragraph
              do stop := !stop#backward_char done;
              let stop = !stop#forward_to_line_end#forward_char in
              let moved = (page#view :> GText.view)#scroll_to_iter stop in
              if moved then begin
                ignore ((page#view :> GText.view)#scroll_to_iter ~use_align:true ~xalign:1.0 ~yalign:0.62 stop);
              end;
              buffer#select_range stop start;
              page#view#misc#grab_focus();
            end else begin
              buffer#place_cursor ~where;
              page#view#scroll_lazy where;
              (*Gmisclib.Idle.add ~prio:300 (fun () ->
                ignore ((page#view :> GText.view)#scroll_to_iter ~use_align:true ~xalign:1.0 ~yalign:0.38 where));*)
            end;
          with Not_found -> ()
            | ex -> Printf.eprintf "File \"outline.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
        end;

  method parse ?(force=false) () =
    if tmp <> "" then begin
      let dump_filename = dump_filename in
      let cmd = sprintf "%s -dump %s %s%s %s%s"
        (Ocaml_config.ocamldoc())
        (Quote.arg dump_filename)
        (" -I +threads")
        (*(if project.Project.thread then " -thread" else if project.Project.vmthread then " -vmthread" else "")*)
        (if includes = [] then "" else (" -I " ^ (String.concat " -I " (List.map Quote.arg includes))))
        (Quote.arg tmp)
        (if Common.application_debug then Miscellanea.redirect_stderr else "")
      in
      let model = GTree.tree_store cols in
      Gaux.may sort_column ~f:(fun (id, dir) -> model#set_sort_column_id id dir);
      ignore begin
        let activity_name = cmd in
        Activity.add Activity.Outline activity_name;
        Oebuild_util.exec ~echo:true ~join:false ~at_exit:begin fun () ->
          try
            let size = (Unix.stat dump_filename).Unix.st_size in
            if not force && current_dump_size = size then (raise Exit);
            let module_list = Odoc_info.load_modules dump_filename in
            current_dump_size <- size;
            last_selected_path <- None;
            Hashtbl.clear table_markup;
            Hashtbl.clear table_markup_name;
            col_counter <- (-1);
            tooltips <- [];
            let locs = locations in
            buffer#block_signal_handlers ();
            Gmisclib.Idle.add ~prio:600 (fun () -> List.iter (fun (_, (m, _)) ->
              GtkText.Buffer.delete_mark buffer#as_buffer m) locs);
            buffer#unblock_signal_handlers ();
            locations <- [];
            let align = view#vadjustment#value /. view#vadjustment#upper in
            ignore (model#connect#row_inserted ~callback:begin fun _ row ->
              col_counter <- col_counter + 1;
              model#set ~row ~column:col_id col_counter;
            end);
            self#append ~model module_list;
            current_model <- Some model;
            view#set_model (Some (model :> GTree.model));
            GtkThread2.async begin fun () ->
              (** Sort functions (0) *)
              model#set_sort_func col_name_sort.GTree.index
                (fun model i1 i2 -> self#compare_name model i1 i2);
              model#set_sort_func col_id.GTree.index
                (fun model i1 i2 -> self#compare_struct model i1 i2);
              model#set_default_sort_func
                (fun model i1 i2 -> self#compare_struct model i1 i2);
            end ();
            (** Expanded and collapsed nodes *)
            view#expand_all ();
            locations <- List.sort begin fun (_, (m1, _)) (_, (m2, _)) ->
              let i1 = buffer#get_iter_at_mark (`MARK m1) in
              let i2 = buffer#get_iter_at_mark (`MARK m2) in
              (-1) * (i1#compare i2)
            end locations;
            Gmisclib.Idle.add ~prio:600 (fun () -> self#select ~align (buffer#get_mark `INSERT)); (* 0.5 *)
            Gmisclib.Idle.add ~prio:600 (fun () -> if Sys.file_exists dump_filename then (Sys.remove dump_filename));
            Activity.remove activity_name;
          with End_of_file | Exit | Unix.Unix_error _ -> (Activity.remove activity_name)
        end cmd;
      end;
    end

  method private init_toolbar_buttons () =
    (** Toolbar buttons *)
    ignore (button_show_types#connect#toggled ~callback:begin fun () ->
      Gaux.may current_model ~f:begin fun (model : GTree.tree_store) ->
        model#foreach begin fun _ row ->
          let id = model#get ~row ~column:col_id in
          let markup =
            if button_show_types#active then
              (try Hashtbl.find table_markup id with Not_found -> sprintf "--->%d" id)
            else
              (try Hashtbl.find table_markup_name id with Not_found -> sprintf "===>%d" id)
          in
          model#set ~row ~column:col_markup markup;
          false
        end;
        Preferences.preferences#get.Preferences.pref_outline_show_types <- button_show_types#active;
        Preferences.save();
      end;
    end);
    let signal_button_sort : GtkSignal.id option ref = ref None in
    let signal_button_sort_rev : GtkSignal.id option ref = ref None in
    signal_button_sort := Some (button_sort#connect#after#toggled ~callback:begin fun () ->
      Gaux.may current_model ~f:begin fun model ->
        let id =
          if button_sort#active then begin
            Gaux.may !signal_button_sort_rev ~f:button_sort_rev#misc#handler_block;
            button_sort_rev#set_active false;
            Gaux.may !signal_button_sort_rev ~f:button_sort_rev#misc#handler_unblock;
            col_name_sort.GTree.index
          end else col_id.GTree.index
        in
        sort_column <- Some (id, `ASCENDING);
        model#set_sort_column_id id `ASCENDING;
        GtkBase.Widget.queue_draw view#as_widget;
      end
    end);
    signal_button_sort_rev := Some (button_sort_rev#connect#after#toggled ~callback:begin fun () ->
      Gaux.may current_model ~f:begin fun model ->
        let id, dir =
          if button_sort_rev#active then begin
            Gaux.may !signal_button_sort ~f:button_sort#misc#handler_block;
            button_sort#set_active false;
            Gaux.may !signal_button_sort ~f:button_sort#misc#handler_unblock;
            col_name_sort.GTree.index, `DESCENDING
          end else col_id.GTree.index, `ASCENDING
        in
        (* `DESCENDING to force a new sort, see #compare_name when button_rev#active *)
        sort_column <- Some (id, dir);
        model#set_sort_column_id id dir;
        GtkBase.Widget.queue_draw view#as_widget;
      end
    end);
    (*  *)
    ignore (button_select_buf#connect#clicked ~callback:(self#select_element ~select_range:true));
    ignore (button_select_struct#connect#clicked ~callback:begin fun () ->
      self#select ?align:None (page#buffer#get_mark `INSERT)
    end);
    ignore (button_refresh#connect#clicked ~callback:begin fun () ->
      self#parse ~force:true ();
    end);

  method private compare_name model i1 i2 =
    let name1 = model#get ~row:i1 ~column:col_name_sort in
    let name2 = model#get ~row:i2 ~column:col_name_sort in
    if button_sort_rev#active then begin
      let name1 = string_rev name1 in
      let name2 = string_rev name2 in
      Pervasives.compare name2 name1
    end else (Pervasives.compare name1 name2)

  method private compare_struct model i1 i2 =
    let o1 = model#get ~row:i1 ~column:col_id in
    let o2 = model#get ~row:i2 ~column:col_id in
    Pervasives.compare o1 o2

  method private markup ~name ~typ ~kind =
    let name = Glib.Convert.convert_with_fallback ~fallback:"" ~from_codeset:Oe_config.ocaml_codeset ~to_codeset:"UTF-8" name in
    let markup_name = Glib.Markup.escape_text name in
    let markup_name =
      match kind with
        | Class | Module | Class_type | Class_virtual -> sprintf "<b>%s</b>" markup_name
        | _ -> markup_name
    in
    Hashtbl.add table_markup_name col_counter markup_name;
    (*  *)
    let typ = Glib.Convert.convert_with_fallback ~fallback:"" ~from_codeset:Oe_config.ocaml_codeset ~to_codeset:"UTF-8" typ in
    let markup_type = if typ = "" then markup_name else
      sprintf "%s <span color='%s'>%s</span>"
        markup_name
        type_color
        (sprintf ": %s"
          (Print_type.markup2 (Miscellanea.replace_all ~regexp:true ["\n", ""; " +", " "] typ)))
    in
    Hashtbl.add table_markup col_counter markup_type;
    if not button_show_types#active || typ = "" then markup_name else markup_type

  method private set_location loc rr =
    let _, loc_pos =
      (*match loc.Odoc_types.loc_impl with Some (a, b) -> a, b | _ -> "", 0*)
      match loc.Odoc_types.loc_impl with
        | Some loc ->
          loc.Location.loc_start.Lexing.pos_fname,
          loc.Location.loc_start.Lexing.pos_cnum
        | _ -> "", 0
    in
    buffer#block_signal_handlers ();
    let mark = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Outline.set_location")*) ?left_gravity:None (buffer#get_iter (`OFFSET loc_pos)) in
    buffer#unblock_signal_handlers ();
    locations <- (rr, (mark, (fun () -> false))) :: locations;

  method private append ~model ?parent module_list =
    List.iter begin fun modu ->
      let get_relative = Name.get_relative modu.Module.m_name in
      (*(** Dependencies *)
      if modu.Module.m_top_deps <> [] then begin
        let row_dep = model#append ?parent () in
        model#set ~row:row_dep ~column:col_name "Dependencies";
        model#set ~row:row_dep ~column:col_kind (Some `Dependencies);
        List.iter begin fun elem ->
          let row = model#append ~parent:row_dep () in
          model#set ~row ~column:col_name elem
        end modu.Module.m_top_deps;
      end;*)
      (** Types *)
      self#append_types ~model ?parent modu;
      (** Elements *)
      List.iter begin fun me ->
        let row = model#append ?parent () in
        match me with
          | Module.Element_module elem ->
            GtkThread2.sync begin fun () ->
              self#set_location elem.Module.m_loc col_counter;
              let name = get_relative elem.Module.m_name in
              model#set ~row ~column:col_name name;
              model#set ~row ~column:col_name_sort name;
              model#set ~row ~column:col_icon (pixbuf_of_kind Module);
              model#set ~row ~column:col_markup (self#markup ~name ~typ:"" ~kind:Module);
              self#append ~model ~parent:row [elem];
            end ()
          | Module.Element_module_type elem ->
            GtkThread2.sync begin fun () ->
              self#set_location elem.Module.mt_loc col_counter;
              let name = get_relative elem.Module.mt_name in
              model#set ~row ~column:col_name name;
              model#set ~row ~column:col_name_sort name;
              model#set ~row ~column:col_markup (self#markup ~name ~typ:"" ~kind:Module);
            end ()
          | Module.Element_included_module elem ->
            GtkThread2.sync begin fun () ->
              let name = elem.Module.im_name in
              model#set ~row ~column:col_name name;
              model#set ~row ~column:col_name_sort elem.Module.im_name;
              model#set ~row ~column:col_markup (self#markup ~name ~typ:"" ~kind:Module);
              (*self#set_location elem.Module.im_loc rr;*)
            end ();
          | Module.Element_class elem ->
            GtkThread2.sync begin fun () ->
              let name = get_relative elem.Class.cl_name in
              let kind = if elem.Class.cl_virtual then Class_virtual else Class in
              model#set ~row ~column:col_icon (pixbuf_of_kind kind);
              model#set ~row ~column:col_name name;
              model#set ~row ~column:col_name_sort name;
              model#set ~row ~column:col_markup (self#markup ~name ~typ:"" ~kind);
              self#set_location elem.Class.cl_loc col_counter;
            end ();
            (** Class_structure *)
            self#append_class ~model ~row modu elem;
          | Module.Element_class_type elem ->
            GtkThread2.sync begin fun () ->
              let name = get_relative elem.Class.clt_name in
              model#set ~row ~column:col_name name;
              model#set ~row ~column:col_name_sort name;
              model#set ~row ~column:col_icon (pixbuf_of_kind Class_type);
              model#set ~row ~column:col_markup (self#markup ~name ~typ:"" ~kind:Class_type);
              self#set_location elem.Class.clt_loc col_counter;
            end ()
          | Module.Element_value elem ->
            GtkThread2.sync begin fun () ->
              Odoc_info.reset_type_names();
              let name = get_relative elem.Value.val_name in
              let typ = string_of_type_expr elem.Odoc_value.val_type in
              let kind = if Value.is_function elem then Function else Simple in
              model#set ~row ~column:col_name name;
              model#set ~row ~column:col_name_sort name;
              model#set ~row ~column:col_type typ;
              model#set ~row ~column:col_markup (self#markup ~name ~typ ~kind);
              model#set ~row ~column:col_icon (pixbuf_of_kind kind);
              self#set_location elem.Value.val_loc col_counter;
              tooltips <- (col_counter, typ) :: tooltips;
            end ();
          | Module.Element_exception elem ->
            GtkThread2.sync begin fun () ->
              let name = get_relative elem.Exception.ex_name in
              model#set ~row ~column:col_name name;
              model#set ~row ~column:col_name_sort name;
              model#set ~row ~column:col_icon (pixbuf_of_kind Exception);
              model#set ~row ~column:col_markup (self#markup ~name ~typ:"" ~kind:Exception);
              self#set_location elem.Exception.ex_loc col_counter;
            end ()
          | Module.Element_type _ ->
            GtkThread2.sync begin fun () ->
              ignore (model#remove row);
            end ()
          | Module.Element_module_comment _ ->
            GtkThread2.sync begin fun () ->
              ignore (model#remove row);
            end ()
      end (Module.module_elements modu);
    end module_list;

  method private append_class ~model ~row modu elem =
    let get_relative = Name.get_relative modu.Module.m_name in
    match elem.Class.cl_kind with
      | Class.Class_structure (inherited, elems) ->
        List.iter begin fun inher ->
          GtkThread2.sync begin fun () ->
            let row = model#append ~parent:row () in
            let name = get_relative inher.Class.ic_name in
            model#set ~row ~column:col_name name;
            model#set ~row ~column:col_name_sort ("0" ^ name ^ "0");
            model#set ~row ~column:col_icon (pixbuf_of_kind Class_inherit);
            model#set ~row ~column:col_markup (self#markup ~name ~typ:"" ~kind:Class_inherit);
            (*self#set_location inher.Class.cl_loc col_counter;*)
          end ()
        end inherited;
        List.iter begin function
          | Class.Class_attribute attr ->
            GtkThread2.sync begin fun () ->
              let attr_name = attr.Odoc_value.att_value.Odoc_value.val_name in
              let father = Name.father attr_name in
              if father = elem.Class.cl_name then begin
                let row = model#append ~parent:row () in
                let name = Name.get_relative elem.Class.cl_name attr_name in
                self#set_location attr.Odoc_value.att_value.Odoc_value.val_loc col_counter;
                Odoc_info.reset_type_names();
                let typ = string_of_type_expr attr.Odoc_value.att_value.Odoc_value.val_type in
                let kind =
                  if attr.Odoc_value.att_virtual && attr.Odoc_value.att_mutable then Attribute_mutable_virtual
                  else if attr.Odoc_value.att_virtual then Attribute_virtual
                  else if attr.Odoc_value.att_mutable then Attribute_mutable
                  else Attribute
                in
                model#set ~row ~column:col_name name;
                model#set ~row ~column:col_name_sort name;
                model#set ~row ~column:col_type typ;
                model#set ~row ~column:col_markup (self#markup ~name ~typ ~kind);
                model#set ~row ~column:col_icon (pixbuf_of_kind kind);
                tooltips <- (col_counter, typ) :: tooltips;
              end
            end ()
          | Class.Class_method met ->
            GtkThread2.sync begin fun () ->
              let row = model#append ~parent:row () in
              let name = Name.get_relative elem.Class.cl_name met.Odoc_value.met_value.Odoc_value.val_name in
              let name =
                if met.Odoc_value.met_virtual then name
                else if met.Odoc_value.met_private then name
                else name
              in
              Odoc_info.reset_type_names();
              let typ = string_of_type_expr met.Odoc_value.met_value.Odoc_value.val_type in
              let kind =
                if met.Odoc_value.met_private && met.Odoc_value.met_virtual then Method_private_virtual
                else if met.Odoc_value.met_virtual then Method_virtual
                else if met.Odoc_value.met_private then Method_private
                else Method
              in
              model#set ~row ~column:col_name name;
              model#set ~row ~column:col_name_sort name;
              model#set ~row ~column:col_type typ;
              model#set ~row ~column:col_markup (self#markup ~name ~typ ~kind);
              model#set ~row ~column:col_icon (pixbuf_of_kind kind);
              self#set_location met.Odoc_value.met_value.Odoc_value.val_loc col_counter;
              tooltips <- (col_counter, typ) :: tooltips;
              end ()
          | Class.Class_comment _ -> ()
        end elems;
      | _ -> ()

  method private append_types ~model ?parent modu =
    if Module.module_types modu <> [] then begin
      let row_types = model#append ?parent () in
      let name = "Types" in
      model#set ~row:row_types ~column:col_name name;
      model#set ~row:row_types ~column:col_name_sort ("0" ^ name ^ "0");
      model#set ~row:row_types ~column:col_icon (pixbuf_of_kind Type);
      model#set ~row:row_types ~column:col_markup (self#markup ~name ~typ:"" ~kind:Type);
      List.iter begin fun elem ->
        let row, rr = GtkThread2.sync begin fun () ->
          let row = model#append ~parent:row_types () in
          let rr = col_counter in
          self#set_location elem.Type.ty_loc rr;
          row, rr
        end () in
        Odoc_info.reset_type_names();
        let typ, kind =
          match elem.Odoc_type.ty_kind with
            | Type.Type_abstract ->
              GtkThread2.sync begin fun () ->
                model#set ~row ~column:col_icon (pixbuf_of_kind Type_abstract);
                let manifest =
                  match elem.Type.ty_manifest with
                  | Some typ -> string_of_type_expr typ
                  | _ -> ""
                in manifest, Type_abstract
              end ()
            | Type.Type_variant constr ->
              GtkThread2.sync begin fun () ->
                model#set ~row ~column:col_icon (pixbuf_of_kind Type_variant);
                let (!!!) vc =
                  List.map begin fun arg ->
                    let te = Odoc_info.string_of_type_expr arg in
                    Str.global_replace (Str.regexp_string ((Name.father elem.Type.ty_name) ^ ".")) "" te
                  end vc.Type.vc_args
                in
                let sep = if true || List.length constr > 5 then "\n | " else " | " in
                "  " ^ String.concat sep (List.map begin fun vc ->
                  vc.Type.vc_name ^
                  (if vc.Type.vc_args <> [] then begin
                    " of " ^ (let args = !!! vc in String.concat " * " args)
                  end else "")
                end constr),
                Type_variant
              end ()
            | Type.Type_record fields ->
              GtkThread2.sync begin fun () ->
                model#set ~row ~column:col_icon (pixbuf_of_kind Type_record);
                "{\n" ^ (String.concat ";\n"
                  (List.map (fun fi -> sprintf "  %s: %s" fi.Type.rf_name
                    (string_of_type_expr fi.Type.rf_type)) fields)) ^
                "\n}", Type_record
              end ()
        in
        let name = Name.get_relative modu.Module.m_name elem.Odoc_type.ty_name in
        model#set ~row ~column:col_name name;
        model#set ~row ~column:col_name_sort name;
        model#set ~row ~column:col_type typ;
        model#set ~row ~column:col_markup (self#markup ~name ~typ ~kind);
        tooltips <- (rr, typ) :: tooltips;
      end (Module.module_types modu);
    end;

end

(** create_empty *)
let create_empty () =
  let vp = GBin.viewport () in
  let label = GMisc.label ~xalign:0.5 ~yalign:0. ~xpad:3 ~ypad:3
    ~text:"Structure is not available" ~packing:vp#add () in
  vp#misc#modify_bg [`NORMAL, `NAME "#ffffff"];
  label#misc#modify_fg [`NORMAL, `NAME "#d0d0d0"];
  vp#coerce










