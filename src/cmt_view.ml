(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

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
[@@@warning "-48"]

open Printf
open GUtil
open Cmt_format
open Location
open Typedtree
open! Asttypes
open! Types

module Log = Common.Log.Make(struct let prefix = "Cmt_view" end)
let _ = Log.set_verbosity `DEBUG

type kind =
  | Function
  | Simple
  | Class
  | Class_virtual
  | Class_type
  | Class_inherit
  | Class_let_bindings
  | Attribute
  | Attribute_mutable
  | Attribute_mutable_virtual
  | Attribute_virtual
  | Initializer
  | Method
  | Method_private
  | Method_virtual
  | Method_private_virtual
  | Method_inherited
  | Type
  | Type_abstract
  | Type_variant
  | Type_record
  | Type_open
  | Module
  | Module_functor
  | Module_type
  | Module_include
  | Exception
  | Error
  | Warning
  | Folder_warnings
  | Folder_errors
  | Dependencies
  | Bookmark of GdkPixbuf.pixbuf
  | Unknown

let pixbuf_of_kind = function
  | Function -> Some Icons.func
  | Simple -> Some Icons.simple
  | Method -> Some Icons.met
  | Method_private -> Some Icons.met_private
  | Method_virtual -> Some Icons.met_virtual
  | Method_private_virtual -> Some Icons.met_private_virtual
  | Method_inherited -> Some Icons.met
  | Initializer -> Some Icons.init
  | Attribute -> Some Icons.attribute
  | Attribute_mutable -> Some Icons.attribute_mutable
  | Attribute_mutable_virtual -> Some Icons.attribute_mutable_virtual
  | Attribute_virtual -> Some Icons.attribute_virtual
  | Type -> Some Icons.typ
  | Type_abstract -> Some Icons.type_abstract
  | Type_variant -> Some Icons.type_variant
  | Type_record -> Some Icons.type_record
  | Type_open -> Some Icons.type_variant
  | Class -> Some Icons.classe
  | Class_virtual -> Some Icons.class_virtual
  | Class_type -> Some Icons.class_type
  | Class_inherit -> Some Icons.class_inherit
  | Class_let_bindings -> None
  | Module -> Some Icons.module_impl
  | Module_functor -> Some Icons.module_funct
  | Module_type -> Some Icons.module_type
  | Module_include -> Some Icons.module_include
  | Exception -> Some Icons.exc
  | Error -> Some Icons.error_14
  | Warning -> Some Icons.warning_14
  | Folder_warnings -> Some Icons.folder_warning
  | Folder_errors -> Some Icons.folder_error
  | Dependencies -> None
  | Bookmark pixbuf -> Some pixbuf
  | Unknown -> None;;

type info = {
  typ          : string;
  kind         : kind option;
  location     : Location.t option;
  body         : Location.t option;
  mutable mark : Gtk.text_mark option;
}

let cols               = new GTree.column_list
let col_icon           = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf")
let col_name           = cols#add Gobject.Data.string
let col_markup         = cols#add Gobject.Data.string
let col_lazy           : (unit -> unit) list GTree.column = cols#add Gobject.Data.caml
let col_default_sort   = cols#add Gobject.Data.int

let string_rev str =
  let len = String.length str in
  Bytes.init len (fun i -> str.[len - i - 1])
  |> Bytes.to_string

let string_rev = Miscellanea.Memo.create string_rev;;

let is_function type_expr =
  let rec f t =
    match [@warning "-4"] t.Types.desc with
      | Types.Tarrow _ -> true
      | Types.Tlink t -> f t
      | _ -> false
  in f type_expr;;

let string_of_type_expr te =
  match [@warning "-4"] te.desc with
    | Tarrow (_, _, t2, _) -> Odoc_info.string_of_type_expr t2
    | _ -> Odoc_info.string_of_type_expr te;;

let string_of_longident t
  = String.concat "." @@ Longident.flatten t

(** empty *)
let empty () =
  let pref = Preferences.preferences#get in
  let vp = GBin.viewport () in
  let label = GMisc.label ~xalign:0.5 ~yalign:0. ~xpad:3 ~ypad:3
    ~text:"Structure is not available" ~packing:vp#add () in
  vp#misc#modify_bg [`NORMAL, `NAME pref.Preferences.pref_outline_color_nor_bg];
  label#misc#modify_fg [
    `NORMAL, `NAME pref.Preferences.pref_outline_color_nor_fg
  ];
  vp#coerce;;

let dummy_re = Str.regexp ""

(** widget *)
class widget ~editor:_ ~page ?packing () =
  let pref                   = Preferences.preferences#get in
  let show_types             = pref.Preferences.pref_outline_show_types in
  let vbox                   = GPack.vbox ?packing () in
  let toolbar                = GPack.hbox ~spacing:0 ~packing:vbox#pack ~show:true () in
  let button_refresh         = GButton.button ~relief:`NONE ~packing:toolbar#pack () in
  let button_show_types      = GButton.toggle_button ~active:show_types ~relief:`NONE ~packing:toolbar#pack () in
  let button_sort            = GButton.toggle_button ~relief:`NONE ~packing:toolbar#pack () in
  let button_sort_rev        = GButton.toggle_button ~relief:`NONE ~packing:toolbar#pack () in
  let button_select_from_buf = GButton.button ~relief:`NONE ~packing:toolbar#pack () in
  let button_select_buf      = GButton.button ~relief:`NONE ~packing:toolbar#pack () in
  let _                      = button_refresh#set_image (GMisc.image (*~stock:`REFRESH*) ~pixbuf:Icons.refresh16 ~icon_size:`MENU ())#coerce in
  let _                      = button_sort#set_image (GMisc.image (*~stock:`SORT_ASCENDING*) ~pixbuf:Icons.sort_asc ~icon_size:`MENU ())#coerce in
  let _                      = button_sort_rev#set_image (GMisc.image (*~stock:`SORT_DESCENDING*) ~pixbuf:Icons.sort_asc_rev ~icon_size:`MENU ())#coerce in
  let _                      = button_show_types#set_image (GMisc.image ~pixbuf:Icons.typ ())#coerce in
  let _                      = button_select_buf#set_image (GMisc.image ~pixbuf:Icons.select_in_buffer ())#coerce in
  let _                      = button_select_from_buf#set_image (GMisc.image ~pixbuf:Icons.select_in_structure ())#coerce in
  let _                      = button_sort#misc#set_tooltip_text "Sort by name" in
  let _                      = button_sort_rev#misc#set_tooltip_text "Sort by reverse name" in
  let _                      = button_show_types#misc#set_tooltip_text "Show types" in
  let _                      = button_select_from_buf#misc#set_tooltip_text "Select in Structure Pane" in
  let _                      = button_select_buf#misc#set_tooltip_text "Select in Buffer" in
  let _                      =
    button_show_types#misc#set_can_focus false;
    button_sort#misc#set_can_focus false;
    button_sort_rev#misc#set_can_focus false;
    button_select_from_buf#misc#set_can_focus false;
    button_select_buf#misc#set_can_focus false;
    button_refresh#misc#set_can_focus false;
  in
  let model                  = GTree.tree_store cols in
  let model_sort_default     = GTree.model_sort model in
  let model_sort_name        = GTree.model_sort model in
  let model_sort_name_rev    = GTree.model_sort model in
  let sw                     = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view                   = GTree.view ~model:model_sort_default ~headers_visible:false ~packing:sw#add ~width:350 ~height:500 () in
  let renderer_pixbuf        = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
  let renderer_markup        = GTree.cell_renderer_text [`YPAD 0] in
  let vc                     = GTree.view_column () in
  let _                      = vc#pack ~expand:false renderer_pixbuf in
  let _                      = vc#pack ~expand:false renderer_markup in
  let _                      = vc#add_attribute renderer_pixbuf "pixbuf" col_icon in
  let _                      = vc#add_attribute renderer_markup "markup" col_markup in

  let _                      = view#selection#set_mode `SINGLE in
  let _                      = view#append_column vc in
  let _                      = view#misc#set_name "outline_treeview" in
  let _                      = view#misc#set_property "enable-tree-lines" (`BOOL true) in
  let label_tooltip          = ref (GMisc.label ~markup:" " ()) in
  let buffer : Ocaml_text.buffer = page#buffer in
object (self)
  inherit GObj.widget vbox#as_widget

  val changed = new changed()
  val mutable last_selected_path = None
  val mutable signal_selection_changed = None
  val mutable timestamp = "", 0.0
  val mutable filename = ""
  val mutable table_collapsed_by_default = []
  val mutable table_expanded_by_user = []
  val mutable table_expanded_by_default = [] (* child_paths *)
  val table_info = Hashtbl.create 17
  val mutable selected_path = None
  val mutable count = 0
  val mutable type_color = ""
  val mutable type_color_sel_re = dummy_re
  val mutable type_color_re = dummy_re
  val mutable type_color_sel = ""
  val mutable span_type_color = ""

  method update_preferences () =
    let pref = Preferences.preferences#get in
    view#misc#modify_font_by_name pref.Preferences.pref_compl_font;
    view#misc#modify_base [
      `NORMAL,   `NAME pref.Preferences.pref_outline_color_nor_bg;
      `SELECTED, `NAME pref.Preferences.pref_outline_color_sel_bg;
      `ACTIVE,   `NAME pref.Preferences.pref_outline_color_act_bg;
    ];
    view#misc#modify_text [
      `NORMAL,   `NAME pref.Preferences.pref_outline_color_nor_fg;
      `SELECTED, `NAME pref.Preferences.pref_outline_color_sel_fg;
      `ACTIVE,   `NAME pref.Preferences.pref_outline_color_act_fg;
    ];
    type_color <- pref.Preferences.pref_outline_color_types;
    type_color_re <- Str.regexp_string type_color;
    type_color_sel <- Color.name_of_gdk (view#misc#style#fg `SELECTED);
    type_color_sel_re <- Str.regexp_string type_color_sel;
    span_type_color <- " <span color='" ^ type_color ^ "'>: ";
    let style_outline, apply_outline = Gtk_theme.get_style_outline pref in
    GtkMain.Rc.parse_string (style_outline ^ "\n" ^ apply_outline);
    view#set_rules_hint (pref.Preferences.pref_outline_color_alt_rows <> None);
    GtkBase.Widget.queue_draw view#as_widget;

  initializer
    self#update_preferences();
    ignore (self#misc#connect#destroy ~callback:self#destroy_marks);
    (* Replace foreground color when row is selected *)
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
          let smodel = self#get_model () in
          let path = smodel#convert_path_to_child_path path in
          Gaux.may last_selected_path ~f:(replace_color_in_markup model true);
          replace_color_in_markup model false path;
          last_selected_path <- Some path;
        | _ -> ()
    end);
    (* Tooltips *)
    view#misc#set_has_tooltip true;
    ignore (view#misc#connect#query_tooltip ~callback:self#create_tooltip);
    (* Events *)
    signal_selection_changed <- Some (view#selection#connect#after#changed ~callback:begin fun () ->
      self#select_name_in_buffer();
      match view#selection#get_selected_rows with
        | path :: _ ->
          let smodel = self#get_model () in
          let path = smodel#convert_path_to_child_path path in
          let row = model#get_iter path in
          ignore (self#force_lazy row);
          selected_path <- Some (self#get_id_path row)
        | _ -> ()
    end);
    ignore (view#connect#after#row_activated ~callback:begin fun _ _ ->
      self#select_name_in_buffer();
      Gmisclib.Idle.add page#view#misc#grab_focus;
    end);
    ignore (view#connect#row_expanded ~callback:begin fun row path ->
      let model = self#get_model() in
      let row = model#convert_iter_to_child_iter row in
      let path = model#convert_path_to_child_path path in
      self#add_table_expanded_by_user (self#get_id_path row) path
    end);
    ignore (view#connect#row_collapsed ~callback:begin fun row _ ->
      let model = self#get_model() in
      let row = model#convert_iter_to_child_iter row in
      table_expanded_by_user <- List.remove_assoc (self#get_id_path row) table_expanded_by_user;
    end);
    ignore (view#misc#connect#realize ~callback:begin fun () ->
      let show = pref.Preferences.pref_outline_show_types in
      if show <> button_show_types#active then button_show_types#clicked()
    end);
    (* Buttons *)
    ignore (button_refresh#connect#clicked ~callback:self#load);
    ignore (button_select_buf#connect#clicked ~callback:self#select_in_buffer);
    ignore (button_show_types#connect#toggled ~callback:begin fun () ->
      model#foreach begin fun path row ->
        let name = model#get ~row ~column:col_name in
        try
          let info = Hashtbl.find table_info path in
          let markup = self#create_markup ?kind:info.kind name info.typ in
          model#set ~row ~column:col_markup markup;
          false
        with Not_found -> false
      end;
      pref.Preferences.pref_outline_show_types <- button_show_types#active;
      (*Preferences.save();*)
    end);
    ignore (button_select_from_buf#connect#clicked ~callback:begin fun () ->
      ignore (self#select_from_buffer ?align:None (page#buffer#get_mark `INSERT))
    end);
    (* Sort *)
    (*model_sort_default#set_sort_column_id col_default_sort.GTree.index `ASCENDING;
    model_sort_name#set_sort_column_id col_name.GTree.index `ASCENDING;
    model_sort_name_rev#set_default_sort_func self#compare_name_rev;*)
    let signal_button_sort : GtkSignal.id option ref = ref None in
    let signal_button_sort_rev : GtkSignal.id option ref = ref None in
    signal_button_sort := Some (button_sort#connect#after#toggled ~callback:begin fun () ->
      Gaux.may !signal_button_sort_rev ~f:button_sort_rev#misc#handler_block;
      button_sort_rev#set_active false;
      Gaux.may !signal_button_sort_rev ~f:button_sort_rev#misc#handler_unblock;
      let model = if button_sort#active then model_sort_name else model_sort_default in
      view#set_model (Some model#coerce);
      List.iter begin fun path ->
        let path = model#convert_child_path_to_path path in
        view#expand_row path
      end (table_expanded_by_default @ (List.map (fun (_, x) -> x) table_expanded_by_user));
    end);
    signal_button_sort_rev := Some (button_sort_rev#connect#after#toggled ~callback:begin fun () ->
      Gaux.may !signal_button_sort ~f:button_sort#misc#handler_block;
      button_sort#set_active false;
      Gaux.may !signal_button_sort ~f:button_sort#misc#handler_unblock;
      let model = if button_sort_rev#active then model_sort_name_rev else model_sort_default in
      view#set_model (Some model#coerce);
      List.iter begin fun path ->
        let path = model#convert_child_path_to_path path in
        view#expand_row path
      end (table_expanded_by_default @ (List.map (fun (_, x) -> x) table_expanded_by_user));
    end);
    Preferences.preferences#connect#changed ~callback:begin fun _ ->
      self#update_preferences ()
    end |> ignore;

  method view = view

  method select_from_buffer ?(align : float option) (mark : Gtk.text_mark) =
    let iter = buffer#get_iter_at_mark (`MARK mark) in
    let found = ref None in
    model#foreach begin fun path _ ->
      try
        let info = Hashtbl.find table_info path in
        match info.mark with
          | Some mark ->
            let i = buffer#get_iter_at_mark (`MARK mark) in
            if i#compare iter > 0 then true
            else begin
              found := Some path;
              false
            end
          | _ -> false
      with Not_found -> false
    end;
    let is_selected = ref false in
    Gaux.may !found ~f:begin fun path ->
      let row = model#get_iter path in
      let has_lazy = self#force_lazy row in
      if has_lazy then is_selected := self#select_from_buffer ?align mark else begin
        let smodel = self#get_model () in
        let path = smodel#convert_child_path_to_path path in
        view#expand_to_path path;
        Gaux.may signal_selection_changed ~f:view#selection#misc#handler_block;
        view#selection#select_path path;
        Gaux.may signal_selection_changed ~f:view#selection#misc#handler_unblock;
        is_selected := true;
        match align with
          | Some align ->
            view#vadjustment#set_value (align *. view#vadjustment#upper);
          | None when page#view#misc#get_flag `HAS_FOCUS ->
            if not (Gmisclib.Util.treeview_is_path_onscreen view path) then begin
              view#scroll_to_cell ~align:(0.38, 0.) path vc;
            end;
          | _ -> ()
      end
    end;
    !is_selected

  method select_in_buffer () =
    match view#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
        let _, ts = timestamp in
        if ts = (Unix.stat filename).Unix.st_mtime then begin
          let smodel = self#get_model () in
          let child_path = smodel#convert_path_to_child_path path in
          try
            let info = Hashtbl.find table_info child_path in
            Gaux.may info.body ~f:begin fun loc ->
              let start, stop = Binannot.linechar_of_loc loc in
              let start = buffer#get_iter (`LINECHAR start) in
              let stop = buffer#get_iter (`LINECHAR stop) in
              let start = start#set_line_index 0 in
              let stop = stop#forward_line#set_line_index 0 in
              buffer#select_range stop start;
              page#view#scroll_lazy start;
              page#view#misc#grab_focus();
            end
          with Not_found -> ()
        end

  method select_name_in_buffer () =
    match view#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
        let smodel = self#get_model () in
        let child_path = smodel#convert_path_to_child_path path in
        try
          let info = Hashtbl.find table_info child_path in
          Gaux.may info.location ~f:begin fun _ ->
            match info.mark with
              | Some mark when not (GtkText.Mark.get_deleted mark) ->
                let start = buffer#get_iter_at_mark (`MARK mark) in
                let row = model#get_iter child_path in
                let name = model#get ~row ~column:col_name in
                let length = match [@warning "-4"] info.kind with
                  | Some Class_let_bindings -> 0
                  | _ -> Glib.Utf8.length name in
                let stop = start#forward_chars length in
                buffer#select_range start stop;
                page#view#scroll_lazy start;
              | _ -> ()
          end
        with Not_found -> ()

  method private destroy_marks () =
    buffer#block_signal_handlers ();
    Hashtbl.iter (fun _ info -> match info.mark with Some mark -> buffer#delete_mark (`MARK mark) | _ -> ()) table_info;
    buffer#unblock_signal_handlers ();

  method load () =
    let compile_buffer () = page#compile_buffer ?join:(Some true) () in
    match Binannot.read_cmt ~project:page#project ~filename:page#get_filename ~compile_buffer () with
      | None -> ()
      | Some (_, _, cmt) ->
        filename <- page#get_filename;
        timestamp <- filename, (Unix.stat filename).Unix.st_mtime;
        (*let ext = if filename ^^ ".ml" then Some ".cmt" else if filename ^^ ".mli" then Some ".cmti" else None in
        match ext with
          | Some ext ->
            let filename_cmt = (Filename.chop_extension filename) ^ ext in
            timestamp <- file, (Unix.stat filename).Unix.st_mtime;
            let cmi, cmt = Cmt_format.read filename_cmt in*)
            (* Delete previous marks in the buffer and clear the model and other conatiners *)
          GtkThread2.sync self#destroy_marks ();
          model#clear();
          count <- 0;
          table_expanded_by_default <- [];
          table_collapsed_by_default <- [];
          Hashtbl.clear table_info;
          (* Parse .cmt file *)
          let smodel = self#get_model () in
          view#set_model None;
          (* Disable sort *)
          model#set_sort_column_id (-2) `ASCENDING;
          model_sort_default#set_sort_column_id (-1) `ASCENDING;
          model_sort_name#set_sort_column_id (-1) `ASCENDING;
          model_sort_name_rev#reset_default_sort_func ();
          (* Create tree *)
          self#parse cmt.cmt_annots;
          (*  *)
          view#set_model (Some smodel#coerce);
          (*  *)
          List.iter begin fun path ->
            let path = smodel#convert_child_path_to_path path in
            view#expand_row path
          end table_expanded_by_default;
          (* Select the same row that was selected in the previous tree *)
          Gaux.may selected_path ~f:begin fun sid ->
            GtkThread2.async model#foreach begin fun path row ->
              let id = self#get_id_path row in
              if id = sid then begin
                Gaux.may signal_selection_changed ~f:view#selection#misc#handler_block;
                view#selection#select_path path;
                Gaux.may signal_selection_changed ~f:view#selection#misc#handler_unblock;
                true
              end else false
            end
          end;
          GtkThread2.async begin fun () ->
            model_sort_default#set_sort_column_id col_default_sort.GTree.index `ASCENDING;
            model_sort_name#set_sort_column_id col_name.GTree.index `ASCENDING;
            model_sort_name_rev#set_default_sort_func self#compare_name_rev;
          end ();
          (*| _ -> () *)

  method private parse = function
    | Implementation impl ->
      List.iter self#append_struct_item impl.str_items
    | Partial_implementation _ ->
      (*Array.iter begin function
        | Partial_structure impl -> List.iter self#append_struct_item impl.str_items
        | Partial_structure_item impl -> self#append_struct_item impl
        | Partial_expression expr -> self#append_expression expr
        | Partial_signature sign -> List.iter self#append_sig_item sign.sig_items
        | Partial_signature_item sign -> self#append_sig_item sign
        | Partial_module_type mt -> self#append_module_type mt.mty_desc
        | Partial_pattern _
        | Partial_class_expr _ ->*)

          Log.println `DEBUG "Partial_implementation: %s" page#get_filename;
          (*editor#pack_outline (empty())#coerce*)

          (*let row = model#append () in
          model#set ~row ~column:col_markup "Partial_implementation"*)
      (*end impl;*)
    | Interface sign ->
      List.iter self#append_sig_item sign.sig_items;
    | Partial_interface _ ->
      let row = model#append () in
      model#set ~row ~column:col_markup "Partial_interface"
    | Packed _ ->
      let row = model#append () in
      model#set ~row ~column:col_markup "Packed"

  method private create_markup ?kind name typ =
    let markup_name =
      match [@warning "-4"] kind with
        | Some Class | Some Class_virtual | Some Class_type | Some Module | Some Module_functor | Some Module_include ->
          "<b>" ^ (Glib.Markup.escape_text name) ^ "</b>"
        | Some Initializer | Some Class_let_bindings | Some Method_inherited -> "<i>" ^ (Glib.Markup.escape_text name) ^ "</i>"
        | _ -> Glib.Markup.escape_text name
    in
    let typ_utf8 = Glib.Convert.convert_with_fallback ~fallback:"" ~from_codeset:Oe_config.ocaml_codeset ~to_codeset:"UTF-8" typ in
    if button_show_types#active && typ <> "" then String.concat "" [
      markup_name; span_type_color;
      (Print_type.markup2 (Miscellanea.replace_all ~regexp:true ["\n", ""; " +", " "] typ_utf8));
      "</span>"
    ] else markup_name

  method private create_tooltip ~x ~y ~kbd tooltip =
    try
      begin
        match GtkTree.TreeView.Tooltip.get_context view#as_tree_view ~x ~y ~kbd with
          | (x, y, Some (_, _, row)) ->
            begin
              match view#get_path_at_pos ~x ~y with
                | Some (tpath, _, _, _) ->
                  let model = self#get_model() in
                  let child_path = model#convert_path_to_child_path tpath in
                  let info = Hashtbl.find table_info child_path in
                  if info.typ <> "" then begin
                    let name = model#get ~row ~column:col_name in
                    let markup = sprintf "<span color='darkblue'>%s</span> :\n%s"
                      (Glib.Markup.escape_text name) (Print_type.markup2 info.typ) in
                    (*GtkBase.Tooltip.set_markup tooltip markup;*)
                    (*GtkBase.Tooltip.set_tip_area tooltip (view#get_cell_area ~path:tpath (*~col:vc*) ());*)
                    !label_tooltip#set_label markup;
                    GtkTree.TreeView.Tooltip.set_row view#as_tree_view tooltip tpath;
                    Gaux.may !label_tooltip#misc#parent ~f:(fun _ -> label_tooltip := GMisc.label ~markup ());
                    GtkBase.Tooltip.set_custom tooltip !label_tooltip#as_widget;
                    true
                  end else false
                | _ -> false
            end
          | _ -> false
      end
    with Not_found | Gpointer.Null -> false

  method private append ?parent ?kind ?loc ?loc_body name typ =
    let markup = self#create_markup ?kind name typ in
    let info = {
      typ      = typ;
      kind     = kind;
      location = loc;
      body     = loc_body;
      mark     = None;
    } in
    let path, row =
      (*GtkThread2.sync begin fun () ->*)
        let row = model#append ?parent () in
        let path = model#get_path row in
        model#set ~row ~column:col_name name;
        model#set ~row ~column:col_markup markup;
        model#set ~row ~column:col_default_sort count;
        count <- count + 1;
        Gaux.may kind ~f:(fun k -> Gaux.may (pixbuf_of_kind k) ~f:(model#set ~row ~column:col_icon));
        path, row
     (* end ()*)
    in
    (*GtkThread2.sync begin fun () ->*)
      Gaux.may loc ~f:begin fun loc ->
        let (line, start), _ = Binannot.linechar_of_loc loc in
        if line <= buffer#end_iter#line then begin
          let iter : GText.iter = buffer#get_iter (`LINE line) in
          (*let iter = iter#set_line_index 0 in*)
          let iter = iter#forward_chars start in
          if iter#line = line then begin
            buffer#block_signal_handlers ();
            info.mark <- Some (buffer#create_mark ?name:None ?left_gravity:None iter);
            buffer#unblock_signal_handlers ()
          end
        end
      end;
      Hashtbl.add table_info path info;
    (*end ();*)
    row

  method private append_struct_item ?parent item =
    match item.str_desc with
      | Tstr_eval (expr, _) ->
        let loc = {item.str_loc with loc_end = item.str_loc.loc_start} in
        ignore (self#append ?parent ~loc ~loc_body:expr.exp_loc "_" "")
      | Tstr_value (_, pe) ->
        List.iter (fun { vb_pat = p; vb_expr = e; _ } -> ignore (self#append_pattern ?parent (p, e))) pe
      | Tstr_class classes -> List.iter (self#append_class ?parent) classes
      | Tstr_class_type classes -> List.iter (fun (_, loc, decl) -> self#append_class_type ?parent ~loc decl) classes
      | Tstr_type (_, decls) -> List.iter (self#append_type ?parent) decls
      | Tstr_exception { tyexn_constructor; _ } ->
        let { ext_name = loc; ext_kind; _ } = tyexn_constructor in
        let core_types = ( match ext_kind with
            | Text_decl (Typedtree.Cstr_tuple core_types, _) -> self#string_of_core_types core_types
            | Text_decl (Typedtree.Cstr_record fields, _) ->  self#string_of_type_record fields
            | Text_rebind _ -> "" )
        in
        ignore (self#append ?parent ~kind:Exception ~loc:loc.loc loc.txt core_types)
      | Tstr_module { mb_name = loc; mb_expr = module_expr; _ }  ->
        let kind =
          match [@warning "-4"] module_expr.mod_desc with
            | Tmod_functor _ -> Module_functor
            | _ -> Module
        in
        let txt = Option.value loc.txt ~default:"_" in
        let parent_mod = self#append ?parent ~kind ~loc:loc.loc txt "" in
        let f () = ignore (self#append_module ~parent:parent_mod module_expr.mod_desc) in
        begin
          match parent with
            | Some _ -> model#set ~row:parent_mod ~column:col_lazy [f];
            | _ -> f()
        end;
        self#add_table_expanded_by_default parent_mod;
      | Tstr_modtype (*(_, loc, mt)*) { mtd_name = loc; Typedtree.mtd_type = Some mt; _ } ->
        let parent = self#append ?parent ~kind:Module_type ~loc:loc.loc loc.txt "" in
        model#set ~row:parent ~column:col_lazy [fun () -> ignore (self#append_module_type ~parent mt.mty_desc)];
      (* Since 4.02.0 -- TODO *)
      | Tstr_modtype { Typedtree.mtd_type = None; _ } -> ()
      | Tstr_include { Typedtree.incl_mod = me; incl_type = sign; _ } ->
        let parent = self#append_module ?parent ~kind:Module_include me.mod_desc in
        let f () = List.iter (fun sign -> ignore (self#append_signature_item ?parent sign)) sign in
        begin
          match parent with
            | Some row -> model#set ~row ~column:col_lazy [f];
            | _ -> f ()
        end;
      | Tstr_recmodule _ -> ()
      | Tstr_open _
      | Tstr_primitive _ -> ()
      (* Since 4.02.0 *)
      | Tstr_typext { Typedtree.tyext_txt = txt; tyext_constructors = constructors; _ } ->
        self#append_type_extension ?parent ~kind:Type_open txt constructors
      | Tstr_attribute _ -> ()


  method private append_sig_item ?parent item =
    match item.sig_desc with
      | Tsig_value { Typedtree.val_name; val_loc; val_desc; _ } ->
        Odoc_info.reset_type_names();
        let typ = string_of_type_expr val_desc.ctyp_type in
        ignore (self#append ?parent ~kind:Function ~loc:val_loc ~loc_body:val_desc.ctyp_loc val_name.txt typ);
      | Tsig_type (_, decls) -> List.iter (self#append_type ?parent) decls
      (*| Tsig_exception { Typedtree.ext_kind; ext_name = loc; _ } ->
        let core_types = ( match ext_kind with
            | Text_decl (Typedtree.Cstr_tuple core_types, _) -> self#string_of_core_types core_types
            | Text_decl (Typedtree.Cstr_record fields, _) -> self#string_of_type_record fields
            | Text_rebind _ -> "" )
        in
        ignore (self#append ?parent ~kind:Exception ~loc:loc.loc loc.txt core_types)*)
      (* Changed in 4.08 TODO *)
      | Tsig_exception _exc -> ()
      | Tsig_module { Typedtree.md_type = mty; md_name = loc; _ } ->
        let kind =
          let [@warning "-4"] _ = "Disable this pattern matching is fragile warning" in
          match [@warning "-4"] mty.mty_desc with
            | Tmty_functor _ -> Module_functor
            | _ -> Module
        in
        let txt = Option.value loc.txt ~default:"_" in
        let parent_mod = self#append ?parent ~kind ~loc:loc.loc txt "" in
        let f () = ignore (self#append_module_type ~parent:parent_mod mty.mty_desc) in
        begin
          match parent with
            | Some _ -> model#set ~row:parent_mod ~column:col_lazy [f];
            | _ -> f()
        end;
        self#add_table_expanded_by_default parent_mod;
      | Tsig_recmodule _ -> ignore (self#append ?parent "Tsig_recmodule" "")
      | Tsig_modtype { Typedtree.mtd_name = loc; mtd_type = modtype_decl; _ } ->
        let parent = self#append ?parent ~kind:Module_type ~loc:loc.loc loc.txt "" in
        let f () =
          match modtype_decl with
            | None -> ()
            | Some mt -> ignore (self#append_module_type ~parent mt.mty_desc)
        in
        model#set ~row:parent ~column:col_lazy [f];
      | Tsig_open _ -> ()
      | Tsig_include idecl ->
        ignore (self#append_module_type ?parent ~kind:Module_include idecl.incl_mod.mty_desc);
      | Tsig_class classes ->
        List.iter (fun clty -> self#append_class_type ?parent ~loc:clty.ci_id_name clty) classes;
      | Tsig_class_type decls ->
        List.iter (fun info -> self#append_class_type ?parent ~loc:info.ci_id_name info) decls;
      (* Since 4.02.0 *)
      | Tsig_typext _ -> ()
      | Tsig_attribute _ -> ()
      (* Since 4.08.0 *)
      | Tsig_typesubst _ -> ()
      | Tsig_modsubst _ -> ()

  method private append_module ?parent ?kind mod_desc =
    match mod_desc with
      | Tmod_functor (_, mexpr) ->
        self#append_module ?parent mexpr.mod_desc
      | Tmod_structure str ->
        List.iter (self#append_struct_item ?parent) str.str_items;
        None
      | Tmod_ident (_, loc) -> Some (self#append ?parent ?kind ~loc:loc.loc (string_of_longident loc.txt) "")
      | Tmod_apply (me1, me2, _) ->
        let p1 = self#append_module ?parent ~kind:Module_functor me1.mod_desc in
        let f () =
          let p2 = self#append_module ?parent:p1 me2.mod_desc in
          ignore (self#append_mty ?parent:p2 me2.mod_type);
          ignore (self#append_mty ?parent:p1 me1.mod_type)
        in
        Gaux.may p1 ~f:(fun row -> model#set ~row ~column:col_lazy [f]);
        None
      | Tmod_constraint _ -> Some (self#append ?parent "Tmod_constraint" "")
      | Tmod_unpack _ -> Some (self#append ?parent "Tmod_unpack" "")

  (* TODO: Handle visibiliy added in 4.08 *)
  method private append_signature_item ?parent ?kind:_ =
    function
    | Sig_value (id, vd, _) -> Some (self#append ?parent ~kind:Simple (Ident.name id) (string_of_type_expr vd.val_type))
    | Sig_type (id, _, _, _) -> Some (self#append ?parent (*~kind:Type*) (Ident.name id) "")
    | Sig_typext (id, _, _, _) -> Some (self#append ?parent ~kind:Exception (Ident.name id) "")
    | Sig_module (_, _, mdecl, _, _) -> self#append_mty ?parent ~kind:Module_type mdecl.md_type
    | Sig_modtype (_, _, _) -> Some (self#append ?parent "Sig_modtype" "")
    | Sig_class (_, _, _, _) -> Some (self#append ?parent "Sig_class" "")
    | Sig_class_type (_, _, _, _) -> Some (self#append ?parent "Sig_class_type" "")

  method private append_mty ?parent ?kind mod_type =
    match mod_type with
      | Mty_ident _ -> Some (self#append ?parent ?kind "Mty_ident" "")
      | Mty_signature sign_items ->
        List.iter (fun x -> ignore (self#append_signature_item ?parent ?kind x)) sign_items;
        None
      | Mty_functor (_, m2) ->
        self#append_mty ?parent ?kind m2
        (*ignore (self#append ?parent (Ident.name id) "")*)
      (* Since 4.02.0 - TODO *)
      | Mty_alias _ -> None

  method private append_module_type ?parent ?kind:_ mod_desc =
    match mod_desc with
      | Tmty_functor (_, mexpr) ->
        self#append_module_type ?parent mexpr.mty_desc
      | Tmty_ident (path, loc) ->
        Some (self#append ?parent ~kind:Module_include ~loc:loc.loc (Path.name path) "")
      | Tmty_signature sign ->
        List.iter (self#append_sig_item ?parent) sign.sig_items;
        None
      | Tmty_with _ -> Some (self#append ?parent "Tmty_with" "")
      | Tmty_typeof me -> self#append_module ?parent me.mod_desc
      (* Since 4.02.0 -- TODO *)
      | Tmty_alias _ -> None

  method private append_type ?parent decl =
    let loc = decl.typ_name in
    let kind : kind =
      match decl.typ_kind with
        | Ttype_abstract -> Type_abstract
        | Ttype_variant _ -> Type_variant
        | Ttype_record _ -> Type_record
        | Ttype_open -> Type_open
    in
    let typ =
      match decl.typ_kind with
        | Ttype_abstract -> self#string_of_type_abstract decl
        | Ttype_variant decls -> self#string_of_type_variant decls
        | Ttype_record decls -> self#string_of_type_record decls
        (* Since 4.02.0 *)
        | Ttype_open -> ".."
    in
    ignore (self#append ?parent ~kind ~loc:loc.loc ~loc_body:decl.typ_loc loc.txt typ)

  method private append_type_extension ?parent ~kind { txt; loc } constructors =
    let typs ext_kind = match ext_kind with
      | Text_decl (Typedtree.Cstr_tuple ctl, cto) ->
        let args = self#string_of_core_types ctl in
        let res = self#string_of_core_type_opt cto in
        self#repr_of_gadt_type args res
      | Text_decl (Typedtree.Cstr_record fields, cto) ->
        let args = self#string_of_type_record fields in
        let res = self#string_of_core_type_opt cto in
        self#repr_of_gadt_type args res
      | Text_rebind (_, { Asttypes.txt; _ }) -> " = " ^ (string_of_longident txt)
    in
    let name_and_types = List.map (fun { Typedtree.ext_name = { txt; _ }; ext_kind; _ } -> txt ^ (typs ext_kind)) constructors in
    let repr = "+ " ^ String.concat " | " name_and_types in
    ignore (self#append ?parent ~kind ~loc (string_of_longident txt) repr)

  method private append_pattern ?parent (pat, expr) =
    match [@warning "-4"] pat.pat_desc with
      | Tpat_var (_, loc) ->
        let kind = if is_function pat.pat_type then Function else Simple in
        Odoc_info.reset_type_names();
        let parent = self#append ?parent ~kind ~loc:loc.loc ~loc_body:expr.exp_loc loc.txt (string_of_type_expr pat.pat_type) in
        Some parent
      | Tpat_tuple pats ->
        List.iter (fun p -> ignore (self#append_pattern ?parent (p, expr))) pats;
        None
      (*| Tpat_alias (pat, _, loc) -> self#append_pattern ?parent (pat, expr)*)
      | (*Tpat_construct _ | Tpat_any | Tpat_alias _ | Tpat_constant _
      | Tpat_variant _ | Tpat_record _ | Tpat_array _ | Tpat_or _
      | Tpat_lazy*) _ -> None (*Some (self#append ?parent "append_pattern" "")*)

  method private append_expression ?parent exp =
    match [@warning "-4"] exp.exp_desc with
      | Texp_ident (_, lid, _) -> ignore (self#append ?parent (Longident.last lid.txt) "")
      | Texp_function { cases; _ } ->
        List.iter (fun { c_lhs = p; c_rhs = e; _ } -> ignore (self#append_pattern ?parent (p, e))) cases
      | Texp_let (_, pes, _) ->
        List.iter (fun { vb_pat = p; vb_expr = e; _ } -> ignore (self#append_pattern ?parent (p, e))) pes
      | Texp_apply (expr, _) -> self#append_expression ?parent expr
      | Texp_sequence _ -> ignore (self#append ?parent "Texp_sequence" "")
      | _ -> ignore (self#append ?parent "x" "")

  method private append_class ?parent (infos, _) =
    let kind = if infos.ci_virt = Asttypes.Virtual then Class_virtual else Class in
    let parent = self#append ~kind ?parent ~loc:infos.ci_id_name.loc infos.ci_id_name.txt "" in
    let loc = {infos.ci_expr.cl_loc with loc_end = infos.ci_expr.cl_loc.loc_start} in
    let let_bindings_parent = self#append ~kind:Class_let_bindings ~loc ~parent "<let-bindings>" "" in
    let count_meth = ref 0 in
    let id_path = self#get_id_path let_bindings_parent in
    let expand_lets = List_opt.assoc id_path table_expanded_by_user <> None in
    ignore (self#append_class_item ~let_bindings_parent ~expand_lets ~count_meth ~parent infos.ci_expr.cl_desc);
    (*let not_has_childs =
      try
        model#iter_n_children (Some let_bindings_parent) = 0 &&
        (model#get ~row:let_bindings_parent ~column:col_lazy) = []
      with Failure _ -> true
    in
    (* DO NOT REMOVE ROWS FROM THE MODEL TO NOT ALTER PATHS IN INDEXES. *)
    if not_has_childs then (ignore (model#remove let_bindings_parent))
    else *)if not expand_lets then begin
      let path = model#get_path let_bindings_parent in
      self#add_table_collapsed_by_default id_path path;
    end;
    if !count_meth > 0 then (self#add_table_expanded_by_default parent)

  method private append_class_item ?let_bindings_parent ?(expand_lets=false) ?count_meth ?parent = function
    | Tcl_structure str ->
      List.map begin fun fi ->
        match fi.cf_desc with
          | Tcf_inherit (_, cl_expr, _, _, inherited_methods) ->
            let parent = self#append_class_item ?let_bindings_parent ~expand_lets ?count_meth ?parent cl_expr.cl_desc in
            Gaux.may parent ~f:begin fun parent ->
              let f () = List.iter (fun (x, _) -> ignore (self#append ~kind:Method_inherited ~parent x "")) inherited_methods in
              let id_path = self#get_id_path parent in
              let expand_inher = List_opt.assoc id_path table_expanded_by_user <> None in
              if expand_inher then begin
                f();
                self#add_table_expanded_by_default parent;
              end else begin
                self#add_table_collapsed_by_default (self#get_id_path parent) (model#get_path parent);
                model#set ~row:parent ~column:col_lazy [f];
              end
            end;
            parent
          | Tcf_initializer _ ->
            let loc = {fi.cf_loc with loc_end = fi.cf_loc.loc_start} in
            let parent = self#append ~kind:Initializer ~loc ~loc_body:fi.cf_loc ?parent "initializer" "" in
            Some parent;
          | Tcf_val (loc, mutable_flag, _, kind, _) ->
            let typ, kind = match kind with
              | Tcfk_virtual ct when mutable_flag = Mutable ->
                string_of_type_expr ct.ctyp_type, Attribute_mutable_virtual
              | Tcfk_virtual ct  ->
                string_of_type_expr ct.ctyp_type, Attribute_virtual
              (* Since 4.02.0 -- TODO handle _override *)
              | Tcfk_concrete (_override, te) when mutable_flag = Mutable ->
                string_of_type_expr te.exp_type, Attribute_mutable
              | Tcfk_concrete (_override, te) ->
                string_of_type_expr te.exp_type, Attribute
            in
            Some (self#append ?parent ~kind ~loc:loc.loc loc.txt typ);
          | Tcf_method (loc, private_flag, kind) ->
            let kind, typ, loc_body = match kind with
              | Tcfk_virtual ct when private_flag = Private ->
                Method_private_virtual, string_of_type_expr ct.ctyp_type, ct.ctyp_loc
              | Tcfk_virtual ct ->
                Method_virtual, string_of_type_expr ct.ctyp_type, ct.ctyp_loc
              (* Since 4.02.0 -- TODO handle _override *)
              | Tcfk_concrete (_override, te) when private_flag = Private ->
                Method_private, string_of_type_expr te.exp_type, te.exp_loc
              | Tcfk_concrete (_override, te) ->
                Method, string_of_type_expr te.exp_type, te.exp_loc
            in
            Gaux.may count_meth ~f:incr;
            Some (self#append ?parent ~kind ~loc:loc.loc ~loc_body:loc_body loc.txt typ);
          | Tcf_constraint (ct1, _) -> Some (self#append ?parent ~loc:ct1.ctyp_loc (sprintf "Tcf_constr") "");
          (* Added in 4.02.3 - TODO *)
          | Tcf_attribute _ -> None
      end str.cstr_fields |> ignore;
      parent
    | Tcl_fun (_, _, _, cl_expr, _) ->
      self#append_class_item ?let_bindings_parent ~expand_lets ?count_meth ?parent cl_expr.cl_desc;
    | Tcl_ident (_, lid, _) ->
      Some (self#append ?parent ~kind:Class_inherit ~loc:lid.loc (string_of_longident lid.txt) "");
    | Tcl_apply (cl_expr, _) ->
      self#append_class_item ?let_bindings_parent ~expand_lets ?count_meth ?parent cl_expr.cl_desc;
    | Tcl_let (_, lets, _, desc) ->
      Gaux.may let_bindings_parent ~f:begin fun row ->
        let f () =
          List.iteri begin fun i { vb_pat = p; vb_expr = e; _ } ->
            ignore (self#append_pattern ~parent:row (p, e));
            if i = 0 && expand_lets then begin
              let path = model#get_path row in
              self#add_table_expanded_by_default row;
              self#add_table_expanded_by_user (self#get_id_path row) path
            end
          end lets
        in
        if expand_lets then f() else begin
          let prev = try model#get ~row ~column:col_lazy with Failure _ -> [] in
          model#set ~row ~column:col_lazy (f :: prev)
        end
      end;
      self#append_class_item ?let_bindings_parent ~expand_lets ?count_meth ?parent desc.cl_desc;
    | Tcl_constraint (cl_expr, _, _, _, _) ->
      self#append_class_item ?let_bindings_parent ~expand_lets ?count_meth ?parent cl_expr.cl_desc;
    (* Added in 4.06 *)
    | Tcl_open (_, cl_expr) ->
      self#append_class_item ?let_bindings_parent ~expand_lets ?count_meth ?parent cl_expr.cl_desc

  method private append_class_type ?parent ~loc infos =
    let parent = self#append ~kind:Class_type ?parent ~loc:loc.loc loc.txt "" in
    let count_meth = ref 0 in
    ignore (self#append_class_type_item ~parent ~count_meth infos.ci_expr.cltyp_desc);
    if !count_meth > 0 then (self#add_table_expanded_by_default parent)

  method private append_class_type_item ?parent ?count_meth = function
    | Tcty_constr _ -> ignore (self#append ?parent (sprintf "Tcty_constr") "")
    | Tcty_signature sign ->
      List.iter begin fun field ->
        match field.ctf_desc with
          | Tctf_inherit _ -> ignore (self#append ?parent (sprintf "Tctf_inher") "")
          | Tctf_val (id, mutable_flag, virtual_flag, ct) ->
            let kind = match mutable_flag, virtual_flag with
              | Mutable, Virtual -> Attribute_mutable_virtual
              | Immutable, Virtual -> Attribute_virtual
              | Mutable, Concrete -> Attribute_mutable
              | Immutable, Concrete -> Attribute
            in
            let typ = string_of_type_expr ct.ctyp_type in
            let loc = field.ctf_loc in (* This location is not correct  *)
            ignore(self#append ?parent ~kind ~loc ~loc_body:ct.ctyp_loc id typ);
          | Tctf_method (id, private_flag, _, ct) ->
            Gaux.may count_meth ~f:incr;
            let kind = match [@warning "-4"] private_flag with Private -> Method_private | _ -> Method in
            let loc = field.ctf_loc in (* This location is not correct  *)
            ignore (self#append ?parent ~kind ~loc ~loc_body:ct.ctyp_loc id (string_of_type_expr ct.ctyp_type))
          | Tctf_constraint _ -> ignore (self#append ?parent (sprintf "Tctf_cstr") "")
          (* Added in 4.02.0 - TODO *)
          | Tctf_attribute _ -> ()
      end (List.rev sign.csig_fields)
    | Tcty_arrow (_, _, class_type) -> ignore (self#append_class_type_item ?parent class_type.cltyp_desc)
    (* Added in 4.06 *)
    | Tcty_open (_, class_type) -> ignore (self#append_class_type_item ?parent class_type.cltyp_desc)

  method private string_of_type_abstract decl =
    match decl.typ_manifest with
      | Some ct -> Odoc_info.string_of_type_expr ct.ctyp_type
      | _ -> ""

  method private string_of_type_record decls =
    Odoc_info.reset_type_names();
    String.concat "" ["{ \n  ";
      String.concat ";\n  " (List.map begin fun { Typedtree.ld_name = loc; ld_type = ct; _ } ->
        loc.txt ^ ": " ^ string_of_type_expr ct.ctyp_type
      end decls);
      " \n}"]

  method private string_of_type_variant decls =
    Odoc_info.reset_type_names();
    "   " ^ (String.concat "\n | " (List.map begin fun { Typedtree.cd_name; cd_args; cd_res; _ } ->
      cd_name.txt ^
        let ts = self#string_of_constructor_arguments cd_args in
        let te = self#string_of_core_type_opt cd_res in
        self#repr_of_gadt_type ts te
    end decls))

  method private string_of_constructor_arguments = function
    | Typedtree.Cstr_tuple core_types -> self#string_of_core_types core_types
    | Typedtree.Cstr_record fields -> self#string_of_type_record fields

  method private string_of_core_types ctl =
    String.concat " * " (List.map (fun ct -> string_of_type_expr ct.ctyp_type) ctl)

  method private string_of_core_type_opt = function
    | Some te -> string_of_type_expr te.ctyp_type
    | None -> ""

  method private repr_of_gadt_type type_args type_result =
    match type_args, type_result with
    | "", "" -> ""
    | _, ""  -> " of " ^ type_args
    | "", _  -> " : " ^ type_result
    | _, _   -> " : " ^ type_args ^ " -> " ^ type_result

  method private add_table_expanded_by_user id path =
    table_expanded_by_user <- (id, path) :: (List.remove_assoc id table_expanded_by_user)

  method private add_table_collapsed_by_default id path =
    table_collapsed_by_default <- (id, path) :: (List.remove_assoc id table_collapsed_by_default)

  method private add_table_expanded_by_default row =
    table_expanded_by_default <- (model#get_path row) :: table_expanded_by_default;

  method private compare_default model i1 i2 =
    let o1 = model#get ~row:i1 ~column:col_default_sort in
    let o2 = model#get ~row:i2 ~column:col_default_sort in
    Stdlib.compare o1 o2

  method private compare_name_rev model i1 i2 =
    let name1 = model#get ~row:i1 ~column:col_name in
    let name2 = model#get ~row:i2 ~column:col_name in
    let name1 = string_rev name1 in
    let name2 = string_rev name2 in
    Stdlib.compare name1 name2

  method private force_lazy row =
    let result = ref false in
    begin
      try
        let callbacks = List.rev (model#get ~row ~column:col_lazy) in
        List.iter (fun f -> f()) callbacks;
        result := callbacks <> [];
      with Failure _ -> ()
    end;
    model#set ~row ~column:col_lazy [];
    !result

  method private get_id_path row =
    let rec loop row id =
      match model#iter_parent row with
        | Some parent ->
          loop parent ((model#get ~row:parent ~column:col_name) ^ "." ^ id)
        | _ -> id
    in
    loop row (model#get ~row ~column:col_name)

  method private get_model : unit -> GTree.model_sort = fun () ->
    if view#model#misc#get_oid = model_sort_default#misc#get_oid then model_sort_default
    else if view#model#misc#get_oid = model_sort_name#misc#get_oid then model_sort_name
    else model_sort_name_rev

  method connect = new signals ~changed
end

and changed () = object inherit [string * bool] signal () end
and signals ~changed =
object
  inherit ml_signals [changed#disconnect]
  method changed = changed#connect ~after
end

let create = new widget

let window ~editor ~page () =
  let window = GWindow.window ~position:`CENTER ~show:false () in
  let vbox = GPack.vbox ~packing:window#add () in
  let widget = create ~editor ~page ~packing:vbox#add () in
  ignore (window#event#connect#key_press ~callback:begin fun ev ->
    if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then (window#destroy (); true)
    else false
  end);
  window#present();
  widget, window;;
