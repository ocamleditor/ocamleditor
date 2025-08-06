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

open Printf
module ColorOps = Color
open Preferences
open GUtil

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
  | Function -> Some (??? Icons.func)
  | Simple -> Some (??? Icons.simple)
  | Method -> Some (??? Icons.met)
  | Method_private -> Some (??? Icons.met_private)
  | Method_virtual -> Some (??? Icons.met_virtual)
  | Method_private_virtual -> Some (??? Icons.met_private_virtual)
  | Method_inherited -> Some (??? Icons.met)
  | Initializer -> Some (??? Icons.init)
  | Attribute -> Some (??? Icons.attribute)
  | Attribute_mutable -> Some (??? Icons.attribute_mutable)
  | Attribute_mutable_virtual -> Some (??? Icons.attribute_mutable_virtual)
  | Attribute_virtual -> Some (??? Icons.attribute_virtual)
  | Type -> Some (??? Icons.typ)
  | Type_abstract -> Some (??? Icons.type_abstract)
  | Type_variant -> Some (??? Icons.type_variant)
  | Type_record -> Some (??? Icons.type_record)
  | Type_open -> Some (??? Icons.type_variant)
  | Class -> Some (??? Icons.classe)
  | Class_virtual -> Some (??? Icons.class_virtual)
  | Class_type -> Some (??? Icons.class_type)
  | Class_inherit -> Some (??? Icons.class_inherit)
  | Class_let_bindings -> None
  | Module -> Some (??? Icons.module_impl)
  | Module_functor -> Some (??? Icons.module_funct)
  | Module_type -> Some (??? Icons.module_type)
  | Module_include -> Some (??? Icons.module_include)
  | Exception -> Some (??? Icons.exc)
  | Error -> Some (??? Icons.error_14)
  | Warning -> Some (??? Icons.warning_14)
  | Folder_warnings -> Some (??? Icons.folder_warning)
  | Folder_errors -> Some (??? Icons.folder_error)
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
(*let col_icon           = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf")*)
let col_name           = cols#add Gobject.Data.string
let col_markup         = cols#add Gobject.Data.string
let col_lazy           : (unit -> unit) list GTree.column = cols#add Gobject.Data.caml
let col_default_sort   = cols#add Gobject.Data.int

let string_rev str =
  let len = String.length str in
  Bytes.init len (fun i -> str.[len - i - 1])
  |> Bytes.to_string

let string_rev = Utils.Memo.create string_rev;;

let is_function type_expr =
  let rec f t =
    match [@warning "-4"] Types.get_desc t with
    | Types.Tarrow _ -> true
    | Types.Tlink t -> f t
    | _ -> false
  in f type_expr;;

let string_of_type_expr ?(is_method=false) te =
  let te = match Types.get_desc te with
    | Tarrow (_, te1, te2, _) when is_method -> te2
    | _ -> te
  in
  Odoc_info.string_of_type_expr te

let string_of_longident t
  = String.concat "." @@ Longident.flatten t

(** empty *)
let empty () =
  let pref = Preferences.preferences#get in
  let vp = GBin.viewport () in
  let label = GMisc.label ~xalign:0.5 ~yalign:0. ~xpad:3 ~ypad:3
      ~text:"Structure is not available" ~packing:vp#add () in
  vp#misc#modify_bg [`NORMAL, `NAME ?? (pref.outline_color_nor_bg)];
  label#misc#modify_fg [
    `NORMAL, `NAME ?? (pref.outline_color_nor_fg)
  ];
  vp#coerce;;

let dummy_re = Str.regexp ""

(** widget *)
class widget ~editor:_ ~page ?packing () =
  let pref                   = Preferences.preferences#get in
  let show_types             = pref.outline_show_types in
  let vbox                   = GPack.vbox ?packing () in
  let toolbar                = GPack.hbox ~spacing:0 ~packing:vbox#pack ~show:true () in
  let button_refresh         = GButton.button ~relief:`NONE ~packing:toolbar#pack () in
  let button_show_types      = GButton.toggle_button ~active:show_types ~relief:`NONE ~packing:toolbar#pack () in
  let button_sort            = GButton.toggle_button ~relief:`NONE ~packing:toolbar#pack () in
  let button_sort_rev        = GButton.toggle_button ~relief:`NONE ~packing:toolbar#pack () in
  let button_select_from_buf = GButton.button ~relief:`NONE ~packing:toolbar#pack () in
  let button_select_buf      = GButton.button ~relief:`NONE ~packing:toolbar#pack () in
  let _                      = button_refresh#add (Gtk_util.label_icon "\u{eb37}")#coerce in
  let _                      = button_sort#add (Gtk_util.label_icon "\u{f15d}")#coerce in
  let _                      = button_sort_rev#add (Gtk_util.label_icon "\u{f1860}")#coerce in
  let _                      = button_show_types#add (Gtk_util.label_icon "\u{03c4}")#coerce in
  let _                      = button_select_buf#add (Gtk_util.label_icon "<span size='xx-small'>\u{f061}</span>\u{f09ee}")#coerce in
  let _                      = button_select_from_buf#add (Gtk_util.label_icon "\u{f13d2}<span size='xx-small'>\u{f060}</span>")#coerce  in
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
  let sw                     = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view                   = GTree.view ~model:model_sort_default ~headers_visible:false ~packing:sw#add ~width:350 ~height:500 () in
  let renderer_pixbuf        = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
  let renderer_markup        = GTree.cell_renderer_text [`YPAD 0] in
  let vc                     = GTree.view_column () in
  let _                      = vc#pack ~expand:false renderer_pixbuf in
  let _                      = vc#pack ~expand:false renderer_markup in
  (*let _                      = vc#add_attribute renderer_pixbuf "pixbuf" col_icon in*)
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
    val mutable code_font_family = ""

    method update_preferences () =
      let pref = Preferences.preferences#get in
      view#misc#modify_font_by_name pref.editor_completion_font;
      view#misc#modify_base [
        `NORMAL,   `NAME ?? (pref.outline_color_nor_bg);
        `SELECTED, `NAME ?? (pref.outline_color_sel_bg);
        `ACTIVE,   `NAME ?? (pref.outline_color_act_bg);
      ];
      view#misc#modify_text [
        `NORMAL,   `NAME ?? (pref.outline_color_nor_fg);
        `SELECTED, `NAME ?? (pref.outline_color_sel_fg);
        `ACTIVE,   `NAME ?? (pref.outline_color_act_fg);
      ];
      type_color <- ?? (pref.outline_color_types);
      type_color_re <- Str.regexp_string type_color;
      type_color_sel <- ColorOps.name_of_gdk (view#misc#style#fg `SELECTED);
      type_color_sel_re <- Str.regexp_string type_color_sel;
      span_type_color <- " <span color='" ^ type_color ^ "'>: ";
      let style_outline, apply_outline = Gtk_theme.get_style_outline pref in
      GtkMain.Rc.parse_string (style_outline ^ "\n" ^ apply_outline);
      view#set_rules_hint (pref.outline_color_alt_rows <> None);
      let base_font = pref.editor_base_font in
      code_font_family <-
        String.sub base_font 0 (Option.value (String.rindex_opt base_font ' ') ~default:(String.length base_font));
      GtkBase.Widget.queue_draw view#as_widget;

    initializer
      self#update_preferences();
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
          let show = pref.outline_show_types in
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
          pref.outline_show_types <- button_show_types#active;
          (*Preferences.save();*)
        end);
      button_select_from_buf#connect#clicked ~callback:begin fun () ->
        self#select_from_buffer ~align:0.5 (page#buffer#get_mark `INSERT) |> ignore;
      end |> ignore;
      let start_auto_select_from_buffer _ =
        GMain.Timeout.add ~ms:1500 ~callback:begin fun () ->
          Gmisclib.Idle.add ~prio:300 (fun () ->
              self#select_from_buffer ?align:None (page#buffer#get_mark `INSERT) |> ignore);
          self#misc#get_flag `VISIBLE && page#view#misc#get_flag `HAS_FOCUS
        end |> ignore;
        false
      in
      start_auto_select_from_buffer() |> ignore;
      page#view#event#connect#focus_in ~callback:start_auto_select_from_buffer |> ignore;
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

    method load () = ()

    method select_from_buffer ?(align : float option) (mark : Gtk.text_mark) =
      if self#misc#get_flag `VISIBLE then begin
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
          let smodel = self#get_model () in
          let path = smodel#convert_child_path_to_path path in
          match view#selection#get_selected_rows with
          | sel_path :: _ when sel_path = path && align = None -> is_selected := true
          | _ -> begin
              let row = model#get_iter path in
              let has_lazy = self#force_lazy row in
              if has_lazy then is_selected := self#select_from_buffer ?align mark else begin
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
            end
        end;
        !is_selected
      end else false

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
                (*buffer#select_range stop start;
                  page#view#scroll_lazy start;*)
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

    method private create_markup ?kind name typ =
      let markup_name =
        match [@warning "-4"] kind with
        | Some Class | Some Class_virtual | Some Class_type | Some Module | Some Module_functor | Some Module_include ->
            "<b>" ^ (Glib.Markup.escape_text name) ^ "</b>"
        | Some Initializer | Some Class_let_bindings | Some Method_inherited -> "<i>" ^ (Glib.Markup.escape_text name) ^ "</i>"
        | _ when Str.string_match (Utils.regexp "[a-zA-Z_]+") name 0 -> Glib.Markup.escape_text name
        (* Allows you to show ligatures in operators *)
        | _ -> "<span face='" ^ code_font_family ^ "'>" ^ Glib.Markup.escape_text name ^ "</span>"
      in
      let typ_utf8 = Glib.Convert.convert_with_fallback ~fallback:"" ~from_codeset:Oe_config.ocaml_codeset ~to_codeset:"UTF-8" typ in
      if button_show_types#active && typ <> "" then String.concat "" [
          markup_name; span_type_color;
          (Print_type.markup2 (Utils.replace_all ~regexp:true ["\n", ""; " +", " "] typ_utf8));
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

    method private add_table_expanded_by_user id path =
      table_expanded_by_user <- (id, path) :: (List.remove_assoc id table_expanded_by_user)

    method private add_table_collapsed_by_default id path =
      table_collapsed_by_default <- (id, path) :: (List.remove_assoc id table_collapsed_by_default)

    method private add_table_expanded_by_default row =
      table_expanded_by_default <- (model#get_path row) :: table_expanded_by_default;

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
