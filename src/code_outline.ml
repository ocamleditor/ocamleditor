open Printf
open Settings_j
open Merlin_j
open Preferences

module Log = Common.Log.Make(struct let prefix = "OUTLINE" end)
let _ = Log.set_verbosity `DEBUG

exception Invalid_linechar
exception Break of Gtk.tree_iter

let pixbuf_of_kind = function
  | "Module" -> Some (??? Icons.module_impl)
  | "Type" -> Some (??? Icons.typ)
  | "Value" -> Some (??? Icons.simple)
  | "Exn" -> Some (??? Icons.exc)
  | "Class" -> Some (??? Icons.classe)
  | _ -> None;;


(*| Function -> Some (??? Icons.func)
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
  | Type_abstract -> Some (??? Icons.type_abstract)
  | Type_variant -> Some (??? Icons.type_variant)
  | Type_record -> Some (??? Icons.type_record)
  | Type_open -> Some (??? Icons.type_variant)
  | Class_virtual -> Some (??? Icons.class_virtual)
  | Class_type -> Some (??? Icons.class_type)
  | Class_inherit -> Some (??? Icons.class_inherit)
  | Class_let_bindings -> None
  | Module_functor -> Some (??? Icons.module_funct)
  | Module_type -> Some (??? Icons.module_type)
  | Module_include -> Some (??? Icons.module_include)
  | Error -> Some (??? Icons.error_14)
  | Warning -> Some (??? Icons.warning_14)
  | Folder_warnings -> Some (??? Icons.folder_warning)
  | Folder_errors -> Some (??? Icons.folder_error)
  | Dependencies -> None
  | Bookmark pixbuf -> Some pixbuf*)

let cols               = new GTree.column_list
let col_icon           = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf")
let col_markup         = cols#add Gobject.Data.string
let col_data           : Merlin_j.outline GTree.column = cols#add Gobject.Data.caml
let col_lazy           : (unit -> unit) list GTree.column = cols#add Gobject.Data.caml
let col_default_sort   = cols#add Gobject.Data.int


class widget ~(page : Editor_page.page) ?packing () =
  let pref                   = Preferences.preferences#get in
  let show_types             = pref.outline_show_types in
  let vbox                   = GPack.vbox ?packing () in
  let model                  = GTree.tree_store cols in
  let sw                     = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view                   = GTree.view ~model ~headers_visible:false ~packing:sw#add ~width:350 ~height:500 () in
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
  let outline : (Merlin_j.outline list) GUtil.variable = new GUtil.variable [] in
  let buffer = page#ocaml_view#obuffer in
  object (self)
    inherit GObj.widget vbox#as_widget

    val mutable code_font_family = ""

    initializer
      self#update_preferences();
      Preferences.preferences#connect#changed ~callback:(fun _ -> self#update_preferences ()) |> ignore;
      outline#connect#changed ~callback:begin fun ol ->
        Log.println `DEBUG "changed";
        self#build()
      end |> ignore;
      let sig_selection_changed = view#selection#connect#changed ~callback:self#jump_to in
      buffer#connect#after#mark_set ~callback:begin fun _ mark ->
        match GtkText.Mark.get_name mark with
        | Some "insert" ->
            Gmisclib.Idle.add ~prio:300 begin fun () ->
              view#selection#misc#handler_block sig_selection_changed;
              self#select_from_buffer mark;
              view#selection#misc#handler_unblock sig_selection_changed
            end
        | _ -> ()
      end |> ignore;

    method outline = outline

    method private get_iter_at_line pos =
      let ln = pos.line - 1 in
      if pos.line <= 0 || ln > buffer#end_iter#line then raise Invalid_linechar;
      buffer#get_iter (`LINE ln)

    method private get_iter_at_linechar pos =
      let it = self#get_iter_at_line pos in
      if pos.col >= it#chars_in_line then raise Invalid_linechar;
      it#set_line_offset pos.col (*buffer#get_iter (`LINECHAR (pos.line - 1, pos.col))*)

    method jump_to () =
      match view#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
          begin
            try
              let row = model#get_iter path in
              let ol = model#get ~row ~column:col_data in
              let start = self#get_iter_at_line ol.ol_start in
              buffer#select_range start start;
              page#view#scroll_lazy start;
              Gmisclib.Idle.add ~prio:300 page#view#misc#grab_focus
            with Invalid_linechar ->
              Log.println `ERROR "Invalid line/char"
          end

    method select_in_buffer () =
      match view#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
          begin
            try
              let row = model#get_iter path in
              let ol = model#get ~row ~column:col_data in
              let start = self#get_iter_at_linechar ol.ol_start in
              let stop = self#get_iter_at_linechar ol.ol_stop in
              buffer#select_range start stop
            with Exit ->
              Log.println `ERROR "Invalid line/char"
          end

    method select_from_buffer ?(align : float option) (mark : Gtk.text_mark) =
      if self#misc#get_flag `VISIBLE then begin
        let iter = buffer#get_iter_at_mark (`MARK mark) in
        let is_found = ref None in
        model#foreach begin fun path row ->
          let ol = model#get ~row ~column:col_data in
          if ol.ol_start.line - 1 <= iter#line && iter#line <= ol.ol_stop.line - 1 then begin
            view#selection#select_iter row;
            is_found := Some path;
            true
          end else false
        end;
        match !is_found with
        | None -> view#selection#unselect_all()
        | Some path -> begin
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

    method build () =
      model#clear();
      let steps =
        outline#get
        |> List.rev
        |> List.map begin fun ol ->
          fun () ->
            let row = model#append () in
            model#set ~row ~column:col_data ol;
            pixbuf_of_kind ol.ol_kind |> Option.iter (model#set ~row ~column:col_icon);
            model#set ~row ~column:col_markup ol.ol_name;
        end
      in
      Gmisclib.Idle.idleize_cascade ~prio:300 steps ()

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
      let style_outline, apply_outline = Gtk_theme.get_style_outline pref in
      GtkMain.Rc.parse_string (style_outline ^ "\n" ^ apply_outline);
      view#set_rules_hint (pref.outline_color_alt_rows <> None);
      let base_font = pref.editor_base_font in
      code_font_family <-
        String.sub base_font 0 (Option.value (String.rindex_opt base_font ' ') ~default:(String.length base_font));
      GtkBase.Widget.queue_draw view#as_widget;

  end
