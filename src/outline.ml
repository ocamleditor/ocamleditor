open Printf
open Settings_j
open Merlin_j
open Preferences

module Log = Common.Log.Make(struct let prefix = "OUTLINE" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `WARN

exception Invalid_linechar of Merlin_j.pos
exception Break of Gtk.tree_iter

open GUtil

class model ~(buffer : Ocaml_text.buffer) () =
  let merlin text func = func ~filename:buffer#filename ~buffer:text in
  object (self)
    val mutable outline = []
    val mutable outline_hash = 0
    val mutable timer_id = None
    val mutable last_refresh_time = 0.0
    val changed = new changed()

    method get = outline

    method attach = self#start_timer

    method detach = self#stop_timer

    method is_valid = buffer#last_edit_time < last_refresh_time

    method update ?(force=false) () =
      (*Log.println `DEBUG "invoke_merlin %b %s" self#is_valid (Filename.basename buffer#filename);*)
      if not self#is_valid || force then begin
        let source_code = buffer#get_text () in
        last_refresh_time <- Unix.gettimeofday();
        (merlin source_code)@@Merlin.outline
        |> Async.start_with_continuation ~name:__FUNCTION__ begin function
        | Merlin.Ok (ol : Merlin_j.outline list) ->
            let comments =
              let open Location in
              Lex.comments source_code
              |> List.map begin fun (c, loc) ->
                let _, start_ln, start_cn = Location.get_pos_info loc.loc_start in
                let _, stop_ln, stop_cn = Location.get_pos_info loc.loc_end in
                {
                  ol_kind = "Comment";
                  ol_name = "";
                  ol_start = { line = start_ln; col = start_cn };
                  ol_stop = { line = stop_ln; col = stop_cn };
                  ol_parent = None;
                  ol_children = [];
                  ol_level = 0;
                }
              end
            in
            let ol = List.rev_append comments ol (*|> List.sort (fun a b -> compare b.ol_start a.ol_start)*) in
            let hash = Hashtbl.hash ol in
            if outline_hash <> hash || force then
              if self#is_valid then begin
                outline_hash <- hash;
                outline <- ol;
                changed#call ();
              end else
                Log.println `WARN
                  "*** not up-to-date (%s) %.2f %.2f ***"
                  (Filename.basename buffer#filename)
                  buffer#last_edit_time  last_refresh_time;
        | Merlin.Failure _ | Merlin.Error _ -> ()
        end
      end

    method private start_timer () =
      match timer_id with
      | None ->
          self#update() |> ignore;
          timer_id <- Some (GMain.Timeout.add ~ms:300 ~callback:(fun () -> self#update(); true));
      | _ -> ()

    method private stop_timer () =
      begin
        match timer_id with
        | None -> ()
        | Some id ->
            timer_id <- None;
            last_refresh_time <- 0.0;
            GMain.Timeout.remove id
      end;

    method connect = new outline_signals ~changed

  end

and changed () = object inherit [unit] signal () end
and outline_signals ~changed =
  object
    inherit ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end

let pixbuf_of_kind = function
  | "Module" -> Some (??? Icons.module_impl)
  | "Type" -> Some (??? Icons.typ)
  | "Value" -> Some (??? Icons.simple)
  | "Exn" -> Some (??? Icons.exc)
  | "Class" -> Some (??? Icons.classe)
  | "ClassType" -> Some (??? Icons.class_type)
  | "Method" -> Some (??? Icons.met)
  | "Label"-> Some (??? Icons.type_record) (* TODO *)
  | "Constructor"-> Some (??? Icons.type_variant) (* TODO *)
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
(*let col_lazy           : (unit -> unit) list GTree.column = cols#add Gobject.Data.caml*)
let col_default_sort   = cols#add Gobject.Data.int


class view ~(outline : Oe.outline) ~(source_view : Ocaml_text.view) ?packing () =
  let pref                   = Preferences.preferences#get in
  let show_types             = pref.outline_show_types in
  let vbox                   = GPack.vbox ?packing () in
  let model                  = GTree.tree_store cols in
  let toolbar                = GButton.toolbar ~orientation:`HORIZONTAL ~style:`TEXT ~packing:(vbox#pack ~expand:false ~fill:false) () in
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
  let buffer = source_view#obuffer in
  let tool_refresh = GButton.tool_button ~packing:toolbar#insert () in
  let tool_goto_cursor_position = GButton.tool_button ~packing:toolbar#insert () in
  let tool_follow_cursor = GButton.toggle_tool_button ~active:true ~packing:toolbar#insert () in
  let tool_collapse_all = GButton.tool_button ~packing:toolbar#insert () in
  object (self)
    inherit GObj.widget vbox#as_widget

    val mutable code_font_family = ""
    val mutable sig_selection_changed = None
    val mutable timer_follow_cursor = None
    val mutable names_expaneded = []

    initializer
      tool_refresh#set_label_widget (Gtk_util.label_icon "\u{f46a}")#coerce;
      tool_collapse_all#set_label_widget (Gtk_util.label_icon "\u{f102}")#coerce;
      tool_goto_cursor_position#set_label_widget (Gtk_util.label_icon "\u{f177}")#coerce;
      tool_follow_cursor#set_label_widget (Gtk_util.label_icon "\u{21c6}")#coerce;

      self#update_preferences();
      Preferences.preferences#connect#changed ~callback:(fun _ -> self#update_preferences ()) |> ignore;
      self#set_follow_cursor true; (* TODO Preferences *)

      let sig_focus_in =
        source_view#event#connect#focus_in ~callback:(fun _ ->
            self#set_follow_cursor tool_follow_cursor#get_active;
            outline#attach(); false)
      in
      let sig_focus_out =
        source_view#event#connect#focus_out ~callback:(fun _ ->
            self#set_follow_cursor false;
            outline#detach(); false)
      in
      outline#connect#changed ~callback:self#build |> ignore;
      sig_selection_changed <- Some (view#selection#connect#changed ~callback:self#jump_to_definition);
      view#connect#row_expanded ~callback:begin fun row path ->
        try
          let ol = model#get ~row ~column:col_data in
          names_expaneded <- ol.ol_name :: names_expaneded;
          self#build_childs row path
        with Gpointer.Null as ex ->
          Printf.eprintf "File \"outline.ml\": **** %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      end |> ignore;
      view#connect#row_collapsed ~callback:begin fun row _ ->
        let ol = model#get ~row ~column:col_data in
        names_expaneded <- names_expaneded |> List.filter (fun n -> ol.ol_name = n);
      end |> ignore;
      tool_follow_cursor#connect#clicked ~callback:(fun () ->
          self#set_follow_cursor tool_follow_cursor#get_active) |> ignore;
      tool_goto_cursor_position#connect#clicked ~callback:begin fun () ->
        self#goto_cursor_position (buffer#get_mark `INSERT)
      end |> ignore;
      tool_refresh#misc#set_tooltip_text "Refresh";
      tool_collapse_all#misc#set_tooltip_text "Collapse All";
      tool_goto_cursor_position#misc#set_tooltip_text "Go to Cursor Position";
      tool_follow_cursor#misc#set_tooltip_text "Follow Cursor";
      tool_collapse_all#connect#clicked ~callback:begin fun () ->
        if tool_follow_cursor#get_active then begin
          Option.iter GMain.Timeout.remove timer_follow_cursor;
          timer_follow_cursor <- None;
          let sig_mark_set = ref None in
          sig_mark_set := Some (buffer#connect#mark_set ~callback:begin fun _ mark ->
              match GtkText.Mark.get_name mark with
              | Some "insert" ->
                  self#set_follow_cursor tool_follow_cursor#get_active;
                  Option.iter (GtkSignal.disconnect buffer#as_buffer) !sig_mark_set;
              | _ -> ()
            end)
        end;
        view#collapse_all()
      end |> ignore;
      tool_refresh#connect#clicked ~callback:(fun () -> outline#update ~force:true ()) |> ignore;
      view#misc#connect#destroy ~callback:begin fun _ ->
        GtkSignal.disconnect source_view#as_view sig_focus_in;
        GtkSignal.disconnect source_view#as_view sig_focus_out;
        self#set_follow_cursor false
      end |> ignore

    method outline = outline

    method refresh () = outline#update ~force:true ()

    method private get_iter_at_line pos =
      let ln = pos.line - 1 in
      if pos.line < 0 || ln > buffer#end_iter#line then raise (Invalid_linechar pos);
      buffer#get_iter (`LINE ln)

    method private get_iter_at_linechar pos =
      let it = self#get_iter_at_line pos in
      if pos.col >= it#chars_in_line then raise (Invalid_linechar pos);
      it#set_line_offset pos.col (*buffer#get_iter (`LINECHAR (pos.line - 1, pos.col))*)

    method jump_to_definition () =
      match view#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
          begin
            try
              let row = model#get_iter path in
              let ol = model#get ~row ~column:col_data in
              let start = self#get_iter_at_line ol.ol_start in
              let start, stop =
                match start#forward_search ol.ol_name with
                | Some bounds -> bounds
                | _ ->
                    Log.println `WARN "name %S not found on line %d" ol.ol_name (start#line + 1);
                    let start = start#forward_word_end#backward_word_start in
                    start, start
              in
              buffer#select_range start stop;
              source_view#scroll_lazy start;
            with Invalid_linechar pos ->
              Log.println `ERROR "Invalid line/char (%d, %d)" pos.line pos.col
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

    method goto_cursor_position (mark : Gtk.text_mark) =
      if self#misc#get_flag `VISIBLE then begin
        let iter = buffer#get_iter_at_mark (`MARK mark) in
        let ln = iter#line + 1 in
        let cn = iter#line_offset + 1 in
        let found_paths = ref [] in
        model#foreach begin fun path row ->
          try
            let ol = model#get ~row ~column:col_data in
            if ol.ol_kind <> "Dummy" then begin
              let start, stop =
                if ol.ol_kind = "Method" then
                  (self#get_iter_at_linechar ol.ol_start)#set_line_offset 0,
                  (self#get_iter_at_linechar ol.ol_stop)#set_line_offset 0
                else
                  self#get_iter_at_linechar ol.ol_start,
                  let it = self#get_iter_at_linechar ol.ol_stop in
                  if it#ends_line then it#forward_char else it#forward_to_line_end
              in
              if iter#in_range ~start ~stop then
                found_paths := (path, ol.ol_stop.line - ol.ol_start.line) :: !found_paths;
            end;
            false
          with Invalid_linechar pos ->
            Log.println `ERROR "Invalid line/char (%d, %d)" pos.line pos.col;
            false
        end;
        match !found_paths with
        | [] ->
            (*Log.println `WARN "%s, %s no paths found at (%d, %d)"
              __FUNCTION__ buffer#filename ln cn;*)
            view#selection#unselect_all()
        | paths -> begin
            paths
            |> List.fold_left begin fun smallest ((_, d) as x) ->
              match smallest with
              | Some ((_, d') as x') when d' < d -> Some x'
              | _ -> Some x
            end None
            |> Option.iter begin fun (path, _) ->
              match view#selection#get_selected_rows with
              | selected_path :: _ when selected_path = path -> ()
              | _ ->
                  view#expand_to_path path;
                  view#selection#select_path path;
                  if not (Gmisclib.Util.treeview_is_path_onscreen view path) then
                    Gmisclib.Idle.add ~prio:300 (fun () ->
                        view#scroll_to_cell ~align:(0.38, 0.) path vc);
            end
          end
      end

    method private build_childs row path =
      let parent = GTree.Path.copy path in
      GTree.Path.down path;
      let first_child = model#get_iter path in
      let first_child_data = model#get ~row:first_child ~column:col_data in
      if first_child_data.ol_kind = "Dummy" then begin
        model#remove first_child |> ignore;
        let row_data = model#get ~row ~column:col_data in
        let steps =
          row_data.ol_children
          (* with idleize_cascade you need to reverse the order. TODO fix idleize_cascade *)
          |> List.sort (fun a b -> compare b.ol_start a.ol_start)
          |> List.map (fun child () -> self#append ~parent:(model#get_iter parent) child)
        in
        let steps = (fun () -> view#expand_row parent) :: steps in
        Gmisclib.Idle.idleize_cascade ~prio:300 steps ();
      end

    method private build () =
      let steps = outline#get |> List.map (fun ol () -> self#append ol) in
      let update_expaneded_rows () =
        model#foreach begin fun path row ->
          let ol = model#get ~row ~column:col_data in
          if names_expaneded |> List.exists (fun ne -> ne = ol.ol_name) then begin
            Log.println `DEBUG "%s %s" __FUNCTION__ ol.ol_name;
            view#expand_row path;
          end;
          false
        end;
      in
      let steps = update_expaneded_rows :: steps in
      model#clear();
      Gmisclib.Idle.idleize_cascade ~prio:200 steps ()

    method private set_follow_cursor active =
      tool_goto_cursor_position#misc#set_sensitive (not active);
      if active then
        timer_follow_cursor <- Some begin
            GMain.Timeout.add ~ms:1000 ~callback:begin fun () ->
              let mark = buffer#get_mark `INSERT in
              Gmisclib.Idle.add ~prio:300 begin fun () ->
                if timer_follow_cursor <> None then begin
                  Option.iter view#selection#misc#handler_block sig_selection_changed;
                  self#goto_cursor_position mark;
                  Option.iter view#selection#misc#handler_unblock sig_selection_changed;
                end
              end;
              true
            end
          end
      else begin
        Option.iter begin fun id ->
          timer_follow_cursor <- None;
          GMain.Timeout.remove id
        end timer_follow_cursor
      end

    method private append ?parent ol =
      if ol.ol_kind <> "Comment" then
        let row = model#append ?parent () in
        model#set ~row ~column:col_data ol;
        let icon = pixbuf_of_kind ol.ol_kind in
        icon |> Option.iter (model#set ~row ~column:col_icon);
        let markup =
          sprintf "%s%s %s"
            (Glib.Markup.escape_text ol.ol_name)
            (if icon = None then sprintf "<span size='x-small' color='#ffc0c0'> (%s)</span>" ol.ol_kind else "")
            (if !Log.verbosity = `DEBUG then
               sprintf "<span size='x-small' color='#c0c0c0'>[ <i>%d, %d - %d, %d</i> ]</span>"
                 ol.ol_start.line (ol.ol_start.col + 1) ol.ol_stop.line (ol.ol_stop.col + 1) else "")
        in
        model#set ~row ~column:col_markup markup;
        if ol.ol_children <> [] then self#append_dummy row

    method private append_dummy row =
      let dummy = model#append ~parent:row () in
      model#set ~row:dummy ~column:col_markup "";
      model#set ~row:dummy ~column:col_data {
        ol_kind = "Dummy";
        ol_name = "";
        ol_start = { line = 0; col = 0 };
        ol_stop = { line = 0; col = 0 };
        ol_level = 0;
        ol_parent = None;
        ol_children = []
      };

    method private update_preferences () =
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
