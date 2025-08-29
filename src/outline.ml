open Printf
open Settings_j
open Merlin_j
open Preferences

module Log = Common.Log.Make(struct let prefix = "OUTLINE" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

exception Invalid_linechar of Merlin_j.pos
exception Break of Gtk.tree_iter

open GUtil

class model ~(page : Editor_page.page) () =
  let merlin text func = func ~filename:page#get_filename ~buffer:text in
  let buffer = page#buffer in
  object (self)
    val mutable outline = []
    val mutable outline_hash = 0
    val mutable timer_id = None
    val mutable last_refresh_time = 0.0
    val changed = new changed()

    method page = page

    method get = outline

    method attach = self#start_timer

    method detach = self#stop_timer

    method is_valid = buffer#last_edit_time < last_refresh_time

    method private invoke_merlin () =
      (*Log.println `DEBUG "invoke_merlin %b %s" self#is_valid (Filename.basename buffer#filename);*)
      if not self#is_valid then begin
        let source_code = buffer#get_text () in
        last_refresh_time <- Unix.gettimeofday();
        (merlin source_code)@@Merlin.outline
        |> Async.start_with_continuation begin function
        | Merlin.Ok (ol : Merlin_j.outline list) ->
            let hash = Hashtbl.hash ol in
            if outline_hash <> hash then
              if self#is_valid then begin
                outline_hash <- hash;
                outline <- ol;
                changed#call ();
              end else
                Log.println `WARN
                  "*** not up-to-date (%s) %f.2 %f.2 ***"
                  (Filename.basename page#get_filename)
                  buffer#last_edit_time  last_refresh_time;
        | Merlin.Failure _ | Merlin.Error _ -> ()
        end
      end;
      true

    method private start_timer () =
      match timer_id with
      | None ->
          self#invoke_merlin() |> ignore;
          timer_id <- Some (GMain.Timeout.add ~ms:300 ~callback:self#invoke_merlin);
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


class view ~(outline : model) ?packing () =
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
  let page = outline#page in
  let buffer = page#ocaml_view#obuffer in
  object (self)
    inherit GObj.widget vbox#as_widget

    val mutable code_font_family = ""
    val mutable sig_selection_changed = None
    val mutable timer_follow_cursor = None

    initializer
      (* Toolbar *)
      let tool_collapse_all = GButton.tool_button ~packing:toolbar#insert () in
      let _ = tool_collapse_all#set_label_widget (Gtk_util.label_icon "\u{f102}")#coerce in
      let tool_follow_cursor = GButton.toggle_tool_button ~active:true ~packing:toolbar#insert () in
      let _ = tool_follow_cursor#set_label_widget (Gtk_util.label_icon "\u{21c6}")#coerce in

      self#update_preferences();
      Preferences.preferences#connect#changed ~callback:(fun _ -> self#update_preferences ()) |> ignore;

      page#view#event#connect#focus_in ~callback:(fun _ ->
          self#set_follow_cursor tool_follow_cursor#get_active;
          outline#attach(); false) |> ignore;
      page#view#event#connect#focus_out ~callback:(fun _ ->
          self#set_follow_cursor false;
          outline#detach(); false) |> ignore;

      outline#connect#changed ~callback:self#build |> ignore;

      sig_selection_changed <- Some (view#selection#connect#changed ~callback:self#jump_to);
      view#connect#row_expanded ~callback:self#build_childs |> ignore;
      tool_follow_cursor#connect#clicked ~callback:(fun () ->
          self#set_follow_cursor tool_follow_cursor#get_active) |> ignore;
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
            end);
          ()
        end;
        view#collapse_all()
      end |> ignore;

    method outline = outline

    method private get_iter_at_line pos =
      let ln = pos.line - 1 in
      if pos.line < 0 || ln > buffer#end_iter#line then raise (Invalid_linechar pos);
      buffer#get_iter (`LINE ln)

    method private get_iter_at_linechar pos =
      let it = self#get_iter_at_line pos in
      if pos.col >= it#chars_in_line then raise (Invalid_linechar pos);
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
              let start, stop =
                match start#forward_search ol.ol_name with
                | Some bounds -> bounds
                | _ ->
                    Log.println `WARN "name %S not found on line %d" ol.ol_name (start#line + 1);
                    let start = start#forward_word_end#backward_word_start in
                    start, start
              in
              buffer#select_range start stop;
              page#view#scroll_lazy start;
              Gmisclib.Idle.add page#view#misc#grab_focus
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

    method select_from_buffer (mark : Gtk.text_mark) =
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
            Log.println `WARN "%s, %s no paths found at (%d, %d)"
              __FUNCTION__ page#get_filename ln cn;
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
        view#expand_row parent;
        let steps =
          row_data.ol_children
          (* with idleize_cascade you need to reverse the order. TODO fix idleize_cascade *)
          |> List.sort (fun a b -> -(compare a.ol_start b.ol_start))
          |> List.map (fun child () -> self#append ~parent:(model#get_iter parent) child)
        in
        Gmisclib.Idle.idleize_cascade ~prio:200 steps ();
      end

    method build () =
      let steps = outline#get |> List.map (fun ol () -> self#append ol)  in
      model#clear();
      Gmisclib.Idle.idleize_cascade ~prio:200 steps ()

    method private set_follow_cursor active =
      if active then
        timer_follow_cursor <- Some begin
            GMain.Timeout.add ~ms:1000 ~callback:begin fun () ->
              let mark = buffer#get_mark `INSERT in
              Gmisclib.Idle.add ~prio:300 begin fun () ->
                Option.iter view#selection#misc#handler_block sig_selection_changed;
                self#select_from_buffer mark;
                Option.iter view#selection#misc#handler_unblock sig_selection_changed
              end;
              true
            end
          end
      else begin
        Option.iter GMain.Timeout.remove timer_follow_cursor;
        timer_follow_cursor <- None
      end

    method private append ?parent ol =
      let row = model#append ?parent () in
      model#set ~row ~column:col_data ol;
      let icon = pixbuf_of_kind ol.ol_kind in
      icon |> Option.iter (model#set ~row ~column:col_icon);
      let markup =
        sprintf "%s%s %s"
          ol.ol_name
          (if icon = None then sprintf "<span size='x-small' color='#ffc0c0'> (%s)</span>" ol.ol_kind else "")
          (sprintf "<span size='x-small' color='#c0c0c0'>[ <i>%d, %d - %d, %d</i> ]</span>"
             ol.ol_start.line (ol.ol_start.col + 1) ol.ol_stop.line (ol.ol_stop.col + 1))
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
