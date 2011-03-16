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

let create_view ~project ~buffer ?file ?packing () =
  let sw = GBin.scrolled_window ~width:100 ~height:100 ~shadow_type:`NONE
    ~hpolicy:`NEVER ~vpolicy:`NEVER ?packing () in
  let view = new Ocaml_text.view ~project ~buffer () in
  Preferences_apply.apply (view :> Text.view) !Preferences.preferences;
  let _  = sw#add view#coerce in
  sw, (view :> Text.view), view

let shortname filename =
  let basename = Filename.basename filename in
  if !Preferences.preferences.Preferences.pref_tab_label_type = 1 then begin
    try Filename.chop_extension basename
    with Invalid_argument("Filename.chop_extension") -> basename
  end else basename

let markup_label filename =
  let shortname = shortname filename in
  if Filename.check_suffix filename !Config.interface_suffix
  then "<i>"^shortname^"</i>" else shortname


(** Editor page *)
class page ?file ~project ~offset ~editor () =
  let buffer = new Ocaml_text.buffer ~project ?file () in
  let sw, text_view, ocaml_view = create_view ~project ~buffer ?file () in
  let vbox = GPack.vbox ~spacing:0 () in
  let phbox = GPack.hbox ~spacing:1 ~packing:vbox#add () in
  let _ = phbox#add sw#coerce in
  let svbox = GPack.vbox ~spacing:1 ~packing:phbox#pack () in
  let global_gutter_ebox = GBin.event_box ~packing:phbox#pack () in
  (** Status bar *)
  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false) () in
  let sobox = GPack.hbox ~spacing:3 ~border_width:0 ~packing:vbox#pack () in
  let spaned = GPack.paned `HORIZONTAL ~packing:sobox#add () in
  let sbox = GPack.hbox ~spacing:1 ~border_width:0 ~packing:spaned#add1 () in
  let status_modified = GMisc.image ~icon_size:`MENU ~packing:sbox#pack () in
  let _ = GMisc.separator `VERTICAL ~packing:sbox#pack () in
  let status_filename = GMisc.label ~xalign:0.0 ~xpad:5 ~width:150 (* 240 *) ~packing:sbox#add () in
  (**  *)
  let _ = GMisc.separator `VERTICAL ~packing:sbox#pack () in
  let status_cursor_line = GMisc.label ~xalign:0.5 ~packing:sbox#pack ~width:70 () in
  let _ = GMisc.separator `VERTICAL ~packing:sbox#pack () in
  let status_cursor_off = GMisc.label ~xalign:0.5 ~packing:sbox#pack ~width:45 () in
  let _ = GMisc.separator `VERTICAL ~packing:sbox#pack () in
  let status_size = GMisc.label ~xalign:0.5 ~packing:sbox#pack ~width:80 () in
  let _ = GMisc.separator `VERTICAL ~packing:sbox#pack () in
  let status_mtime = GMisc.label ~xalign:0.5 ~packing:sbox#pack ~width:120 () in
  (** Navigation buttons in the statusbar *)
  let first_sep = GMisc.separator `VERTICAL ~packing:sobox#pack () in
  let location_goto where =
    match where editor#location_history with
      | None -> true
      | Some loc -> editor#location_history_goto loc; true
  in
  let button_h_prev = GBin.event_box ~packing:sobox#pack (*~show:false*) () in
  let _ = button_h_prev#misc#set_tooltip_text "Back" in
  let _ = Gtk_util.set_ebox_invisible button_h_prev in
  let _ = GMisc.image ~pixbuf:Icons.arrow_prev_14 ~packing:button_h_prev#add () in
  let _ = button_h_prev#event#connect#button_press ~callback:begin fun _ ->
    location_goto Location_history.previous
  end in
  let button_h_next = GBin.event_box ~packing:sobox#pack (*~show:false*) () in
  let _ = button_h_next#misc#set_tooltip_text "Forward" in
  let _ = Gtk_util.set_ebox_invisible button_h_next in
  let _ = GMisc.image ~pixbuf:Icons.arrow_next_14 ~packing:button_h_next#add () in
  let _ = button_h_next#event#connect#button_press ~callback:begin fun _ ->
    location_goto Location_history.next
  end in
  let button_h_last = GBin.event_box ~packing:sobox#pack (*~show:false*) () in
  let _ = button_h_last#misc#set_tooltip_text "Last Edit Location" in
  let _ = Gtk_util.set_ebox_invisible button_h_last in
  let _ = GMisc.image ~pixbuf:Icons.arrow_last_14 ~packing:button_h_last#add () in
  let _ = button_h_last#event#connect#button_press ~callback:begin fun _ ->
    location_goto Location_history.goto_last_edit_location
  end in
  let _ = GMisc.separator `VERTICAL ~packing:sobox#pack () in
  (** Icons for font size and row spacing adjustment *)
  let button_font_incr = GBin.event_box ~packing:sobox#pack () in
  let _ = Gtk_util.set_ebox_invisible button_font_incr in
  let _ = GMisc.image ~pixbuf:Icons.zoom_in_14 ~packing:button_font_incr#add () in
  let button_font_decr = GBin.event_box ~packing:sobox#pack () in
  let _ = Gtk_util.set_ebox_invisible button_font_decr in
  let _ = GMisc.image ~pixbuf:Icons.zoom_out_14 ~packing:button_font_decr#add () in
  let button_rowspacing_incr = GBin.event_box ~packing:sobox#pack () in
  let _ = Gtk_util.set_ebox_invisible button_rowspacing_incr in
  let _ = GMisc.image ~pixbuf:Icons.lines_in_14 ~packing:button_rowspacing_incr#add () in
  let button_rowspacing_decr = GBin.event_box ~packing:sobox#pack () in
  let _ = Gtk_util.set_ebox_invisible button_rowspacing_decr in
  let _ = GMisc.image ~pixbuf:Icons.lines_out_14 ~packing:button_rowspacing_decr#add () in
  (** Font size *)
  let _ = button_font_incr#event#connect#button_press ~callback:begin fun ev ->
    let fd = text_view#misc#pango_context#font_description in
    let size = Pango.Font.get_size fd + Pango.scale in
    Pango.Font.modify fd ~size ();
    text_view#misc#modify_font fd;
    Line_num_labl.iter (fun lab -> lab#misc#modify_font fd) text_view#line_num_labl;
    Gtk_util.idle_add text_view#paint_gutter;
    true
  end in
  let _ = button_font_decr#event#connect#button_press ~callback:begin fun ev ->
    let fd = text_view#misc#pango_context#font_description in
    let size = Pango.Font.get_size fd in
    if size - Pango.scale >= (7 * Pango.scale) then begin
      let size = size - Pango.scale in
      Pango.Font.modify fd ~size ();
      text_view#misc#modify_font fd;
      Line_num_labl.iter (fun lab -> lab#misc#modify_font fd) text_view#line_num_labl;
      Gtk_util.idle_add text_view#paint_gutter
    end;
    true;
  end in
  (** Row spacing *)
  let _ = button_rowspacing_incr#event#connect#button_press ~callback:begin fun ev ->
    text_view#set_pixels_above_lines (min 2 (text_view#pixels_above_lines + 1));
    text_view#set_pixels_below_lines (min 2 (text_view#pixels_below_lines + 1));
    Gtk_util.idle_add text_view#paint_gutter;
    true
  end in
  let _ = button_rowspacing_decr#event#connect#button_press ~callback:begin fun ev ->
    text_view#set_pixels_above_lines (max 0 (text_view#pixels_above_lines - 1));
    text_view#set_pixels_below_lines (max 0 (text_view#pixels_below_lines - 1));
    Gtk_util.idle_add text_view#paint_gutter;
    true;
  end in
  (** Scrollbars *)
  let hscrollbar = GRange.scrollbar `HORIZONTAL ~adjustment:sw#hadjustment ~packing:spaned#add2 () in
  let vscrollbar = GRange.scrollbar `VERTICAL ~adjustment:sw#vadjustment ~packing:svbox#add () in
  let _ =
    text_view#event#connect#scroll ~callback:begin fun ev ->
      let sign = match GdkEvent.Scroll.direction ev with
        | `UP when sw#vadjustment#value > sw#vadjustment#lower -> (-.1.)
        | `DOWN when sw#vadjustment#value < sw#vadjustment#upper -. sw#vadjustment#page_size -> 1.
        | _ -> 0.
      in
      if sign <> 0. then begin
        let value = sw#vadjustment#value +. (sw#vadjustment#step_increment *. sign) in
        (sw#vadjustment#set_value value);
      end;
      false
    end
  in
  (** Global gutter *)
  let global_gutter = GMisc.drawing_area ~width:9 ~packing:global_gutter_ebox#add
    ~show:!Preferences.preferences.Preferences.pref_show_global_gutter () in
  (*  *)
  let _ = List.iter begin fun lab ->
    lab#set_use_markup true
  end [status_filename; status_cursor_line; status_cursor_off; status_size; status_mtime] in
  let _ =
    buffer#create_tag ~name:"tag_matching_delim" [
      `BACKGROUND_GDK (Preferences.tag_color "highlight");
      `BACKGROUND_FULL_HEIGHT_SET true;
      (*`UNDERLINE (Preferences.tag_underline "highlight");*)
    ]
  in
object (self)
  inherit GObj.widget vbox#as_widget
  val mutable view = text_view
  val mutable file = file
  val mutable read_only = false;
  val mutable tab_widget : (GBin.alignment * GButton.button * GMisc.label) option = None
  val mutable resized = false
  val mutable changed_after_last_autosave = false
  val mutable load_complete = false
  val annot_type = new Annot_type.annot_type ocaml_view
  val error_indication = new Error_indication.error_indication ocaml_view vscrollbar (global_gutter, global_gutter_ebox)
  val mutable outline = None

  method annot_type = annot_type
  method error_indication = error_indication

  method global_gutter = global_gutter#coerce

  method outline = outline
  method set_outline x = outline <- x

  method load_complete = load_complete

  method changed_after_last_autosave = changed_after_last_autosave
  method set_changed_after_last_autosave x = changed_after_last_autosave <- x

  method private set_tag_annot_background () =
    annot_type#tag#set_property (`BACKGROUND
      (Color.name (Color.set_value 1.0 (`NAME !Preferences.preferences.Preferences.pref_bg_color_popup))));

  method read_only = read_only
  method set_read_only ro =
    read_only <- ro;
    text_view#set_editable (not ro)
  method set_tab_widget x = tab_widget <- Some x
  method tab_widget = match tab_widget with None -> assert false | Some x -> x
  method file = file
  method set_file f = file <- f
  method get_filename = match file with None -> "untitled.ml" | Some f -> f#path
  method view = view
  method ocaml_view = ocaml_view
  method buffer = buffer
  method project = project
  method vadjustment = sw#vadjustment
  method undo () = if not (buffer#undo#undo()) then (text_view#scroll_lazy (buffer#get_iter `INSERT))
  method redo () = if not (buffer#undo#redo()) then (text_view#scroll_lazy (buffer#get_iter `INSERT))

  method initial_offset : int = offset

  method set_code_folding_enabled x =
    let is_ml = match file with None -> false | Some file -> file#name ^^ ".ml" || file#name ^^ ".mli" in
    ocaml_view#code_folding#set_enabled (x && is_ml);

  method redisplay () =
    if buffer#lexical_enabled then begin
      let buffer = (buffer :> GText.buffer) in
      Lexical.init_tags buffer;
      Lexical.tag buffer;
    end;
    Preferences_apply.apply view !Preferences.preferences;
    self#set_code_folding_enabled !Preferences.preferences.Preferences.pref_code_folding_enabled;
    Gaux.may (GtkText.TagTable.lookup buffer#tag_table "tag_matching_delim")
      ~f:(fun x -> GtkText.TagTable.remove buffer#tag_table x);
    ignore (buffer#create_tag ~name:"tag_matching_delim" [
      `BACKGROUND_GDK (Preferences.tag_color "highlight");
      `BACKGROUND_FULL_HEIGHT_SET true;
    ]);
    self#set_tag_annot_background();
    self#error_indication#create_tags();
    self#error_indication#set_flag_underline !Preferences.preferences.Preferences.pref_err_underline;
    self#error_indication#set_flag_tooltip !Preferences.preferences.Preferences.pref_err_tooltip;
    self#error_indication#set_flag_gutter !Preferences.preferences.Preferences.pref_err_gutter;
    self#view#create_highlight_current_line_tag()

  method update_statusbar () =
    match self#file with
      | None -> ()
      | Some file ->
        try
          kprintf status_filename#set_label "%s" file#name;
          let stat = Unix.stat file#path in
          let tm = Unix.localtime stat.Unix.st_mtime in
          kprintf status_size#set_label "%d bytes" stat.Unix.st_size;
          let last_modified = sprintf "%4d-%d-%d %02d:%02d:%02d" (tm.Unix.tm_year + 1900)
            (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
          in
          kprintf status_mtime#set_label "%s" last_modified;
        with _ -> ()

  method backup () = Gaux.may file ~f:begin fun (file : File.file) ->
    Project.backup_file project file
  end

  method save () = Gaux.may file ~f:begin fun file ->
    if not file#is_readonly then begin
      if !Preferences.preferences.Preferences.pref_editor_bak then (self#backup());
      let text = Project.convert_from_utf8 project (buffer#get_text ()) in
      self#set_file None;
      file#write text;
      self#set_file (Some (File.create file#path ()));
      self#update_statusbar();
      (*  *)
      Gtk_util.idle_add (fun () -> self#compile_buffer ~commit:true ());
      (* Delete existing recovery copy *)
      self#set_changed_after_last_autosave false;
      Autosave.delete ~filename:file#path ();
      (*  *)
      buffer#set_modified false;
    end
  end

  method revert () = Gaux.may file ~f:begin fun file ->
    Bookmark.write();
    let old_markers =
      try
        let bms = List.filter (fun bm -> bm.Bookmark.filename = file#path) !Bookmark.bookmarks in
        List.iter Bookmark.mark_to_offset bms;
        Xlist.filter_map (fun bm -> bm.Bookmark.marker) bms
      with Not_found -> []
    in
    Gutter.destroy_markers view#gutter old_markers;
    (*  *)
    let vv = vscrollbar#adjustment#value in
    buffer#delete ~start:buffer#start_iter ~stop:buffer#end_iter;
    ignore (self#load());
    self#set_file (Some (File.create file#path ()));
    Gtk_util.idle_add ~prio:300 (fun () -> vscrollbar#adjustment#set_value vv);
    Gtk_util.idle_add ~prio:400 begin fun () ->
      let rect = view#visible_rect in
      let where, _ = view#get_line_at_y (Gdk.Rectangle.y rect + Gdk.Rectangle.height rect / 2) in
      buffer#place_cursor ~where
    end
  end

  method load ?(scroll=true) () =
    match file with
      | None -> false
      | Some file ->
        begin
          try
            self#set_code_folding_enabled false;
            buffer#insert (Project.convert_to_utf8 project file#read);
            (* Bookmarks: offsets to marks *)
            List.iter begin fun bm ->
              if bm.Bookmark.filename = file#path then
                let mark = (Bookmark.offset_to_mark (self#buffer :> GText.buffer) bm) in
                let marker = Gutter.create_marker ~mark ~pixbuf:(List.assoc bm.Bookmark.num Bookmark.icons) () in
                bm.Bookmark.marker <- Some marker;
                view#gutter.Gutter.markers <- marker :: view#gutter.Gutter.markers
            end !Bookmark.bookmarks;
            Bookmark.write();
            (*  *)
            if buffer#lexical_enabled then begin
              Lexical.tag self#view#buffer ~start:buffer#start_iter ~stop:buffer#end_iter;
            end;
            (* Initial cursor position *)
            if scroll then begin
              let where = buffer#get_iter (`OFFSET offset) in
              buffer#place_cursor ~where;
              self#view#scroll_lazy where;
            end;
            (*  *)
            buffer#set_modified false;
            buffer#connect#changed ~callback:begin fun () ->
              changed_after_last_autosave <- true;
              buffer#set_changed_after_last_autocomp (Unix.gettimeofday())
            end;
            buffer#undo#enable();
            load_complete <- true;
            self#view#paint_current_line_background (self#buffer#get_iter `INSERT);
            (*  *)
            self#set_code_folding_enabled editor#code_folding_enabled#get; (* calls scan_folding_points, if enabled *)
            ignore (self#view#matching_delim ());
            Gtk_util.idle_add ~prio:300 (fun () -> self#compile_buffer ~commit:false ());
            true
          with Glib.Convert.Error (_, message) -> begin
            let message = if project.Project.encoding <> Some "UTF-8" then (kprintf Convert.to_utf8
              "Cannot convert file\n\n%s\n\nfrom %s codeset to UTF-8.\n\n%s"
                file#path
                (match project.Project.encoding with None -> "Default" | Some x -> x)
                message) else message
            in
            let dialog = GWindow.message_dialog ~title:"Text file contains invalid characters."
              ~message_type:`ERROR ~message ~buttons:GWindow.Buttons.ok () in
            (match dialog#run () with _ -> dialog#destroy())
          end;
          false;
        end

  method compile_buffer ~commit () =
    let filename = self#get_filename in
    if project.Project.autocomp_enabled
    && ((project.Project.in_source_path filename) <> None)
    && (filename ^^ ".ml" || filename ^^ ".mli") then begin
      buffer#set_changed_after_last_autocomp 0.0;
      Autocomp.compile_buffer ~project ~editor ~page:self ~commit ();
    end else begin
      let empty = new Outline.widget ~project ~page:self ~tmp:"" in
      editor#pack_outline empty#coerce;
      self#set_outline (Some empty)
    end

  method tooltip ?(typ=false) ((x, y) as location) =
    let location = `XY location in
    if typ then (self#annot_type#tooltip location);
    if !Preferences.preferences.Preferences.pref_err_tooltip
    then (error_indication#tooltip location)

  initializer
    view#hyperlink#enable();
    buffer#set_modified false;
    self#set_tag_annot_background();
    (** Expose: Statusbar *)
    self#view#event#connect#after#expose ~callback:begin fun _ ->
      let line, line_offset, _, iter_offset = view#line_char in
      kprintf status_cursor_line#set_label "<b>%4d : %-3d</b>" line line_offset;
      kprintf status_cursor_off#set_label "%d" iter_offset;
      if read_only then (status_modified#set_pixbuf Icons.lock_14; status_modified#misc#set_tooltip_text "Read-only")
      else if buffer#modified then (status_modified#set_pixbuf Icons.save_14; status_modified#misc#set_tooltip_text "Modified")
      else (status_modified#set_pixbuf Icons.none_14);
      false
    end;
    (** After focus_in, check whether the file is changed on disk *)
    ignore (text_view#event#connect#after#focus_in ~callback:begin fun _ ->
      Gaux.may self#file ~f:begin fun f ->
        if Sys.file_exists f#path && f#changed then
          if buffer#modified then begin
            let message = "File\n" ^ self#get_filename^"\nchanged on disk, reload?" in
            let yes = (fun () -> self#revert()) in
            let no () = self#set_file (Some (File.create f#path ())) in
            Dialog.confirm ~message ~f:(yes, no) self;
          end else self#revert();
        self#set_read_only f#is_readonly;
      end;
      false
    end);
    (** Clean up type annotation tag *)
    text_view#event#connect#scroll ~callback:(fun _ -> annot_type#remove_tag(); error_indication#hide_tooltip(); false);
    text_view#event#connect#leave_notify ~callback:(fun _ -> annot_type#remove_tag(); error_indication#hide_tooltip(); false);
    text_view#event#connect#focus_out ~callback:(fun _ -> annot_type#remove_tag(); error_indication#hide_tooltip(); false);
    ignore (text_view#buffer#connect#mark_set ~callback:begin fun iter mark ->
      match GtkText.Mark.get_name mark with
        | Some name when name = "insert" ->
           Gaux.may outline ~f:(fun ol ->
             Gtk_util.idle_add ~prio:500 (fun () -> ol#select mark))
        | Some name when Str.string_before name 5 = "delim" -> ()
        | _ -> annot_type#remove_tag(); error_indication#hide_tooltip()
    end);
    (** Horizontal scrollbar appears/disappears according to the window size *)
    ignore (sw#misc#connect#size_allocate ~callback:begin fun x ->
      let alloc = sw#misc#allocation in
      if not resized then (spaned#set_position ((alloc.Gtk.width) * 6 / 10));
      if hscrollbar#adjustment#page_size = hscrollbar#adjustment#upper
      then (hscrollbar#misc#hide(); first_sep#misc#show()) else (hscrollbar#misc#show(); first_sep#misc#hide());
      resized <- true;
    end);
    (** Hyperlinks *)
    ignore (self#view#hyperlink#connect#hover ~callback:begin fun (bounds, iter) ->
      editor#with_current_page begin fun page ->
        match Definition.find_definition ~project:editor#project ~page ~iter with
          | None -> ()
          | Some (start, stop, _, _, _) ->
            let start = buffer#get_iter (`OFFSET start) in
            let stop = buffer#get_iter (`OFFSET stop) in
            bounds := Some (start, stop)
      end
    end);
    ignore (self#view#hyperlink#connect#activate ~callback:begin fun iter ->
      editor#scroll_to_definition iter;
    end);
end


