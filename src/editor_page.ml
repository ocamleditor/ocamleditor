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
open Miscellanea
open GUtil

let create_view ~project ~buffer ?file ?packing () =
  let sw = GBin.scrolled_window ~width:100 ~height:100 ~shadow_type:`NONE
    ~hpolicy:`NEVER ~vpolicy:`NEVER ?packing () in
  let view = new Ocaml_text.view ~project ~buffer () in
  Preferences_apply.apply (view :> Text.view) Preferences.preferences#get;
  let _  = sw#add view#coerce in
  sw, (view :> Text.view), view

let shortname filename =
  let basename = Filename.basename filename in
  if Preferences.preferences#get.Preferences.pref_tab_label_type = 1 then begin
    try Filename.chop_extension basename
    with Invalid_argument("Filename.chop_extension") -> basename
  end else basename

let markup_label filename =
  let shortname = shortname filename in
  if filename ^^^ ".mli" then "<i>"^shortname^"</i>" else shortname

let create_small_button ?button ?tooltip ~pixbuf ?callback ?packing ?show () =
  let button =
    match button with Some b -> b | _ -> GButton.button ~relief:`NONE ?packing ?show ()
  in
  button#set_image (GMisc.image ~pixbuf ())#coerce;
  button#set_focus_on_click false;
  button#misc#set_name "smallbutton";
  Gaux.may tooltip ~f:button#misc#set_tooltip_text;
  Gaux.may callback ~f:(fun callback -> ignore (button#connect#clicked ~callback));
  button;;

let create_small_toggle_button ?tooltip ~pixbuf ?callback ?packing ?show () =
  let button = GButton.toggle_button ~relief:`NONE ?packing ?show () in
  let _ = create_small_button ~button:(button :> GButton.button) ?tooltip ~pixbuf ?callback ?packing () in
  button;;

(** Editor page *)
class page ?file ~project ~scroll_offset ~offset ~editor () =
  let file_changed             = new file_changed () in
  let buffer                   = new Ocaml_text.buffer ~project ?file () in
  let sw, text_view, ocaml_view = create_view ~project ~buffer ?file () in
  let vbox                     = GPack.vbox ~spacing:0 () in
  let textbox                  = GPack.hbox ~spacing:1 ~packing:vbox#add () in
  let _                        = textbox#add sw#coerce in (* Text box *)
  let svbox                    = GPack.vbox ~spacing:1 ~packing:textbox#pack () in (* Vertical scrollbar box *)
  let global_gutter_ebox       = GBin.event_box ~packing:textbox#pack () in (* Global gutter box *)
  (** Status bar *)
  let _                        = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false) () in
  let sbbox                    = GPack.hbox ~spacing:1 ~border_width:0 ~packing:vbox#pack () in
  let spaned                   = GPack.paned `HORIZONTAL ~packing:sbbox#add () in
  let sbox                     = GPack.hbox ~spacing:1 ~border_width:0 ~packing:(spaned#pack1 ~shrink:false) () in
  let status_modified          = GMisc.image ~icon_size:`MENU ~packing:sbox#pack () in
  let _                        = GMisc.separator `VERTICAL ~packing:sbox#pack () in
  let status_filename          = GMisc.label ~selectable:true ~xalign:0.0 ~xpad:5 ~width:240 ~ellipsize:`END ~packing:sbox#add () in
  let sep_status_pos_box       = GMisc.separator `VERTICAL ~packing:sbox#pack () in
  let status_pos_box           = GPack.hbox ~spacing:3 ~packing:sbox#pack () in
  let _                        = GMisc.label ~xalign:0.0 ~yalign:0.5 ~text:"Ln:" ~packing:status_pos_box#pack () in
  let status_pos_lin           = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:33 ~packing:status_pos_box#pack () in
  (*let _                        = status_pos_lin#misc#modify_font_by_name "bold" in*)
  let _                        = GMisc.label ~xalign:0.0 ~yalign:0.5 ~text:"Col:" ~packing:status_pos_box#pack () in
  let status_pos_col           = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:33 ~packing:status_pos_box#pack () in
  (*let _                        = status_pos_col#misc#modify_font_by_name "bold" in*)
  let _                        = GMisc.label ~xalign:0.0 ~yalign:0.5 ~text:"Off:" ~packing:status_pos_box#pack () in
  let status_pos_off           = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:40 ~packing:status_pos_box#pack () in
  let _                        = GMisc.label ~xalign:0.0 ~yalign:0.5 ~text:"Sel:" ~packing:status_pos_box#pack () in
  let status_pos_sel           = GMisc.label ~xalign:0.0 ~yalign:0.5 ~width:70 ~text:"0" ~packing:status_pos_box#pack () in
  (**  *)
  let _                        = GMisc.separator `VERTICAL ~packing:sbox#pack () in
  let spinner                  = GMisc.image ~width:15 ~packing:sbox#pack () in
  let _                        = GMisc.separator `VERTICAL ~packing:sbox#pack () in
  let button_dotview           = create_small_toggle_button
    ~pixbuf:Icons.tree
    ~packing:sbbox#pack ()
    ~show:(false (*Oe_config.dot_version <> None*))
  in
  (** Icons for font size and row spacing adjustment *)
  let button_font_incr = create_small_button
    ~pixbuf:Icons.zoom_in_14
    ~packing:sbbox#pack
    ~callback:begin fun () ->
      let fd = text_view#misc#pango_context#font_description in
      let size = Pango.Font.get_size fd + Pango.scale in
      Pango.Font.modify fd ~size ();
      text_view#misc#modify_font fd;
      Line_num_labl.iter (fun lab -> lab#misc#modify_font fd) text_view#line_num_labl;
      Gmisclib.Idle.add text_view#draw_gutter;
    end ()
  in
  let button_font_decr = create_small_button
    ~pixbuf:Icons.zoom_out_14
    ~packing:sbbox#pack
    ~callback:begin fun () ->
      let fd = text_view#misc#pango_context#font_description in
      let size = Pango.Font.get_size fd in
      if size - Pango.scale >= (7 * Pango.scale) then begin
        let size = size - Pango.scale in
        Pango.Font.modify fd ~size ();
        text_view#misc#modify_font fd;
        Line_num_labl.iter (fun lab -> lab#misc#modify_font fd) text_view#line_num_labl;
        Gmisclib.Idle.add text_view#draw_gutter
      end
    end ()
  in
  let button_rowspacing_incr = create_small_button
    ~pixbuf:Icons.lines_in_14
    ~packing:sbbox#pack
    ~callback:begin fun () ->
      let above, below = Preferences.preferences#get.Preferences.pref_editor_pixels_lines in
      text_view#set_pixels_above_lines (min (4 + above) (text_view#pixels_above_lines + 1));
      text_view#set_pixels_below_lines (min (4 + below) (text_view#pixels_below_lines + 1));
      Gmisclib.Idle.add text_view#draw_gutter;
    end ()
  in
  let button_rowspacing_decr = create_small_button
    ~pixbuf:Icons.lines_out_14
    ~packing:sbbox#pack
    ~callback:begin fun () ->
      text_view#set_pixels_above_lines (max 0 (text_view#pixels_above_lines - 1));
      text_view#set_pixels_below_lines (max 0 (text_view#pixels_below_lines - 1));
      Gmisclib.Idle.add text_view#draw_gutter;
    end ()
  in
  (** Show whitespace and word wrap *)
  let button_toggle_wrap = create_small_toggle_button ~pixbuf:Icons.wrap_off_14 ~packing:sbbox#pack () in
  let button_toggle_whitespace = create_small_toggle_button ~pixbuf:Icons.whitespace_off_14 ~packing:sbbox#pack () in
  (** Navigation buttons in the statusbar *)
  (*let first_sep = GMisc.separator `VERTICAL ~packing:sobox#pack () in*)
  let location_goto where =
    match where editor#location_history with
      | None -> ()
      | Some loc -> editor#location_history_goto loc
  in
  let button_h_prev            = create_small_button
    ~pixbuf:Icons.arrow_prev_14
    ~tooltip:"Back"
    ~packing:sbbox#pack
    ~callback:(fun _ -> location_goto Location_history.previous) () in
  let button_h_next            = create_small_button
    ~pixbuf:Icons.arrow_next_14
    ~tooltip:"Forward"
    ~packing:sbbox#pack
    ~callback:(fun _ -> location_goto Location_history.next) () in
  let button_h_last            = create_small_button
    ~pixbuf:Icons.arrow_last_14
    ~tooltip:"Last Edit Location"
    ~packing:sbbox#pack
    ~callback:(fun _ -> location_goto Location_history.goto_last_edit_location) () in
  (** Scrollbars *)
  let hscrollbar = GRange.scrollbar `HORIZONTAL ~adjustment:sw#hadjustment ~packing:spaned#add2 () in
  let vscrollbar = GRange.scrollbar `VERTICAL ~adjustment:sw#vadjustment (*~update_policy:`DELAYED*) ~packing:svbox#add () in
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
  let global_gutter = GMisc.drawing_area ~width:Oe_config.global_gutter_size ~packing:global_gutter_ebox#add
    ~show:Preferences.preferences#get.Preferences.pref_show_global_gutter () in
  let _ = global_gutter#misc#set_has_tooltip true in
  let _ = global_gutter#event#add [`BUTTON_PRESS; `BUTTON_RELEASE] in
  (*  *)
  let _                        = List.iter begin fun lab ->
    lab#set_use_markup true
  end [status_filename] in
  let _                        =
    buffer#create_tag ~name:"tag_matching_delim" [
      `BACKGROUND_GDK (Preferences.tag_color "highlight");
      `BACKGROUND_FULL_HEIGHT_SET true;
      (*`UNDERLINE (Preferences.tag_underline "highlight");*)
    ]
  in
object (self)
  inherit GObj.widget vbox#as_widget
  val mutable view = text_view
  val mutable file : Editor_file_type.abstract_file option = file
  val mutable read_only = false;
  val mutable tab_widget : (GBin.alignment * GButton.button * GMisc.label) option = None
  val mutable resized = false
  val mutable changed_after_last_autosave = false
  val mutable changed_after_last_diff = true
  val mutable load_complete = false
  val mutable annot_type = None
  val error_indication = new Error_indication.error_indication ocaml_view vscrollbar global_gutter
  val mutable outline = None
  val mutable dotview = None
  val mutable word_wrap = editor#word_wrap
  val mutable show_whitespace = editor#show_whitespace_chars
  val mutable signal_buffer_changed = None
  val mutable signal_button_toggle_wrap = None
  val mutable signal_button_toggle_whitespace = None
  val mutable signal_button_dotview = None
  val mutable global_gutter_tooltips : ((int * int * int * int) * (unit -> GObj.widget)) list = []

  method global_gutter_tooltips = global_gutter_tooltips
  method set_global_gutter_tooltips x = global_gutter_tooltips <- x

  method annot_type = annot_type
  method error_indication = error_indication

  method global_gutter = global_gutter
  method vscrollbar = vscrollbar

  method outline = outline
  method set_outline x = outline <- x

  method changed_after_last_autosave = changed_after_last_autosave
  method set_changed_after_last_autosave x = changed_after_last_autosave <- x

  method changed_after_last_diff = changed_after_last_diff
  method set_changed_after_last_diff x = changed_after_last_diff <- x

  method private set_tag_annot_background () =
    Opt.may annot_type (fun annot_type ->
      annot_type#tag#set_property (`BACKGROUND Preferences.preferences#get.Preferences.pref_bg_color_popup));

  method read_only = read_only
  method set_read_only ro =
    read_only <- ro;
    text_view#set_editable (not ro)
  method set_tab_widget x = tab_widget <- Some x
  method tab_widget = match tab_widget with None -> assert false | Some x -> x
  method file = file
  method set_file file_obj =
    file <- file_obj;
    begin
      match file_obj with
        | Some file_obj ->
          let _, _, label = self#tab_widget in
          label#set_text (shortname file_obj#filename);
          self#update_statusbar();
        | None -> ()
    end;
    buffer#reset_tmp_filename();
    file_changed#call file_obj

  method get_filename = match file with None -> "untitled.ml" | Some f -> f#filename
  method get_title =
    match file with
      | Some file ->
        begin
          match file#remote with
            | None -> file#filename
            | Some rmt -> sprintf "%s@%s%s" rmt.Editor_file_type.user rmt.Editor_file_type.host file#filename
        end;
      | _ -> ""
  method view = view
  method ocaml_view = ocaml_view
  method buffer = buffer
  method project = project
  method vadjustment = sw#vadjustment
  method status_pos_sel = status_pos_sel
  method undo () = if not (buffer#undo#undo()) then (text_view#scroll_lazy (buffer#get_iter `INSERT))
  method redo () = if not (buffer#undo#redo()) then (text_view#scroll_lazy (buffer#get_iter `INSERT))

  method initial_offset : int = offset
  method scroll_offset = scroll_offset

  method set_code_folding_enabled x =
    let is_ml = match file with None -> false | Some file ->
      file#basename ^^^ ".ml" || file#basename ^^^ ".mli" in
    ocaml_view#code_folding#set_enabled (x && is_ml);

  method redisplay () =
    if buffer#lexical_enabled then begin
      buffer#init_tags ();
      let buffer = (buffer :> GText.buffer) in
      Lexical.tag buffer;
    end;
    Preferences_apply.apply view Preferences.preferences#get;
    self#set_code_folding_enabled Preferences.preferences#get.Preferences.pref_code_folding_enabled;
    ocaml_view#code_folding#set_fold_line_color ocaml_view#options#text_color;
    Gaux.may (GtkText.TagTable.lookup buffer#tag_table "tag_matching_delim")
      ~f:(fun x -> GtkText.TagTable.remove buffer#tag_table x);
    ignore (buffer#create_tag ~name:"tag_matching_delim" [
      `BACKGROUND_GDK (Preferences.tag_color "highlight");
      `BACKGROUND_FULL_HEIGHT_SET true;
    ]);
    self#set_tag_annot_background();
    self#error_indication#create_tags();
    self#error_indication#set_flag_underline Preferences.preferences#get.Preferences.pref_err_underline;
    self#error_indication#set_flag_tooltip Preferences.preferences#get.Preferences.pref_err_tooltip;
    self#error_indication#set_flag_gutter Preferences.preferences#get.Preferences.pref_err_gutter;
    self#view#create_highlight_current_line_tag();
    Gaux.may outline ~f:(fun outline -> outline#view#misc#modify_font_by_name Preferences.preferences#get.Preferences.pref_compl_font);
    error_indication#set_phase ();

  method update_statusbar () =
    match self#file with
      | None -> ()
      | Some file ->
        try
          status_filename#set_label self#get_title;
          begin
            match file#stat() with
              | Some stat ->
                let tm = Unix.localtime (stat.Editor_file_type.mtime) in
                let last_modified = sprintf "%4d-%d-%d %02d:%02d:%02d" (tm.Unix.tm_year + 1900)
                  (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
                in
                kprintf status_filename#misc#set_tooltip_markup "%s%s\nLast modified: %s\n%s bytes - %s lines - %s characters"
                  (if project.Prj.in_source_path file#filename <> None then "<b>" ^ project.Prj.name ^ "</b>\n" else "")
                  self#get_title
                  last_modified
                  (Text_util.format_int stat.Editor_file_type.size)
                  (Text_util.format_int buffer#line_count)
                  (Text_util.format_int buffer#char_count);
              | _ -> ()
          end;
        with _ -> ()

  method backup () = Gaux.may file ~f:begin fun (file : Editor_file.file) ->
    Project.backup_file project file
  end

  method save () = Gaux.may file ~f:begin fun file ->
    if not file#is_readonly then begin
      if Preferences.preferences#get.Preferences.pref_editor_bak then (self#backup());
      let text = Project.convert_from_utf8 project (buffer#get_text ()) in
      file#write text;
      Gmisclib.Idle.add self#update_statusbar;
      Gmisclib.Idle.add (fun () -> self#compile_buffer ?join:None ());
      (* Delete existing recovery copy *)
      self#set_changed_after_last_autosave false;
      Autosave.delete ~filename:file#filename ();
      (*  *)
      buffer#set_modified false;
    end
  end

  method revert () = Gaux.may file ~f:begin fun file ->
    Project.save_bookmarks project;
    let old_markers =
      try
        let bms = List.filter (fun bm -> bm.Oe.bm_filename = file#filename) project.Prj.bookmarks in
        List.iter Bookmark.mark_to_offset bms;
        Xlist.filter_map (fun bm -> bm.Oe.bm_marker) bms
      with Not_found -> []
    in
    Gutter.destroy_markers view#gutter old_markers;
    (*  *)
    let vv = vscrollbar#adjustment#value in
    buffer#block_signal_handlers();
    buffer#delete ~start:buffer#start_iter ~stop:buffer#end_iter;
    ignore (self#load());
    buffer#unblock_signal_handlers();
    self#set_file (Some file);
    (*  *)
    self#set_changed_after_last_autosave false;
    Autosave.delete ~filename:file#filename ();
    (*  *)
    Gmisclib.Idle.add ~prio:300 (fun () -> vscrollbar#adjustment#set_value vv);
    Gmisclib.Idle.add ~prio:400 begin fun () ->
      let rect = view#visible_rect in
      let where, _ = view#get_line_at_y (Gdk.Rectangle.y rect + Gdk.Rectangle.height rect / 2) in
      buffer#place_cursor ~where
    end
  end

  method load_complete = load_complete

  method load ?(scroll=true) () =
    match file with
      | None -> false
      | Some file ->
        begin
          try
            self#set_code_folding_enabled false;
            buffer#insert (Project.convert_to_utf8 project file#read);
            (* Initial cursor position *)
            if scroll then begin
              Gmisclib.Idle.add begin fun () ->
                let where = buffer#get_iter (`OFFSET offset) in
                buffer#place_cursor ~where;
                let where = buffer#get_iter (`OFFSET scroll_offset) in
                Gmisclib.Idle.add ~prio:300 begin fun () ->
                  ignore (self#view#scroll_to_iter ~use_align:(self#view#scroll_to_iter where) ~xalign:1.0 where);
                 end
              end;
            end;
            (* Colorize *)
            if buffer#lexical_enabled then (Lexical.tag self#view#buffer);
            buffer#set_modified false;
            begin
              match signal_buffer_changed with
                | None ->
                  signal_buffer_changed <- Some (buffer#connect#changed ~callback:begin fun () ->
                    changed_after_last_autosave <- true;
                    changed_after_last_diff <- true;
                    buffer#set_changed_timestamp (Unix.gettimeofday());
                    buffer#set_changed_after_last_autocomp true
                  end);
                | _ -> ()
            end;
            if not buffer#undo#is_enabled then (buffer#undo#enable());
            load_complete <- true;
            buffer#save_buffer ~filename:buffer#orig_filename () |> ignore;
            (*  *)
            self#set_code_folding_enabled editor#code_folding_enabled#get; (* calls scan_folding_points, if enabled *)
            self#view#matching_delim ();
            Gmisclib.Idle.add ~prio:300 (fun () -> self#compile_buffer ?join:None ());
            (* Bookmarks: offsets to marks *)
            let redraw = ref false in
            List.iter begin fun bm ->
              if bm.Oe.bm_filename = file#filename then
                let mark = (Bookmark.offset_to_mark (self#buffer :> GText.buffer) bm) in
                let callback =
                  if bm.Oe.bm_num >= Bookmark.limit then Some (fun _ ->
                    editor#bookmark_remove ~num:bm.Oe.bm_num;
                    redraw := true;
                    true)
                  else None
                in
                let marker = Gutter.create_marker ~mark ?pixbuf:(Bookmark.icon bm.Oe.bm_num) ?callback () in
                bm.Oe.bm_marker <- Some marker;
                view#gutter.Gutter.markers <- marker :: view#gutter.Gutter.markers
            end project.Prj.bookmarks;
            Project.save_bookmarks project;
            if !redraw then (GtkBase.Widget.queue_draw text_view#as_widget);
            true
          with Glib.Convert.Error (_, message) -> begin
            let message = if project.Prj.encoding <> Some "UTF-8" then (kprintf Convert.to_utf8
              "Cannot convert file\n\n%s\n\nfrom %s codeset to UTF-8.\n\n%s"
                file#filename
                (match project.Prj.encoding with None -> "Default" | Some x -> x)
                message) else message
            in
            let dialog = GWindow.message_dialog ~title:"Text file contains invalid characters."
              ~message_type:`ERROR ~message ~buttons:GWindow.Buttons.ok () in
            (match dialog#run () with _ -> dialog#destroy())
          end;
          false;
        end

  method compile_buffer ?join () =
    let filename = self#get_filename in
    if project.Prj.autocomp_enabled
    && ((project.Prj.in_source_path filename) <> None)
    && (filename ^^^ ".ml" || filename ^^^ ".mli") then begin
      buffer#set_changed_after_last_autocomp false;
      Autocomp.compile_buffer ~project ~editor ~page:self ?join ();
    end else begin
      editor#pack_outline (Cmt_view.empty());
      self#set_outline None;
    end

  method tooltip ?(typ=false) ((*(x, y) as*) location) =
    let location = `XY location in
    if typ then (Opt.may annot_type (fun at -> at#tooltip location));
    if Preferences.preferences#get.Preferences.pref_err_tooltip
    then (error_indication#tooltip location)

  method status_modified_icon = status_modified

  method create_menu () = Editor_menu.create ~editor ~page:self ()

  method set_word_wrap value =
    Gaux.may signal_button_toggle_wrap ~f:button_toggle_wrap#misc#handler_block;
    button_toggle_wrap#set_active value;
    Gaux.may signal_button_toggle_wrap ~f:button_toggle_wrap#misc#handler_unblock;
    text_view#options#set_word_wrap value;
    word_wrap <- value;
    Gmisclib.Idle.add (fun () -> GtkBase.Widget.queue_draw text_view#as_widget);

  method set_show_whitespace value =
    Gaux.may signal_button_toggle_whitespace ~f:button_toggle_whitespace#misc#handler_block;
    button_toggle_whitespace#set_active value;
    Gaux.may signal_button_toggle_whitespace ~f:button_toggle_whitespace#misc#handler_unblock;
    text_view#options#set_show_whitespace_chars value;
    show_whitespace <- value;
    Gmisclib.Idle.add (fun () -> GtkBase.Widget.queue_draw text_view#as_widget);

  method button_dep_graph = button_dotview

  method show_revision_history () =
    let rev = Revisions.create ~page:self in
    let hbox = GPack.hbox ~spacing:1 () in
    let _ = GMisc.image ~pixbuf:Icons.history ~packing:hbox#pack () in
    let label = GMisc.label ~text:(sprintf "\xC2\xAB%s\xC2\xBB history" (Filename.basename self#get_filename)) ~packing:hbox#pack () in
    Messages.vmessages#append_page ~label_widget:hbox#coerce ~with_spinner:false rev;
    rev#set_title label#text;
    rev#present();
    rev#set_icon None;

  method private show_dep_graph () =
    match dotview with
      | Some widget ->
        widget#destroy();
        dotview <- None;
        textbox#misc#show();
        List.iter (fun b -> b#misc#set_sensitive true)
          [button_font_incr; button_font_decr; button_rowspacing_incr; button_rowspacing_decr; button_h_prev; button_h_next; button_h_last];
        List.iter (fun b -> b#misc#set_sensitive true) [button_toggle_wrap; button_toggle_whitespace];
        hscrollbar#misc#show();
        status_pos_box#misc#show();
        sep_status_pos_box#misc#show();
      | None ->
        begin
          let reset_button () =
            Gaux.may signal_button_dotview ~f:button_dotview#misc#handler_block;
            button_dotview#set_active false;
            Gaux.may signal_button_dotview ~f:button_dotview#misc#handler_unblock;
          in
          try
            Opt.may_default (editor#project.Prj.in_source_path self#get_filename) begin fun filename ->
              let filename = String.concat "/" (Miscellanea.filename_split filename) in
              let on_ready_cb viewer =
                Opt.may viewer begin fun viewer ->
                  if button_dotview#active then begin
                    textbox#misc#hide();
                    vbox#reorder_child viewer#coerce ~pos:0;
                    dotview <- Some viewer;
                    List.iter (fun b -> b#misc#set_sensitive false)
                      [button_font_incr; button_font_decr; button_rowspacing_incr; button_rowspacing_decr;
                      button_h_prev; button_h_next; button_h_last];
                    List.iter (fun b -> b#misc#set_sensitive false) [button_toggle_wrap; button_toggle_whitespace];
                    hscrollbar#misc#hide();
                    status_pos_box#misc#hide();
                    sep_status_pos_box#misc#hide();
                  end
                end
              in
              match Dot.draw ~project:editor#project ~filename ~packing:vbox#add ~on_ready_cb () with
                | None -> reset_button() | _ -> ();
            end begin fun () ->
              let title = "Could not show dependency graph" in
              let message = sprintf "%s for file: \n\n%s" title self#get_filename in
              Dialog.message ~title ~message `INFO;
              reset_button ()
            end ()
          with ex ->
            reset_button();
            Dialog.display_exn ~parent:self ~title:"Error creating dependency graph" ex
        end;

  method cut () =
    self#copy ();
    self#delete ()

  method copy () =
    let clipboard = GData.clipboard Gdk.Atom.clipboard in
    clipboard#set_text (buffer#selection_text ());

  method delete () =
    let start, stop = buffer#selection_bounds in
    buffer#delete ~start ~stop

  method paste () =
    match (GData.clipboard (Gdk.Atom.clipboard))#text with
      | None -> ()
      | Some text ->
        if view#editable then begin
          GtkSignal.stop_emit();
          ignore (buffer#delete_interactive ~start:(buffer#get_iter `INSERT) ~stop:(buffer#get_iter `SEL_BOUND) ());
          buffer#insert text;
          let iter = buffer#get_iter `INSERT in
          if buffer#lexical_enabled then begin
            Lexical.tag view#buffer
              ~start:(iter#backward_chars (Glib.Utf8.length text))#backward_line
              ~stop:iter#forward_line;
          end;
          buffer#remove_tag view#highlight_current_line_tag
            ~start:(iter#backward_chars (Glib.Utf8.length text))#backward_line
            ~stop:iter#forward_line;
          view#draw_current_line_background ~force:true iter;
          Gmisclib.Idle.add (fun () -> ignore (view#scroll_to_iter iter));
        end

  initializer
    global_gutter#misc#connect#query_tooltip ~callback:begin fun ~x ~y ~kbd tooltip ->
      try
        let _, f = List.find (fun ((x0, y0, w, h), _) -> x0 <= x && x <= x0 + w && y0 <= y && y <= y0 + h) global_gutter_tooltips in
        GtkBase.Tooltip.set_custom tooltip (f())#as_widget;
        true
      with Not_found -> false
    end |> ignore;
    ignore (self#misc#connect#destroy ~callback:begin fun () ->
      Opt.may file (fun f -> f#cleanup())
    end);
    annot_type <- Some (new Annot_type.annot_type ~page:self);
    (**  *)
    view#hyperlink#enable();
    self#set_tag_annot_background();
    (** Expose: Statusbar *)
    let signal_expose = ref (self#view#event#connect#after#expose ~callback:begin fun _ ->
        let iter = self#buffer#get_iter `INSERT in
        status_pos_lin#set_text (string_of_int (iter#line + 1));
        status_pos_col#set_text (string_of_int iter#line_offset);
        status_pos_off#set_text (string_of_int iter#offset);
        false
      end)
    in
    ignore (vscrollbar#connect#value_changed ~callback:(fun () -> view#misc#handler_block !signal_expose));
    ignore (vscrollbar#connect#after#value_changed ~callback:(fun () ->
        Gmisclib.Idle.add ~prio:300 (fun () -> view#misc#handler_unblock !signal_expose)));
    (** After focus_in, check whether the file is changed on disk *)
    ignore (text_view#event#connect#after#focus_in ~callback:begin fun _ ->
      Gaux.may self#file ~f:begin fun f ->
        if f#exists && f#changed then
          if buffer#modified then begin
            let message = "File\n" ^ self#get_filename^"\nchanged on disk, reload?" in
            let yes = "Reload", self#revert in
            let no = "Do Not Reload", (fun () -> self#set_file (Some f)) in
            ignore (Dialog.confirm ~title:"Reload File" ~message ~yes ~no self);
          end else self#revert();
        self#set_read_only f#is_readonly;
        if read_only then begin
          status_modified#set_pixbuf Icons.lock_14;
          status_modified#misc#set_tooltip_text "Read-only"
        end;
      end;
      false
    end);
    (** Clean up type annotation tag *)
    ignore (text_view#event#connect#scroll ~callback:(fun _ -> Opt.may annot_type (fun at -> at#remove_tag()); error_indication#hide_tooltip(); false));
    ignore (text_view#event#connect#leave_notify ~callback:(fun _ -> Opt.may annot_type (fun at -> at#remove_tag()); error_indication#hide_tooltip(); false));
    ignore (text_view#event#connect#focus_out ~callback:(fun _ -> Opt.may annot_type (fun at -> at#remove_tag()); error_indication#hide_tooltip(); false));
    (** Horizontal scrollbar appears/disappears according to the window size *)
    ignore (sw#misc#connect#size_allocate ~callback:begin fun _ ->
      let alloc = sw#misc#allocation in
      if not resized then (spaned#set_position ((alloc.Gtk.width) * 6 / 10));
      if hscrollbar#adjustment#page_size = hscrollbar#adjustment#upper
      then (hscrollbar#misc#hide(); (*first_sep#misc#show()*)) else (hscrollbar#misc#show(); (*first_sep#misc#hide()*));
      resized <- true;
    end);
    (** Hyperlinks *)
    ignore (self#view#hyperlink#connect#hover ~callback:begin fun (bounds, iter) ->
      if iter#inside_word then begin
        match editor#get_definition iter with
          | None -> ()
          | Some _ ->
            match
              Binannot_ident.find_ident
                ~project ~filename:self#get_filename ~offset:iter#offset
                ~compile_buffer:(fun () -> self#compile_buffer ?join:(Some true)) ()
            with
              | Some ident_at_iter ->
                let _, start, stop = Binannot.cnum_of_loc ident_at_iter.Binannot.ident_loc.Location.loc in
                let start = buffer#get_iter (`OFFSET start) in
                let stop = buffer#get_iter (`OFFSET stop) in
                bounds := Some (start, stop)
              | _ -> ()
      end
    end);
    ignore (self#view#hyperlink#connect#activate ~callback:begin fun iter ->
      editor#scroll_to_definition ~page:self ~iter;
    end);
    (** Spinner *)
    let activate_spinner (active : Activity.t list) =
      match active with
        | [] ->
          spinner#set_pixbuf Icons.empty_14;
          spinner#misc#set_tooltip_text "";
        | msgs ->
          let msgs = snd (List.split msgs) in
          spinner#set_file (App_config.application_icons // "spinner.gif");
          spinner#misc#set_tooltip_text (String.concat "\n" (List.rev msgs));
    in
    ignore (Activity.table#connect#changed ~callback:activate_spinner);
    activate_spinner Activity.table#get;
    (** Code folding *)
    ignore (ocaml_view#code_folding#connect#toggled ~callback:begin fun _(*(expand, _, _)*) ->
      error_indication#paint_global_gutter()
    end);
    (** Mark occurrences *)
    ignore (view#mark_occurrences_manager#connect#mark_set ~callback:error_indication#paint_global_gutter);
    (** Special bookmarks: draw an horizontal line.  *)
    let start_selection = ref None in
    let _ = text_view#event#connect#after#button_press ~callback:begin fun ev ->
      let window = GdkEvent.get_window ev in
      match text_view#get_window `LEFT with
        | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
          let y0 = Gdk.Rectangle.y text_view#visible_rect in
          let y = GdkEvent.Button.y ev in
          let where = fst (text_view#get_line_at_y ((int_of_float y) + y0)) in
          start_selection := Some where;
          false
        | _ -> false
    end in
    let _ = text_view#event#connect#after#button_release ~callback:begin fun ev ->
      let window = GdkEvent.get_window ev in
      match text_view#get_window `LEFT with
        | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
          let y0 = Gdk.Rectangle.y text_view#visible_rect in
          let y = GdkEvent.Button.y ev in
          let where = fst (text_view#get_line_at_y ((int_of_float y) + y0)) in
          begin
            match !start_selection with
              | Some started when started#equal where ->
                begin
                  match Project.find_bookmark project self#get_filename buffer#as_gtext_buffer where with
                    | Some bm when bm.Oe.bm_num >= Bookmark.limit ->
                      editor#bookmark_remove ~num:bm.Oe.bm_num;
                      GtkBase.Widget.queue_draw text_view#as_widget;
                    | _ ->
                      let num = Project.get_actual_maximum_bookmark project in
                      let num = if num >= Bookmark.limit then num + 1 else Bookmark.limit in
                      let callback (_ : Gtk.text_mark) =
                        editor#bookmark_remove ~num;
                        GtkBase.Widget.queue_draw text_view#as_widget;
                        true
                      in
                      editor#bookmark_create ~num ?where:(Some where) ?callback:(Some callback) ();
                end;
              | _ -> ()
          end;
          start_selection := None;
          false
        | _ -> false
    end in
    (**  *)
    let callback _ = self#set_word_wrap (not word_wrap) in
    self#set_word_wrap word_wrap;
    signal_button_toggle_wrap <- Some (button_toggle_wrap#connect#clicked ~callback);
    let callback _ = self#set_show_whitespace (not show_whitespace) in
    self#set_show_whitespace show_whitespace;
    signal_button_toggle_whitespace <- Some (button_toggle_whitespace#connect#clicked ~callback);
    (** Dotview *)
    if Oe_config.dot_version <> None then begin
      (signal_button_dotview <- Some (button_dotview#connect#clicked ~callback:self#show_dep_graph));
      button_dotview#misc#set_tooltip_text (Menu_view.get_switch_viewer_label (Some self));
      button_dotview#misc#set_sensitive (Menu_view.get_switch_view_sensitive editor#project self);
    end

  method connect = new signals ~file_changed
end

(** Signals *)
and file_changed () = object inherit [Editor_file.file option] signal () end

and signals ~file_changed =
object
  inherit ml_signals [file_changed#disconnect]
  method file_changed = file_changed#connect ~after
end
