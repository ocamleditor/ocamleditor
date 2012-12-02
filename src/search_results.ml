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
open GdkKeysyms
open GUtil
open Gobject
open Find_text
open Miscellanea
open Location
open Lexing

type t = {
  filename      : string;
  real_filename : string;
  timestamp     : float;
  locations     : (GdkPixbuf.pixbuf option * Location.t) list; (* filename, start_offset, stop_offset *)
}

(** widget *)
class widget ~(editor : Editor.editor) ?packing () =
  let search_started    = new search_started () in
  let search_finished   = new search_finished () in
  let vbox              = GPack.vbox ?packing () in
  let paned             = GPack.paned `HORIZONTAL ~packing:vbox#add () in
  (* Toolbar *)
  let toolbar           = GButton.toolbar ~style:`ICONS ~orientation:`HORIZONTAL ~packing:vbox#pack () in
  let _                 = toolbar#set_icon_size `MENU in
  let button_stop       = GButton.tool_button ~stock:`STOP ~packing:toolbar#insert () in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_prev_file  = GButton.tool_button ~stock:`MEDIA_REWIND ~packing:toolbar#insert () in
  let _                 = button_prev_file#misc#set_tooltip_text "Previous file" in
  let button_prev_line  = GButton.tool_button ~stock:`MEDIA_PREVIOUS ~packing:toolbar#insert () in
  let _                 = button_prev_line#misc#set_tooltip_text "Previous line" in
  let button_next_line  = GButton.tool_button ~stock:`MEDIA_NEXT ~packing:toolbar#insert () in
  let _                 = button_next_line#misc#set_tooltip_text "Next line" in
  let button_next_file  = GButton.tool_button ~stock:`MEDIA_FORWARD ~packing:toolbar#insert () in
  let _                 = button_next_file#misc#set_tooltip_text "Next file" in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_remove     = GButton.tool_button ~stock:`REMOVE ~packing:toolbar#insert () in
  let _                 = button_remove#misc#set_tooltip_text "Remove entry" in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_restart    = GButton.tool_button ~stock:`REFRESH ~packing:toolbar#insert () in
  let _                 = button_restart#misc#set_tooltip_text "Repeat current search" in
  let button_new_search = GButton.tool_button ~stock:`FIND ~packing:toolbar#insert () in
  let _                 = button_new_search#misc#set_tooltip_text "New search" in
  let _                 = GButton.separator_tool_item ~packing:toolbar#insert () in
  let item_message      = GButton.tool_item ~packing:toolbar#insert () in
  let label_message     = GMisc.label ~markup:"" ~packing:item_message#add () in
  (* Scrolled windows *)
  let lsw               = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
    ~packing:(paned#pack1 ~resize:false ~shrink:true) () in
  let rsw               = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:(paned#pack2 ~resize:true ~shrink:true) () in
  (* Model and view for files *)
  let cols              = new GTree.column_list in
  let col_file          = cols#add Gobject.Data.string in
  let col_hits          = cols#add Gobject.Data.int in
  let col_path          = cols#add Gobject.Data.string in
  let col_entry         = cols#add Gobject.Data.caml in
  let col_nlines        = cols#add Gobject.Data.int in
  let model_files       = GTree.list_store cols in
  let view_files        = GTree.view ~model:model_files ~headers_clickable:true ~packing:lsw#add () in
  let _                 = view_files#misc#set_property "enable-grid-lines" (`INT 3) in
  let _                 = Gaux.may Oe_config.find_text_output_font_condensed ~f:view_files#misc#modify_font_by_name in
  let renderer          = GTree.cell_renderer_text [(*`STRETCH `CONDENSED*)] in
  let vc_file           = GTree.view_column ~title:"File" ~renderer:(renderer, ["text", col_file]) () in
  let vc_hits           = GTree.view_column ~title:"Hits" ~renderer:(renderer, ["text", col_hits]) () in
  let vc_path           = GTree.view_column ~title:"Directory" ~renderer:(renderer, ["text", col_path]) () in
  let _                 = view_files#append_column vc_file in
  let _                 = view_files#append_column vc_hits in
  let _                 = view_files#append_column vc_path in
  let _                 = vc_file#set_resizable true in
  let _                 = vc_hits#set_resizable true in
  let _                 = vc_path#set_resizable true in
  let _                 = vc_file#set_sort_column_id 0 in
  let _                 = vc_hits#set_sort_column_id 1 in
  let _                 = vc_path#set_sort_column_id 2 in
  (* Model and view for lines *)
  let pref              = Preferences.preferences#get in
  let gutter_bg_color   = Color.name (Color.set_value 0.93 (`NAME (fst pref.Preferences.pref_bg_color))) in
  let cols              = new GTree.column_list in
  let col_pixbuf        = cols#add ((Gobject.Data.gobject_option : (GdkPixbuf.pixbuf option) Gobject.data_conv)) in
  let col_markup        = cols#add Gobject.Data.string in
  let col_line_num      = cols#add Gobject.Data.int in
  let col_locations     = cols#add Gobject.Data.caml in
  let model_lines       = GTree.list_store cols in
  let view_lines        = GTree.view ~model:model_lines ~headers_visible:false ~packing:rsw#add () in
  let _                 = view_lines#misc#set_property "enable-grid-lines" (`INT 2) in
  let renderer          = GTree.cell_renderer_text [`YPAD 0; `XPAD 0; `XALIGN 1.0; `CELL_BACKGROUND gutter_bg_color] in
  let renderer_markup   = GTree.cell_renderer_text [`YPAD 2; `XPAD 0; ] in
  let renderer_pixbuf   = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0; `CELL_BACKGROUND gutter_bg_color] in
  let vc_line_num       = GTree.view_column ~title:"" () in
  let _                 = vc_line_num#pack ~expand:false renderer_pixbuf in
  let _                 = vc_line_num#pack ~expand:true renderer in
  let _                 = vc_line_num#add_attribute renderer_pixbuf "pixbuf" col_pixbuf in
  let _                 = vc_line_num#add_attribute renderer "markup" col_line_num in
  let vc_markup         = GTree.view_column ~title:"" ~renderer:(renderer_markup, ["markup", col_markup]) () in
  let _                 = view_lines#append_column vc_line_num in
  let _                 = view_lines#append_column vc_markup in
  (*  *)
  let _                 = view_lines#misc#modify_base [
    `SELECTED, `COLOR (Preferences.tag_color "highlight_current_line");
    `NORMAL,   `NAME (fst pref.Preferences.pref_bg_color);
    `ACTIVE, `NAME gutter_bg_color
  ] in
  let _                 = view_lines#misc#modify_font_by_name pref.Preferences.pref_base_font in
object (self)
  inherit GObj.widget vbox#as_widget
  inherit Messages.page

  val project = editor#project
  val mutable canceled = false
  val mutable hits = 0
  val mutable count_dirs = 0
  val mutable count_files = 0
  val mutable results = []
  val mutable signal_lines_selection_changed = None

  initializer
    ignore (button_remove#connect#clicked ~callback:self#remove_entry);
    ignore (button_stop#connect#clicked ~callback:(fun () -> canceled <- true));
    ignore (button_restart#connect#clicked ~callback:self#start_search);
    ignore (button_new_search#connect#clicked ~callback:self#new_search);
    ignore (button_prev_file#connect#clicked ~callback:self#prev_file);
    ignore (button_next_file#connect#clicked ~callback:self#next_file);
    ignore (button_prev_line#connect#clicked ~callback:self#prev_line);
    ignore (button_next_line#connect#clicked ~callback:self#next_line);
    button_stop#misc#set_sensitive false;
    (* Search signals *)
    ignore (self#connect#search_started ~callback:(fun () -> button_stop#misc#set_sensitive true));
    ignore (self#connect#after#search_finished ~callback:(fun () -> button_stop#misc#set_sensitive false));
    (* Selection changed signals *)
    ignore (view_files#selection#connect#changed ~callback:self#files_selection_changed);
    signal_lines_selection_changed <- Some (view_lines#selection#connect#changed ~callback:self#lines_selection_changed);
    (* Row activated signals *)
    ignore (view_files#connect#row_activated ~callback:self#row_file_activated);
    ignore (view_lines#connect#row_activated ~callback:self#row_line_activated);
    (* Search finished *)
    ignore (self#connect#search_finished ~callback:begin fun () ->
      self#active#set false;
      self#present ();
    end);
    (* Tooltips *)
    let set_tooltips view f =
      view#misc#set_has_tooltip true;
      ignore (view#misc#connect#query_tooltip ~callback: begin fun ~x ~y ~kbd tooltip ->
        try
          begin
            match GtkTree.TreeView.Tooltip.get_context view#as_tree_view ~x ~y ~kbd with
              | (x, y, Some (_, _, row)) ->
                begin
                  match view#get_path_at_pos ~x ~y with
                    | Some (tpath, _, _, _) ->
                      f tooltip row;
                      GtkTree.TreeView.Tooltip.set_row view#as_tree_view tooltip tpath;
                      true
                    | _ -> false
                end
              | _ -> false
          end
        with Not_found | Gpointer.Null -> false
      end);
    in
    set_tooltips view_files begin fun tooltip row ->
      let filename = (model_files#get ~row ~column:col_path) // (model_files#get ~row ~column:col_file) in
      GtkBase.Tooltip.set_text tooltip filename;
    end;
    (*set_tooltips view_lines begin fun tooltip row ->
      let line = model_lines#get ~row ~column:col_markup in
      GtkBase.Tooltip.set_markup tooltip line;
    end;*)

  method start_search () =
    let old_filename =
      match view_files#selection#get_selected_rows with
        | path :: _ ->
          let row = model_files#get_iter path in
          let entry = model_files#get ~row ~column:col_entry in
          Some entry.filename
        | _ -> None
    in
    search_started#call ();
    search_finished#call ();
    match old_filename with
      | Some old_filename ->
        model_files#foreach begin fun _ row ->
          let entry = model_files#get ~row ~column:col_entry in
          if entry.filename = old_filename then begin
            view_files#selection#select_iter row;
            true
          end else false
        end
      | _ -> view_files#selection#select_path (GTree.Path.create [0]);

  method set_results : t list -> unit = fun results ->
    self#clear();
    let dirs = ref [] in
    List.iter begin fun ({filename; locations; _} as entry) ->
      let row_hits = List.length locations in
      let dir = Filename.dirname filename in
      dirs := dir :: !dirs;
      hits <- hits + row_hits;
      count_files <- count_files + 1;
      count_dirs <- count_dirs + (if List.mem dir !dirs then 0 else 1);
      let row = model_files#append () in
      model_files#set ~row ~column:col_file (Filename.basename filename);
      model_files#set ~row ~column:col_path dir;
      model_files#set ~row ~column:col_hits row_hits;
      model_files#set ~row ~column:col_entry entry;
    end results

  method private files_selection_changed () =
    Gaux.may signal_lines_selection_changed ~f:view_lines#selection#misc#handler_block;
    view_lines#selection#unselect_all ();
    model_lines#clear();
    begin
      match view_files#selection#get_selected_rows with
        | path :: _ ->
          let row = model_files#get_iter path in
          let entry = model_files#get ~row ~column:col_entry in
          let nlines = self#set_locations entry in
          model_files#set ~row ~column:col_nlines nlines
        | _ -> ()
    end;
    Gaux.may signal_lines_selection_changed ~f:view_lines#selection#misc#handler_unblock;

  method private set_locations {real_filename; locations; _} =
    let line_nums = List.map (fun (_, loc) -> loc.loc_start.pos_lnum) locations in
    let lines = Miscellanea.get_lines_from_file ~filename:real_filename line_nums in
    let locations_lines =
      List.map begin fun (lnum, line) ->
        let locs = List.filter (fun (_, loc) -> lnum = loc.loc_start.pos_lnum) locations in
        (lnum, line, locs)
      end lines
    in
    List.iter begin fun (lnum, line, locs) ->
      let markup = (*String.trim*) line in
      let markup = Glib.Markup.escape_text (Convert.to_utf8 markup) in
      let pixbuf = List.fold_left (fun acc (p, _) -> max p acc) None locs in
      let row = model_lines#append () in
      model_lines#set ~row ~column:col_locations locs;
      model_lines#set ~row ~column:col_pixbuf pixbuf;
      model_lines#set ~row ~column:col_line_num lnum;
      model_lines#set ~row ~column:col_markup markup;
    end locations_lines;
    List.length locations_lines

  method private lines_selection_changed ?(focus=false) () =
    match view_files#selection#get_selected_rows with
      | path :: _ ->
        let row = model_files#get_iter path in
        let {filename; timestamp; _} = model_files#get ~row ~column:col_entry in
        begin
          match editor#get_page (`FILENAME filename) with
            | Some page ->
              if not page#load_complete then (ignore (editor#load_page ~scroll:false page));
              begin
                match view_lines#selection#get_selected_rows with
                  | path :: _ ->
                    let buffer = page#buffer in
                    let ts = buffer#changed_timestamp in
                    if ts > timestamp then (view_lines#misc#set_sensitive false)
                    else begin
                      view_lines#misc#set_sensitive true;
                      let row = model_lines#get_iter path in
                      let locs = model_lines#get ~row ~column:col_locations in
                      match locs with
                        | (_, loc) :: _ ->
                          let start = buffer#get_iter (`OFFSET loc.loc_start.pos_cnum) in
                          let stop = buffer#get_iter (`OFFSET loc.loc_end.pos_cnum) in
                          buffer#select_range start stop;
                          page#ocaml_view#scroll_lazy start;
                          editor#goto_view page#view;
                          if focus then page#view#misc#grab_focus()
                        | _ -> ()
                    end;
                    (*Gaux.may signal_lines_selection_changed ~f:view_lines#selection#misc#handler_block;
                    view_lines#set_cursor ~cell:renderer_markup ~edit:true path vc_markup;
                    Gaux.may signal_lines_selection_changed ~f:view_lines#selection#misc#handler_unblock;*)
                  | _ -> ()
              end
            | _ ->
              ignore (editor#open_file ~active:false ~scroll_offset:0 ~offset:0 filename);
              self#lines_selection_changed()
        end;
      | _ -> ()

  method private row_line_activated _ _ = self#lines_selection_changed ~focus:true ()

  method private row_file_activated path _ =
    let row = model_files#get_iter path in
    let entry = model_files#get ~row ~column:col_entry in
    if List.length entry.locations = 1 then begin
      let path0 = GTree.Path.create [0] in
      view_lines#selection#select_path path0;
      self#row_line_activated path0 vc_line_num
    end

  method clear () =
    hits <- 0;
    count_dirs <- 0;
    count_files <- 0;
    results <- [];
    canceled <- false;
    view_lines#misc#set_sensitive true;
    GtkThread2.sync model_lines#clear ();
    GtkThread2.sync model_files#clear ();
    GtkThread2.sync label_message#set_label "Searching...";

  method prev_file () =
    let path = match view_files#selection#get_selected_rows with
      | path :: _ ->
        ignore (GTree.Path.prev path);
        path
      | _ -> GTree.Path.create [List.length results - 1]
    in
    view_files#selection#select_path path;
    view_files#scroll_to_cell ~align:(0.6, 0.0) path vc_file;

  method next_file () =
    let path = match view_files#selection#get_selected_rows with
      | path :: _ ->
        GTree.Path.next path;
        path
      | _ -> GTree.Path.create [0]
    in
    view_files#selection#select_path path;
    if (GTree.Path.get_indices path).(0) < List.length results then
      view_files#scroll_to_cell ~align:(0.4, 0.0) path vc_file;

  method prev_line () = self#move_line `PREV
  method next_line () = self#move_line `NEXT

  method private move_line dir =
    let get_last () =
      match view_files#selection#get_selected_rows with
        | path :: _ ->
          let nlines = model_files#get ~row:(model_files#get_iter path) ~column:col_nlines in
          Some (GTree.Path.create [nlines -1])
        | _ -> None
    in
    let path =
      match view_lines#selection#get_selected_rows with
        | [] when dir = `NEXT -> Some (GTree.Path.create [0])
        | [] when dir = `PREV -> get_last()
        | path :: _ when dir = `NEXT ->
          begin
            match get_last() with
              | Some last when GTree.Path.to_string last = GTree.Path.to_string path -> Some (GTree.Path.create [0])
              | _ -> GTree.Path.next path; Some path
          end;
        | path :: _ when dir = `PREV ->
          if GTree.Path.prev path then (Some path) else (get_last())
        | _ -> None
    in
    Gaux.may path ~f:begin fun path ->
      view_lines#selection#select_path path;
      view_lines#scroll_to_cell path vc_line_num;
    end

  method remove_entry () =
    match view_files#selection#get_selected_rows with
      | path :: _ ->
        let next =
          if GTree.Path.to_string path = GTree.Path.to_string (GTree.Path.create [count_files - 1])
          then let next = GTree.Path.copy path in ignore ((GTree.Path.prev next)); next
          else path
        in
        ignore (model_files#remove (model_files#get_iter path));
        count_files <- count_files - 1;
        view_files#selection#select_path next;
        view_files#scroll_to_cell ~align:(0.5, 0.0) next vc_file;
      | _ -> ()

  method parent_changed messages = ()
  method new_search () = ()
  method button_stop = button_stop
  method button_remove = button_remove
  method button_restart = button_restart
  method button_new_search = button_new_search
  method button_prev_file = button_prev_file
  method button_next_file = button_next_file
  method button_prev_line = button_prev_line
  method button_next_line = button_next_line
  method label_message = label_message

  method connect = new signals ~search_started ~search_finished
end

and search_started () = object inherit [unit] signal () end
and search_finished () = object inherit [unit] signal () end
and signals ~search_started ~search_finished =
object
  inherit ml_signals [search_started#disconnect; search_finished#disconnect]
  method search_started = search_started#connect ~after
  method search_finished = search_finished#connect ~after
end
