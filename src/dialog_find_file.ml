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

open Miscellanea

let create ?(all=true) ~(editor : Editor.editor) ~path () =
  let title = if all then "Find File..." else "Select File..." in
  let window = GWindow.window ~title ~icon:Icons.oe
    ~width:600 ~height:400 ~modal:true ~position:`CENTER ~border_width:5 ~show:false () in
(*    Gaux.may (GWindow.toplevel vbox) ~f:(fun x -> window#set_transient_for x#as_window);*)
  let vbox = GPack.vbox ~spacing:8 ~packing:window#add () in
  let entry = GEdit.entry ~packing:(vbox#pack ~expand:false) () in
  let cols = new GTree.column_list in
  let col_icon  = cols#add Gobject.Data.string in
  let col_name  = cols#add Gobject.Data.string in
  let col_path  = cols#add Gobject.Data.string in
  let col_changed  = cols#add Gobject.Data.boolean in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [] in
  let renderer_bold = GTree.cell_renderer_text [] in
  let renderer_bool = GTree.cell_renderer_toggle [] in
  let renderer_icon = GTree.cell_renderer_pixbuf [] in
  renderer_bold#set_properties [(*`WEIGHT `BOLD*)];
  renderer_icon#set_properties [`STOCK_SIZE `MENU];
  let vc_icon = GTree.view_column ~title:"" ~renderer:(renderer_icon, ["stock-id", col_icon]) () in
  let vc_name = GTree.view_column ~title:"File" ~renderer:(renderer_bold, ["text", col_name]) () in
  let vc_path = GTree.view_column ~title:"Path" ~renderer:(renderer, ["text", col_path]) () in
  let vc_changed = GTree.view_column ~title:"Changed"
    ~renderer:(renderer_bool, ["active", col_changed]) () in
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:vbox#add () in
  let view = GTree.view ~model:model ~headers_visible:false ~reorderable:true ~width:130
    ~enable_search:false ~packing:sw#add () in
  view#append_column vc_icon;
  view#append_column vc_name;
  view#append_column vc_path;
  view#append_column vc_changed;
  vc_changed#set_visible false;
  vc_name#set_cell_data_func renderer_bold begin fun model row ->
    let p = model#get ~row ~column:col_path in
    renderer_bold#set_properties [`WEIGHT (if Miscellanea.filename_relative path p <> None then `BOLD else `NORMAL)]
  end;
  view#selection#set_mode `MULTIPLE;
  let find filter =
    let filenames =
      let files = if all then (File.readdirs path) else [] in
      let recent = List.filter (fun x -> not (List.mem x files))
        editor#file_history.File_history.content in
      let recent = List.filter Sys.file_exists recent in
      let files = List.filter begin fun x ->
        (Filename.check_suffix x ".ml" ||
        Filename.check_suffix x ".mli" ||
        Filename.check_suffix x ".mll" ||
        Filename.check_suffix x ".mly" ||
        Filename.check_suffix x ".bat" ||
        Filename.check_suffix x ".cmd" ||
        Filename.check_suffix x ".txt" ||
        Filename.check_suffix x ".sh")
      end files in
      let files = files @ recent in
      let files = if filter = "" then files else begin
        List.filter begin fun x ->
          (Str.string_match (Str.regexp_case_fold filter) (Filename.basename x) 0)
        end files
      end in
      files
    in
    model#clear();
    List.iter begin fun filename ->
      let row = model#append() in
      model#set ~row ~column:col_name (Filename.basename filename);
      model#set ~row ~column:col_path (Filename.dirname filename);
      let opened, changed =
        match editor#get_page (Editor_types.File (File.create filename ())) with
          | None -> false, false
          | Some page -> true, page#view#buffer#modified
      in
      model#set ~row ~column:col_icon (if changed then "gtk-floppy"
        else (if opened then "gtk-apply" else ""));
      model#set ~row ~column:col_changed changed;
    end filenames;
    view#selection#select_path (GTree.Path.create [0]);
  in
  let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~border_width:8 ~spacing:8
    ~packing:(vbox#pack ~expand:false) () in
  let button_close = GButton.button ~label:"Close Files" ~packing:bbox#add () in
  let button_ok = GButton.button ~label:"Open Files" ~packing:bbox#add () in
  let button_cancel = GButton.button ~label:"Done" ~packing:bbox#add () in
  let button_clear_history = GButton.button ~label:"Clear File History" ~packing:bbox#add () in
  bbox#set_child_secondary button_clear_history#coerce true;
  button_clear_history#connect#clicked ~callback:begin fun () ->
    File_history.clear editor#file_history;
    find entry#text
  end;
  button_cancel#connect#clicked ~callback:window#destroy;
  let activate () =
    List.iter begin fun path ->
      let row = model#get_iter path in
      let path = model#get ~row ~column:col_path in
      let name = model#get ~row ~column:col_name in
      ignore (editor#open_file ~active:true ~offset:0 (Filename.concat path name))
    end view#selection#get_selected_rows;
    window#destroy()
  in
  button_ok#connect#clicked ~callback:activate;
  button_close#connect#clicked ~callback:begin fun () ->
    List.iter begin fun path ->
      let row = model#get_iter path in
      let path = model#get ~row ~column:col_path in
      let name = model#get ~row ~column:col_name in
      editor#open_file ~active:true ~offset:0 (Filename.concat path name);
      Gaux.may (editor#get_page Editor_types.Current) ~f:editor#dialog_confirm_close;
    end view#selection#get_selected_rows;
    find entry#text
  end;
  view#connect#row_activated ~callback:begin fun path _ ->
    activate ();
  end;
  window#event#connect#key_press ~callback:begin fun ev ->
    let key = GdkEvent.Key.keyval ev in
    if key = GdkKeysyms._Escape then (window#destroy(); true)
    else if key = GdkKeysyms._Return then (activate(); true)
    else if key = GdkKeysyms._Down && ((GdkEvent.Key.state ev) = []) then begin
      view#misc#grab_focus();
      (try view#selection#select_path (GTree.Path.create [0]) with _ -> ());
      false
    end else begin
      false
    end
  end;
  view#event#connect#key_press ~callback:begin fun ev ->
    let key = GdkEvent.Key.keyval ev in
    if key = GdkKeysyms._BackSpace then begin
      (try entry#set_text (String.sub entry#text 0 (entry#text_length - 1)) with _ -> ());
      entry#misc#grab_focus();
      entry#set_position entry#text_length;
      true
    end else if key = GdkKeysyms._Left then begin
      entry#misc#grab_focus();
      entry#set_position (entry#text_length - 1);
      true
    end else if key = GdkKeysyms._Right then begin
      entry#misc#grab_focus();
      entry#set_position entry#text_length;
      true
    end else begin
      entry#set_text (entry#text ^ (GdkEvent.Key.string ev));
      false
    end
  end;
  entry#connect#changed ~callback:begin fun () ->
    let filter = (Str.global_replace (!~ "*") "[-+ 'a-zA-Z!^%&$()@#;,_0-9]*" entry#text) in
    find filter;
  end;
  (*find "";*)
  view#set_search_column 1;
  entry#misc#grab_focus();
  window#present()
