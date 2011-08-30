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

(** file_select *)
let file_select ~editor () =
  let window = GWindow.window ~title:"Switch"
    ~width:600 ~height:400 ~modal:true
    ~position:`CENTER ~border_width:5 ~show:false () in
  Gaux.may (GWindow.toplevel editor) ~f:(fun x -> window#set_transient_for x#as_window);
  let vbox = GPack.vbox ~spacing:0 ~packing:window#add () in
  let cols = new GTree.column_list in
  let col_icon  = cols#add Gobject.Data.string in
  let col_name  = cols#add Gobject.Data.string in
  let col_path  = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [] in
  let renderer_bold = GTree.cell_renderer_text [] in
  let renderer_icon = GTree.cell_renderer_pixbuf [] in
  renderer_bold#set_properties [`WEIGHT `BOLD];
  let vc_name = GTree.view_column ~title:"File" ~renderer:(renderer_bold, ["text", col_name]) () in
  let vc_path = GTree.view_column ~title:"Path" ~renderer:(renderer, ["text", col_path]) () in
  let vc_icon = GTree.view_column ~title:"" ~renderer:(renderer_icon, ["stock-id", col_icon]) () in
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:vbox#add () in
  let view = GTree.view ~model:model ~headers_visible:false ~reorderable:true ~width:130 ~packing:sw#add () in
  view#append_column vc_icon;
  view#append_column vc_name;
  view#append_column vc_path;
  view#selection#set_mode `MULTIPLE;
  view#set_search_column 1;
  List.iter begin fun p ->
    let row = model#append() in
    let _, _, label = p#tab_widget in
    model#set ~row ~column:col_name label#text;
    model#set ~row ~column:col_path p#get_filename;
    let opened, changed =
      match editor#get_page (Oe.Page_file (File.create p#get_filename ())) with
        | None -> false, false
        | Some page -> true, page#view#buffer#modified
    in
    model#set ~row ~column:col_icon (if changed then "gtk-floppy" else "");
  end editor#pages;
  let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~border_width:8 ~spacing:8
    ~packing:(vbox#pack ~expand:false) () in
  let button_close = GButton.button ~label:"Close files" ~packing:bbox#add () in
  let button_cancel = GButton.button ~label:"Done" ~packing:bbox#add () in
  button_cancel#connect#clicked ~callback:window#destroy;
  let activate () =
    List.iter begin fun path ->
      let row = model#get_iter path in
      let filename = model#get ~row ~column:col_path in
      ignore (editor#open_file ~active:true ~offset:0 filename)
    end view#selection#get_selected_rows;
    window#destroy()
  in
  view#connect#row_activated ~callback:(fun _ _ -> activate ());
  button_close#connect#clicked ~callback:begin fun () ->
    let closing = ref [] in
    List.iter begin fun path ->
      let row = model#get_iter path in
      let filename = model#get ~row ~column:col_path in
      let page = editor#open_file ~active:true ~offset:0 filename in
      Gaux.may (editor#get_page Oe.Page_current) ~f:editor#dialog_confirm_close;
      closing := row :: !closing;
    end view#selection#get_selected_rows;
    ignore (List.map model#remove !closing)
  end;
  window#event#connect#key_release ~callback:begin fun ev ->
    let key = GdkEvent.Key.keyval ev in
    if key = GdkKeysyms._Escape then (window#destroy(); true)
    else begin
      window#present();
      false
    end
  end;
  editor#with_current_page begin fun page ->
    model#foreach begin fun _ row ->
      let filename = model#get ~row ~column:col_path in
      if filename = page#get_filename then begin
        view#selection#select_iter row;
        false
      end else false
    end;
    window#misc#set_can_focus true;
    window#misc#grab_focus();
    view#misc#grab_focus();
    window#present()
  end


(** confirm_close *)
let confirm_close ~editor (page : Editor_page.page) =
  if page#buffer#modified then begin
    let dialog = GWindow.dialog
      ~title:"Close Modified File"
      ~urgency_hint:true
      ~position:`CENTER
      ~modal:true
      ~border_width:8
      ~icon:Icons.oe
      () in
    dialog#add_button "Save" `SAVE;
    dialog#add_button "Do Not Save" `DO_NOT_SAVE;
    dialog#add_button "Cancel" `CANCEL;
    dialog#set_default_response `CANCEL;
    dialog#vbox#set_spacing 8;
    let hbox = GPack.hbox ~spacing:8 ~packing:dialog#vbox#add () in
    let _ = GMisc.image ~stock:`SAVE ~icon_size:`DIALOG ~packing:hbox#pack () in
    let text = sprintf "File modified: \xC2\xAB%s\xC2\xBB. Do you wish to save changes?"
      (Filename.basename page#get_filename) in
    let _ = GMisc.label ~xalign:0.0 ~text ~packing:hbox#add () in
    match dialog#run () with
      | `SAVE ->
        editor#save page;
        editor#close page;
        dialog#destroy()
      | `DO_NOT_SAVE ->
        editor#close page;
        dialog#destroy()
      | _ -> dialog#destroy()
  end else (editor#close page)


(** save_modified *)
let save_modified ~editor ~close ~callback pages =
  if pages <> [] then begin
    let dialog = GWindow.dialog ~position:`CENTER ~border_width:5 ~no_separator:true
      ~icon:Icons.oe ~modal:true ~title:"Save Modified" () in
    let checklist = new Checklist.checklist
      ~packing:dialog#vbox#add
      (List.map (fun (x, p) -> x, p#get_filename) pages) in
    dialog#add_button_stock `OK `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#action_area#add checklist#button_all#coerce;
    dialog#action_area#add checklist#button_none#coerce;
    dialog#action_area#set_child_secondary checklist#button_all#coerce true;
    dialog#action_area#set_child_secondary checklist#button_none#coerce true;
    match dialog#run () with
      | `OK ->
        checklist#iter begin fun save filename ->
          let _, page = List.find (fun (_, p) -> p#get_filename = filename) pages in
          if save then (editor#save page) else (Autosave.delete ~filename ());
          if close then editor#close page;
        end;
        dialog#destroy();
        callback();
      | _ -> dialog#destroy()
  end else (callback())


(** save_as_rename *)
let save_as_rename ~editor ~action (page : Editor_page.page) =
  match page#file with
    | None -> ()
    | Some file ->
      let title, f = match action with
        | `SAVE_AS ->
          let save_as ~replace new_filename =
            try
              let outchan = open_out_bin new_filename in
              lazy begin
                File.write new_filename (page#buffer#get_text());
                ignore (editor#open_file ~active:true ~offset:0 new_filename)
              end /*finally*/ lazy (close_out outchan);
            with ex -> Dialog.display_exn editor ex
          in
          "Save As...", save_as
        | `RENAME ->
          let rename ~replace new_filename =
            try
              if replace then begin
                Gaux.may ~f:editor#close (editor#get_page (Oe.Page_file file));
              end;
              Project.rename_file editor#project file new_filename;
              let _, _, label = page#tab_widget in
              label#set_text (Editor_page.shortname new_filename);
              page#update_statusbar()
            with ex -> Dialog.display_exn editor ex
          in
          "Rename File...", rename
      in
      let window = GWindow.file_chooser_dialog ~action:`SAVE ~position:`CENTER
        ~width:640 ~modal:true ~title ~show:false () in
      window#set_select_multiple false;
      window#add_select_button_stock `OK `OK;
      window#add_button_stock `CANCEL `CANCEL;
      window#set_default_response `OK;
      window#set_filename page#get_filename;
      match window#run () with
        | `OK ->
          begin
            try
              let new_filename = List.hd window#get_filenames in
              let same_name_case_insensitive = action = `RENAME &&
                (String.uppercase (Filename.basename new_filename)) =
                (String.uppercase (Filename.basename page#get_filename)) in
              if same_name_case_insensitive then begin
                f ~replace:false new_filename;
                window#destroy()
              end else if Sys.file_exists new_filename then begin
                if action = `RENAME && new_filename = page#get_filename then begin
                   window#destroy()
                end else begin
                  let message = Glib.Convert.convert ~from_codeset:"" ~to_codeset:"utf8"
                    (sprintf "File\n%s\nexists, replace?" new_filename) in
                  let question = GWindow.message_dialog ~message_type:`QUESTION
                    ~position:`CENTER ~message ~buttons:GWindow.Buttons.ok_cancel () in
                  match question#run () with
                    | `OK ->
                      Sys.remove new_filename;
                      f ~replace:true new_filename;
                      question#destroy();
                      window#destroy();
                    | _ ->
                      window#destroy();
                      question#destroy();
                      editor#dialog_save_as_rename ~action page
                end
              end else begin
                f ~replace:false new_filename;
                window#destroy()
              end;
            with Failure "hd" -> ()
          end;
        | _ -> window#destroy()


(** file_open *)
let file_open ~editor () =
  let path = editor#project.Project.root // Project.src in
  let filters = [
    ("Source files", ["*.ml*"; "README*"; "INSTALL*";
      "ChangeLog"; "CHANGES"; "NEWS*"; "TODO*"; "BUGS*"; "CONTRIB*";
      "Makefile*"; "*.sh"; "*.bat"; "*.cmd"]);
    ("All files", ["*"])]
  in
  let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~width:600 ~height:600
    ~title:"Open file..." ~icon:Icons.oe ~position:`CENTER ~show:false () in
  List.iter begin fun (name, patterns) ->
    dialog#add_filter (GFile.filter ~name ~patterns ())
  end filters;
  dialog#add_select_button_stock `OK `OK;
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#set_current_folder path;
  dialog#set_select_multiple true;
  match dialog#run () with
    | `OK ->
      List.iter (fun filename ->
        ignore (editor#open_file ~active:true ~offset:0 filename)) dialog#get_filenames;
      dialog#destroy()
    | _ -> dialog#destroy()

