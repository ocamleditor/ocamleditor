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

let menu_item_view_menubar : (unit -> (GMenu.check_menu_item * GtkSignal.id) list ref) ref =
    ref (fun () -> failwith "menu_item_view_menubar")

(** show_whitespace_characters_toggled *)
let show_whitespace_characters_toggled ~editor () =
  let show = not (editor#show_whitespace_chars) in
  editor#set_show_whitespace_chars show;
  Preferences.preferences#get.Preferences.pref_show_whitespace_chars <- show;
  Preferences.save();;

(** toggle_word_wrap_toggled *)
let toggle_word_wrap_toggled ~editor () =
  let wrap = not (editor#word_wrap) in
  editor#set_word_wrap wrap;
  Preferences.preferences#get.Preferences.pref_editor_wrap <- wrap;
  Preferences.save();;

(** create *)
let create ~editor ~page () =
  let get_menu_item_view_menubar () =
    match !(!menu_item_view_menubar ()) with
      | [item, _] -> item
      | _ -> assert false
  in
  let gmenu = GMenu.menu () in
  let cut = GMenu.image_menu_item ~stock:`CUT ~packing:gmenu#append () in
  let copy = GMenu.image_menu_item ~stock:`COPY ~packing:gmenu#append () in
  let paste = GMenu.image_menu_item ~stock:`PASTE ~packing:gmenu#append () in
  let delete = GMenu.image_menu_item ~stock:`DELETE ~packing:gmenu#append () in
  let select_all = GMenu.image_menu_item ~stock:`SELECT_ALL ~packing:gmenu#append () in
  gmenu#append (GMenu.separator_item ());
  (*  *)
  let eval_in_toplevel = GMenu.image_menu_item ~label:"Eval in Toplevel" ~packing:gmenu#append () in
  eval_in_toplevel#set_image (GMisc.image ~stock:`EXECUTE ~icon_size:`MENU ())#coerce;
  ignore (eval_in_toplevel#connect#activate ~callback:begin fun () ->
    editor#with_current_page (fun page -> page#ocaml_view#obuffer#send_to_shell ());
  end);
  let select_in_structure_pane = GMenu.image_menu_item ~label:"Select in Structure Pane" ~packing:gmenu#append () in
  select_in_structure_pane#set_image (GMisc.image ~pixbuf:Icons.select_in_structure ())#coerce;
  ignore (select_in_structure_pane#connect#activate ~callback:begin fun () ->
    editor#with_current_page (fun page ->
      editor#set_show_outline true;
      Gaux.may page#outline ~f:(fun ol -> ol#select ?align:None (page#buffer#get_mark `INSERT)));
  end);
  let find_definition = GMenu.menu_item ~label:"Find Definition" ~packing:gmenu#append () in
  let find_references = GMenu.menu_item ~label:"Find References" ~packing:gmenu#append () in
  gmenu#append (GMenu.separator_item ());
  (* Show Whitespace Characters and Toggle Word-Wrap*)
  let show_whitespace_characters = GMenu.check_menu_item ~label:"Show Whitespace Characters" ~packing:gmenu#append () in
  let toggle_word_wrap = GMenu.check_menu_item ~label:"Toggle Word-Wrap" ~packing:gmenu#append () in
  (*gmenu#append (GMenu.separator_item ());
  (* Show Menubar *)
  let show_menubar = GMenu.check_menu_item ~label:"Show Menubar" ~packing:gmenu#append () in*)
  (*  *)
  let sigids = [
    show_whitespace_characters,
      show_whitespace_characters#connect#toggled ~callback:(show_whitespace_characters_toggled ~editor);
    toggle_word_wrap,
      toggle_word_wrap#connect#toggled ~callback:(toggle_word_wrap_toggled ~editor);
    (*show_menubar,
      show_menubar#connect#activate ~callback:begin fun () ->
        let item = get_menu_item_view_menubar() in
        item#activate();
        item#set_active show_menubar#active;
      end;*)
  ] in
  (*  *)
  ignore (find_definition#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page ->
      ignore (editor#scroll_to_definition (page#buffer#get_iter `INSERT)))));
  ignore (find_references#connect#activate ~callback:begin
    Activity.wrap Activity.Annot begin fun () ->
      editor#with_current_page begin fun page ->
        editor#find_references (page#buffer#get_iter `INSERT)
      end
    end
  end);
  (*  *)
  let callback ev =
    let clip = GData.clipboard Gdk.Atom.clipboard in
    cut#connect#activate ~callback:(fun () -> page#buffer#cut_clipboard ?default_editable:None clip);
    cut#misc#set_sensitive page#buffer#has_selection;
    copy#connect#activate ~callback:(fun () -> page#buffer#copy_clipboard clip);
    copy#misc#set_sensitive page#buffer#has_selection;
    paste#connect#activate ~callback:(fun () ->
      page#buffer#paste_clipboard ?iter:None ?default_editable:None clip);
    paste#misc#set_sensitive (clip#text <> None);
    delete#connect#activate ~callback:(fun () ->
      ignore (page#buffer#delete_selection ?interactive:None ?default_editable:None ()));
    delete#misc#set_sensitive page#buffer#has_selection;
    select_all#connect#activate ~callback:page#buffer#select_all;

    Gmisclib.Idle.add begin fun () ->
      let def_sensitive, ref_sensitive =
        match editor#get_page `ACTIVE with
          | None -> false, false
          | Some page ->
            let iter = page#buffer#get_iter `INSERT in
            if not iter#ends_line && not (Glib.Unichar.isspace iter#char) then begin
              Definition.has_definition_references ~page ~iter
            end else false, false
      in
      find_definition#misc#set_sensitive def_sensitive;
      find_references#misc#set_sensitive ref_sensitive;
    end;

    List.iter (fun (w, s) -> w#misc#handler_block s) sigids;
    show_whitespace_characters#set_active editor#show_whitespace_chars;
    toggle_word_wrap#set_active editor#word_wrap;
    (*show_menubar#set_active (get_menu_item_view_menubar())#active;*)
    List.iter (fun (w, s) -> w#misc#handler_unblock s) sigids;
    gmenu#popup ~button:3 ~time:(GdkEvent.Button.time ev);
    Gdk.Window.set_cursor gmenu#misc#window (Gdk.Cursor.create `ARROW);
    true
  in
  ignore(page#view#event#connect#button_press ~callback:begin fun ev ->
    if (GdkEvent.Button.button ev = 3 && GdkEvent.get_type ev = `BUTTON_PRESS) then begin
      let x = int_of_float (GdkEvent.Button.x ev) in
      let y = int_of_float (GdkEvent.Button.y ev) in
      let x, y = page#view#window_to_buffer_coords ~tag:`TEXT ~x ~y in
      let where = page#view#get_iter_at_location ~x ~y in
      page#buffer#place_cursor ~where;
      if x < page#view#gutter.Gutter.size - page#view#gutter.Gutter.fold_size then begin
        (*  *)
      end else (ignore ((callback ev)));
      true
    end else false
  end);
  ignore (page#view#event#connect#key_press ~callback:begin fun ev ->
    if (GdkEvent.Key.keyval ev = GdkKeysyms._Menu) then (callback ev) else false
  end);
