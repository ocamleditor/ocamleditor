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

let image_menu_item ~label ?(pixbuf=Icons.empty_8) ?stock ?(show=true) ~packing () =
  let menu_item = GMenu.menu_item ~packing ~show () in
  let hbox = GPack.hbox ~border_width: 6 ~packing: menu_item#add () in
  let _image = 
		if Option.is_none stock then
		 GMisc.image ~pixbuf ~icon_size: `MENU ~packing: hbox#add () 
	else
	GMisc.image ?stock ~packing: hbox#add ()
in
  let _label = GMisc.label ~text: label ~packing: hbox#add () in
  menu_item
;;

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
  let cut = image_menu_item ~label:"Cut" ~stock:`CUT ~packing:gmenu#append () in
  let copy = image_menu_item ~label:"Copy" ~stock:`COPY ~packing:gmenu#append () in
  let paste = image_menu_item ~label:"Paste" ~stock:`PASTE ~packing:gmenu#append () in
  let delete = image_menu_item ~label:"Delete" ~stock:`DELETE ~packing:gmenu#append () in
  let select_all = image_menu_item ~label:"Select all" ~stock:`SELECT_ALL ~packing:gmenu#append () in
  gmenu#append (GMenu.separator_item ());
  (*  *)
  if Oe_config.ocp_indent_version <> None then begin
    let indent_all = image_menu_item ~label:"Indent All" ~packing:gmenu#append () in
    ignore (indent_all#connect#activate ~callback:begin fun () ->
      editor#with_current_page (fun page -> ignore (Ocp_indent.indent ~view:page#view `ALL));
    end);
    gmenu#append (GMenu.separator_item ());
  end;
  (*  *)
  let show_doc_at_cursor = image_menu_item ~label:"Show Documentation" ~packing:gmenu#append () in
  show_doc_at_cursor#connect#activate ~callback:editor#show_doc_at_cursor |> ignore;
  show_doc_at_cursor#misc#set_sensitive (Menu_file.get_file_switch_sensitive page);
  let find_definition = image_menu_item ~label:"Find Definition" ~pixbuf: Icons.definition ~packing:gmenu#append () in
  let find_references = image_menu_item ~label:"Find References" ~pixbuf: Icons.references ~packing:gmenu#append () in
  let find_used_components = GMenu.menu_item ~packing:gmenu#append () in
  let label_find_used_components = GMisc.label ~xalign:0. ~markup:"" ~packing:find_used_components#add () in
  (*  *)
  gmenu#append (GMenu.separator_item ());
  let select_in_structure_pane = image_menu_item ~label:"Select in Structure Pane" ~pixbuf: Icons.select_in_structure ~packing:gmenu#append () in
  ignore (select_in_structure_pane#connect#activate ~callback:begin fun () ->
    editor#with_current_page begin
      let sigid = ref None in
      let rec f page =
        if editor#show_outline then begin
          Gaux.may !sigid ~f:begin fun id ->
            editor#disconnect id;
            sigid := None;
          end;
          match page#outline with
            | Some ol ->
              ignore (ol#select_from_buffer ?align:None (page#buffer#get_mark `INSERT))
            | _ -> ()
        end else begin
          sigid := Some (editor#connect#outline_visibility_changed
            ~callback:(function true -> f page | false -> ()));
          editor#set_show_outline true;
        end;
      in f
    end
  end);
  select_in_structure_pane#misc#set_sensitive (Menu_file.get_file_switch_sensitive page);
  (*  *)
  gmenu#append (GMenu.separator_item ());
  let eval_in_toplevel = image_menu_item ~label:"Eval in Toplevel" ~pixbuf: Icons.toplevel ~packing:gmenu#append () in
  ignore (eval_in_toplevel#connect#activate ~callback:begin fun () ->
    editor#with_current_page (fun page -> page#ocaml_view#obuffer#send_to_shell ());
  end);
  eval_in_toplevel#misc#set_sensitive (Menu_file.get_file_switch_sensitive page);
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
      ignore (editor#scroll_to_definition ~page ~iter:(page#buffer#get_iter `INSERT)))));
  ignore (find_references#connect#activate ~callback:begin
    Activity.wrap Activity.Annot (fun () -> Menu_search.find_definition_references editor)
  end);
  ignore (find_used_components#connect#activate ~callback:(fun () -> Menu_search.find_used_components editor));
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

    Menu_search.update_items_visibility
      ~label_find_used_components
      ~find_used_components
      ~find_definition
      ~find_references
      editor;

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
      let s1, s2 = page#view#buffer#selection_bounds in
      if where#compare s1 <= 0 || where#compare s2 >= 0 then page#buffer#place_cursor ~where;
      if x < page#view#gutter.Gutter.size - page#view#gutter.Gutter.fold_size then begin
        (*  *)
      end else (ignore ((callback ev)));
      true
    end else false
  end);
  ignore (page#view#event#connect#key_press ~callback:begin fun ev ->
    if (GdkEvent.Key.keyval ev = GdkKeysyms._Menu) then (callback ev) else false
  end);
