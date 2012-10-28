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
open Miscellanea

type t = {
  mutable menu_items                    : GMenu.menu_item list;
  file_rename                           : GMenu.menu_item;
  file_recent_select                    : GMenu.menu_item;
  file_recent_clear                     : GMenu.menu_item;
  file_recent_sep                       : GMenu.menu_item;
  file_switch                           : GMenu.menu_item;
  file_close                            : GMenu.image_menu_item;
  file_close_all                        : GMenu.menu_item;
  file_revert                           : GMenu.image_menu_item;
  file_delete                           : GMenu.image_menu_item;
  window                                : GMenu.menu;
  mutable window_radio_group            : Gtk.radio_menu_item Gtk.group option;
  mutable window_pages                  : (int (* page oid *) * GMenu.radio_menu_item) list;
  mutable window_signal_locked          : bool;
  mutable window_n_childs               : int;
  project                               : GMenu.menu;
  mutable project_history               : (string * GMenu.check_menu_item) list;
  mutable project_history_signal_locked : bool;
}

(** set_label *)
let set_label item text = item#misc#set_property "label" (`STRING (Some text));;

(** file *)
let file ~browser ~group ~flags items =
  let editor = browser#editor in
  let file = GMenu.menu_item ~label:"File" () in
  let menu = GMenu.menu ~packing:file#set_submenu () in
  (** New Project *)
  let new_project = GMenu.menu_item ~label:"New Project..." ~packing:menu#add () in
  ignore (new_project#connect#activate ~callback:browser#dialog_project_new);
  (** New file *)
  let new_file = GMenu.image_menu_item ~image:(GMisc.image ~stock:`NEW ~icon_size:`MENU ())
    ~label:"New File..." ~packing:menu#add () in
  new_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._n ~flags;
  ignore (new_file#connect#activate ~callback:browser#dialog_file_new);
  (** Open Project *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let project_open = GMenu.image_menu_item
    ~label:"Open Project..." ~packing:menu#add () in
  ignore (project_open#connect#activate ~callback:browser#dialog_project_open);
  project_open#add_accelerator ~group ~modi:[`CONTROL;`SHIFT] GdkKeysyms._o ~flags;
  (** Open File *)
  let open_file = GMenu.image_menu_item ~image:(GMisc.image ~stock:`OPEN ~icon_size:`MENU ())
    ~label:"Open File..." ~packing:menu#add () in
  open_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._o ~flags;
  ignore (open_file#connect#activate ~callback:editor#dialog_file_open);
  (** Recent Files... *)
  let file_recent = GMenu.menu_item ~label:"Recent Files" ~packing:menu#add () in
  let file_recent_menu = GMenu.menu ~packing:file_recent#set_submenu () in
  (* file_recent_select *)
  file_recent_menu#add items.file_recent_select;
  ignore (items.file_recent_select#connect#activate ~callback:begin fun () ->
    browser#dialog_find_file ?all:(Some false) ()
  end);
  items.file_recent_select#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._K ~flags;
  (* recent items *)
  let recent_items = ref [] in
  ignore (file_recent#connect#activate ~callback:begin fun () ->
    let count = ref 0 in
    try
      List.iter file_recent_menu#remove !recent_items;
      recent_items := [];
      List.iter begin fun filename ->
        incr count;
        let label = filename in
        let mi = GMenu.menu_item ~label ~packing:(file_recent_menu#insert ~pos:1) () in
        recent_items := mi :: !recent_items;
        ignore (mi#connect#activate ~callback:(fun () ->
          ignore (editor#open_file ~active:true ~scroll_offset:0 ~offset:0 filename)));
        if !count > 30 then (raise Exit)
      end (List.rev editor#file_history.File_history.content);
    with Exit -> ()
  end);
  (* file_recent_clear *)
  file_recent_menu#add items.file_recent_sep;
  file_recent_menu#add items.file_recent_clear;
  ignore (items.file_recent_clear#connect#activate ~callback:editor#file_history_clear);
  (** Switch Implementation/Interface *)
  menu#add items.file_switch;
  ignore (items.file_switch#connect#activate ~callback:(fun () -> editor#with_current_page editor#switch_mli_ml));
  items.file_switch#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._I ~flags;
  (** Save *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let save_file = GMenu.image_menu_item ~label:"Save" ~packing:menu#add () in
  save_file#set_image (GMisc.image ~pixbuf:Icons.save_16 ())#coerce;
  save_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._s ~flags;
  ignore (save_file#connect#activate ~callback:begin fun () ->
    Gaux.may ~f:editor#save (editor#get_page `ACTIVE)
  end);
  (** Save as.. *)
  let save_as = GMenu.image_menu_item
    ~image:(GMisc.image ~pixbuf:Icons.save_as_16 ())
    ~label:"Save As..." ~packing:menu#add () in
  save_as#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._s ~flags;
  ignore (save_as#connect#activate ~callback:begin fun () ->
    Gaux.may ~f:editor#dialog_save_as (editor#get_page `ACTIVE)
  end);
  (** Save All *)
  let save_all = GMenu.image_menu_item ~label:"Save All" ~packing:menu#add () in
  save_all#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._a ~flags;
  ignore (save_all#connect#activate ~callback:browser#save_all);
  save_all#set_image (GMisc.image ~pixbuf:Icons.save_all_16 ())#coerce;
  (** Rename *)
  menu#add items.file_rename;
  ignore (items.file_rename#connect#activate ~callback:begin fun () ->
    editor#with_current_page begin fun current_page ->
      editor#dialog_rename current_page;
      browser#set_title browser#editor
    end
  end);
  (** Close current *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  menu#add (items.file_close :> GMenu.menu_item);
  items.file_close#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F4 ~flags;
  ignore (items.file_close#connect#activate ~callback:begin fun () ->
    editor#with_current_page (fun p -> ignore (editor#dialog_confirm_close p))
  end);
  (** Close all except current *)
  menu#add items.file_close_all;
  ignore (items.file_close_all#connect#activate ~callback:begin fun () ->
    editor#with_current_page (fun p -> editor#close_all ?except:(Some p) ())
  end);
  (** Revert current *)
  menu#add (items.file_revert :> GMenu.menu_item);
  ignore (items.file_revert#connect#activate ~callback:begin fun () ->
    editor#with_current_page editor#revert
  end);
  (** Delete current *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  menu#add (items.file_delete :> GMenu.menu_item);
  ignore (items.file_delete#connect#activate ~callback:editor#dialog_delete_current);
  (** Exit *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let quit = GMenu.image_menu_item ~label:"Exit" ~packing:menu#add () in
  quit#set_image (GMisc.image ~stock:`QUIT ~icon_size:`MENU ())#coerce;
  ignore (quit#connect#activate ~callback:(fun () -> browser#exit editor ()));
  (** callback *)
  ignore (file#misc#connect#state_changed ~callback:begin fun _ ->
    let has_current_page = editor#get_page `ACTIVE <> None in
    List.iter (fun i -> i#misc#set_sensitive has_current_page) [
      (items.file_switch :> GMenu.menu_item);
      (save_file :> GMenu.menu_item);
      (save_as :> GMenu.menu_item);
      (save_all :> GMenu.menu_item);
      (items.file_rename :> GMenu.menu_item);
      (items.file_close :> GMenu.menu_item);
      (items.file_close_all :> GMenu.menu_item);
      (items.file_revert :> GMenu.menu_item);
      (items.file_delete :> GMenu.menu_item);
    ];
    editor#with_current_page begin fun page ->
      let name = page#get_filename in
      items.file_switch#misc#set_sensitive (name ^^ ".ml" || name ^^ ".mli")
    end;
    let has_current_project = browser#current_project#get <> None in
    new_file#misc#set_sensitive has_current_project;
  end);
  file
;;

(** edit *)
let edit ~browser ~group ~flags
    ~get_menu_item_undo
    ~get_menu_item_redo items =
  let editor = browser#editor in
  let edit = GMenu.menu_item ~label:"Edit" () in
  let menu = GMenu.menu ~packing:edit#set_submenu () in
  (** Undo *)
  let undo = GMenu.image_menu_item ~label:"Undo" ~packing:menu#add () in
  undo#set_image (GMisc.image ~stock:`UNDO ~icon_size:`MENU ())#coerce;
  ignore (undo#connect#activate ~callback:(fun () -> editor#with_current_page (fun page -> page#undo())));
  undo#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._z ~flags;
  get_menu_item_undo := (fun () -> undo);
  (** Redo *)
  let redo = GMenu.image_menu_item ~label:"Redo" ~packing:menu#add () in
  redo#set_image (GMisc.image ~stock:`REDO ~icon_size:`MENU ())#coerce;
  ignore (redo#connect#activate ~callback:(fun () -> editor#with_current_page (fun page -> page#redo())));
  redo#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._z ~flags;
  get_menu_item_redo := (fun () -> redo);
  (** Select Word *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let select_word = GMenu.menu_item ~label:"Select Word" ~packing:menu#add () in
  ignore (select_word#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page ->
      ignore (page#ocaml_view#obuffer#select_ocaml_word ?pat:None ()))));
  select_word#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._w ~flags;
  (** Move to Matching Delimiter *)
  let move_par_expr = GMenu.menu_item ~label:"Move to Matching Delimiter" ~packing:menu#add () in
  ignore (move_par_expr#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page -> page#view#matching_delim_goto ?select:None ?strict:None ())));
  move_par_expr#add_accelerator ~group ~modi:[`CONTROL;] GdkKeysyms._d ~flags;
  (** Select to Matching Delimiter *)
  let select_par_expr = GMenu.menu_item ~label:"Select to Matching Delimiter" ~packing:menu#add () in
  ignore (select_par_expr#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page ->
      ignore (page#view#matching_delim_goto ?select:(Some true) ?strict:None ()))));
  select_par_expr#add_accelerator ~group ~modi:[`CONTROL;`SHIFT] GdkKeysyms._d ~flags;
  (** Comment/Uncomment *)
  let comment = GMenu.menu_item ~label:"Comment Block" ~packing:menu#add () in
  ignore (comment#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page -> ignore (page#ocaml_view#toggle_comment ()))));
  comment#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._q ~flags;
  (** Change To Uppercase/Lowercase *)
  let toggle_case = GMenu.menu_item ~label:"Convert To Uppercase/Lowercase" ~packing:menu#add () in
  ignore (toggle_case#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page -> page#buffer#toggle_case ())));
  toggle_case#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._u ~flags;
  (** Indent selection *)
  let indent_selection = GMenu.image_menu_item ~label:"Indent Selection" ~packing:menu#add () in
  indent_selection#set_image (GMisc.image ~stock:`INDENT ~icon_size:`MENU ())#coerce;
  ignore (indent_selection#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page -> page#buffer#indent ?decrease:(Some false) ())));
  indent_selection#add_accelerator ~group ~modi:[] GdkKeysyms._Tab ~flags;
  (** UnIndent selection *)
  let unindent_selection = GMenu.image_menu_item ~label:"Unindent Line/Selection" ~packing:menu#add () in
  unindent_selection#set_image (GMisc.image ~stock:`UNINDENT ~icon_size:`MENU ())#coerce;
  ignore (unindent_selection#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page -> page#buffer#indent ?decrease:(Some true) ())));
  unindent_selection#add_accelerator ~group ~modi:[`SHIFT] GdkKeysyms._Tab ~flags;
  (** Templates *)
  let templates = GMenu.menu_item ~label:"Templates..." ~packing:menu#add () in
  ignore (templates#connect#activate ~callback:(fun () ->
    browser#with_current_project (fun project ->
      editor#with_current_page (fun page -> Templ.popup project page#ocaml_view))));
  templates#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._j ~flags;
  (** Completion *)
  let complet = GMenu.menu_item ~label:"Completion" ~packing:menu#add () in
  ignore (complet#connect#activate ~callback:begin fun () ->
    browser#with_current_project begin fun project ->
      editor#with_current_page (fun page -> Mbrowser_compl.create ~project ~page ())
    end
  end);
  complet#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._space ~flags;
  (** Inferred Types *)
  let annot_type = GMenu.menu_item ~label:"Inferred Types" ~packing:menu#add () in
  let annot_type_menu = GMenu.menu ~packing:annot_type#set_submenu () in
  let annot_type_show = GMenu.menu_item ~label:"Show Type at Cursor" ~packing:annot_type_menu#add () in
  ignore (annot_type_show#connect#activate ~callback:browser#annot_type);
  annot_type_show#add_accelerator ~group ~modi:[] GdkKeysyms._F2 ~flags;
  let annot_type_copy = GMenu.menu_item ~label:"Copy Type at Cursor to Clipboard" ~packing:annot_type_menu#add () in
  ignore (annot_type_copy#connect#activate ~callback:browser#annot_type_copy);
  annot_type_copy#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F2 ~flags;
  let annot_type_tooltips = GMenu.check_menu_item
    ~label:"Enable On-Mouse-Hover"
    ~active:Preferences.preferences#get.Preferences.pref_annot_type_tooltips_enabled
    ~packing:annot_type_menu#add ()
  in
  ignore (annot_type_tooltips#connect#toggled ~callback:(fun () ->
    browser#annot_type_set_tooltips annot_type_tooltips#active));
  annot_type_tooltips#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._F2 ~flags;
  let _ = GMenu.separator_item ~packing:menu#add () in
  (** Eval in Toplevel *)
  let to_shell = GMenu.image_menu_item ~label:"Eval in Toplevel" ~packing:menu#add () in
  to_shell#set_image (GMisc.image ~stock:`EXECUTE ~icon_size:`MENU ())#coerce;
  ignore (to_shell#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page -> page#ocaml_view#obuffer#send_to_shell ())));
  to_shell#add_accelerator ~group ~modi:[] GdkKeysyms._F8 ~flags;
  let _ = GMenu.separator_item ~packing:menu#add () in
  (** Select All *)
  let select_all = GMenu.image_menu_item ~label:"Select All" ~packing:menu#add () in
  select_all#set_image (GMisc.image ~stock:`SELECT_ALL ~icon_size:`MENU ())#coerce;
  ignore (select_all#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page ->
      ignore(page#buffer#select_range page#buffer#start_iter page#buffer#end_iter))));
  select_all#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._a ~flags;
  (**  *)
  ignore (edit#misc#connect#state_changed ~callback:begin fun _ ->
    let has_current_page = editor#get_page `ACTIVE <> None in
    if not has_current_page then begin
      undo#misc#set_sensitive false;
      redo#misc#set_sensitive false;
    end;
    List.iter (fun i -> i#misc#set_sensitive has_current_page) [
      (select_word :> GMenu.menu_item);
      (move_par_expr :> GMenu.menu_item);
      (select_par_expr :> GMenu.menu_item);
      (comment :> GMenu.menu_item);
      (toggle_case :> GMenu.menu_item);
      (templates :> GMenu.menu_item);
      (complet :> GMenu.menu_item);
      (annot_type :> GMenu.menu_item);
      (to_shell :> GMenu.menu_item);
      (select_all :> GMenu.menu_item);
      (indent_selection :> GMenu.menu_item);
      (unindent_selection :> GMenu.menu_item);
    ];
    editor#with_current_page begin fun page ->
      let name = page#get_filename in
      let sensitive = name ^^ ".ml" || name ^^ ".mli" in
      List.iter (fun i -> i#misc#set_sensitive sensitive) [
        (comment :> GMenu.menu_item);
        (templates :> GMenu.menu_item);
        (complet :> GMenu.menu_item);
        (annot_type :> GMenu.menu_item);
        (to_shell :> GMenu.menu_item);
      ];
      List.iter (fun x -> x#misc#set_sensitive page#buffer#has_selection)
        [toggle_case#coerce; indent_selection#coerce];
      let has_tag_delim = page#view#current_matching_tag_bounds <> [] in
      move_par_expr#misc#set_sensitive has_tag_delim;
      select_par_expr#misc#set_sensitive has_tag_delim;
    end;
  end);
  edit
;;

(** search *)
let search ~browser ~group ~flags items =
  let editor = browser#editor in
  let search_item = GMenu.menu_item ~label:"Search" () in
  let menu = GMenu.menu ~packing:search_item#set_submenu () in
  (** Find and Replace *)
  let find_repl = GMenu.image_menu_item ~label:"Find and Replace" ~packing:menu#add () in
  find_repl#set_image (GMisc.image ~stock:`FIND_AND_REPLACE ~icon_size:`MENU ())#coerce;
  ignore (find_repl#connect#activate ~callback:(fun () ->
    browser#find_and_replace ?find_all:None ?search_word_at_cursor:None ()));
  find_repl#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._f ~flags;
  (** Find Next *)
  let find_next = GMenu.image_menu_item ~label:"Find Next" ~packing:menu#add () in
  ignore (find_next#connect#activate ~callback:browser#find_next);
  find_next#add_accelerator ~group GdkKeysyms._F3 ~flags;
  (** Find Previous *)
  let find_prev = GMenu.image_menu_item ~label:"Find Previous" ~packing:menu#add () in
  ignore (find_prev#connect#activate ~callback:begin fun () ->
    try
      editor#with_current_page (fun page ->
        Find_text_in_buffer.find Find_text.Backward
          ~view:page#view ~canceled:(fun () -> false))
    with Find_text.No_current_regexp -> ()
  end);
  find_prev#add_accelerator ~group ~modi:[`SHIFT] GdkKeysyms._F3 ~flags;
  (** Search Again *)
  let search_again = GMenu.image_menu_item ~label:"Search Again" ~packing:menu#add () in
  search_again#set_image (GMisc.image ~pixbuf:Icons.search_again_16 ())#coerce;
  ignore (search_again#connect#activate ~callback:browser#search_again);
  search_again#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F3 ~flags;
  (** Find All *)
  let find_all = GMenu.image_menu_item ~label:"Find All" ~packing:menu#add () in
  find_all#set_image (GMisc.image ~pixbuf:Icons.search_results_16 ())#coerce;
  ignore (find_all#connect#activate ~callback:(fun () ->
    browser#find_and_replace ?find_all:(Some true) ?search_word_at_cursor:(Some true) ()));
  find_all#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._f ~flags;
  (** Search Incremental *)
  let i_search = GMenu.image_menu_item ~label:"Search Incremental" ~packing:menu#add () in
  i_search#set_image (GMisc.image ~stock:`FIND ~icon_size:`MENU ())#coerce;
  ignore (i_search#connect#activate ~callback:editor#i_search);
  i_search#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._e ~flags;
  (** Find/Replace in Path *)
  let find_in_path = GMenu.image_menu_item ~label:"Find/Replace in Path" ~packing:menu#add () in
  ignore (find_in_path#connect#activate ~callback:begin fun () ->
    browser#with_current_project begin fun project ->
      let dialog, page = Find_text_dialog.create ~editor ~project () in
      let hbox = GPack.hbox ~spacing:3 () in
      let _icon = GMisc.image ~pixbuf:Icons.search_results_16 ~packing:hbox#pack () in
      let label = GMisc.label ~packing:hbox#pack () in
      ignore (page#connect#search_started ~callback:begin fun () ->
        if page#misc#parent = None then
          (ignore (Messages.vmessages#append_page ~label_widget:hbox#coerce page#as_page));
        label#set_text page#text_to_find;
        page#present ();
      end);
      ignore (page#connect#search_finished ~callback:begin fun () ->
        page#active#set false;
        page#present ();
      end);
      dialog#show();
    end
  end);
  find_in_path#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._p ~flags;
  (** Clear find/replace history *)
  let clear_find_history = GMenu.image_menu_item ~label:"Clear Find/Replace History" ~packing:menu#add () in
  ignore (clear_find_history#connect#activate ~callback:Find_text.clear_history);
  let _ = GMenu.separator_item ~packing:menu#add () in
  (** Go to Line *)
  let goto_line = GMenu.image_menu_item
    ~image:(GMisc.image ~stock:`JUMP_TO ~icon_size:`MENU ())
    ~label:"Go to Line..." ~packing:menu#add () in
  ignore (goto_line#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page -> page#ocaml_view#goto ())));
  goto_line#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._g ~flags;
  (** Find definition *)
  let find_definition = GMenu.image_menu_item
    ~label:"Find Definition" ~packing:menu#add () in
  ignore (find_definition#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page ->
      ignore (editor#scroll_to_definition (page#buffer#get_iter `INSERT)))));
  find_definition#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._Return ~flags;
  (** Find references *)
  let find_references = GMenu.image_menu_item
    ~label:"Find References" ~packing:menu#add () in
  ignore (find_references#connect#activate ~callback:begin
    Activity.wrap Activity.Annot begin fun () ->
      editor#with_current_page begin fun page ->
        editor#find_references (page#buffer#get_iter `INSERT)
      end
    end
  end);
  find_references#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._Return ~flags;
  ignore (search_item#misc#connect#state_changed ~callback:begin fun _ ->
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
  end);
  (** Find file *)
  let dialog_find_file = GMenu.menu_item ~label:"Find File..." ~packing:menu#add () in
  ignore (dialog_find_file#connect#activate ~callback:(fun () -> browser#dialog_find_file ?all:(Some true) ()));
  dialog_find_file#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._L ~flags;
  let _ = GMenu.separator_item ~packing:menu#add () in
  (** Bookmarks *)
  let bookmarks = GMenu.image_menu_item
    ~image:(GMisc.image ~pixbuf:Icons.bB ~icon_size:`MENU ())
    ~label:"Bookmarks" ~packing:menu#add () in
  let bookmark_menu = GMenu.menu ~packing:bookmarks#set_submenu () in
  let _  = GMenu.tearoff_item ~packing:bookmark_menu#add () in
  Gmisclib.Idle.add begin fun () ->
    List.iter2 begin fun num key ->
      let mi = GMenu.menu_item ~label:(sprintf "Goto Bookmark %d" num) ~packing:bookmark_menu#add () in
      ignore (mi#connect#activate ~callback:(fun () -> editor#bookmark_goto ~num));
      mi#add_accelerator ~group ~modi:[`CONTROL] key ~flags;
    end [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] [_1; _2; _3; _4; _5; _6; _7; _8; _9; _0];
    let _ = GMenu.separator_item ~packing:bookmark_menu#add () in
    List.iter2 begin fun num key ->
      let mi = GMenu.menu_item ~label:(sprintf "Bookmark %d" num) ~packing:bookmark_menu#add () in
      ignore (mi#connect#activate ~callback:(fun () -> editor#bookmark_create ~num ?where:None ?callback:None ()));
      mi#add_accelerator ~group ~modi:[`MOD1; `CONTROL] key ~flags;
    end [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] [_1; _2; _3; _4; _5; _6; _7; _8; _9; _0];
  end;
  (** Callback *)
  ignore (search_item#misc#connect#state_changed ~callback:begin fun _ ->
    let items = [
      (find_repl :> GMenu.menu_item);
      (find_next :> GMenu.menu_item);
      (find_prev :> GMenu.menu_item);
      (search_again :> GMenu.menu_item);
      (find_all :> GMenu.menu_item);
      (i_search :> GMenu.menu_item);
      (goto_line :> GMenu.menu_item);
    ] in
    let has_current_page = editor#get_page `ACTIVE <> None in
    List.iter (fun i -> i#misc#set_sensitive has_current_page) items;
  end);
  search_item
;;

(** view *)
let view ~browser ~group ~flags
    ~menu_item_view_menubar
    ~menu_item_view_toolbar
    ~menu_item_view_tabbar
    ~menu_item_view_outline
    ~menu_item_view_messages ~menu_item_view_hmessages items =
  let editor = browser#editor in
  let view = GMenu.menu_item ~label:"View" () in
  let menu = GMenu.menu ~packing:view#set_submenu () in
  (*begin
    let item = GMenu.check_menu_item ~label:"Menubar" ~active:browser#menubar_visible ~packing:menu#add () in
    item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._n ~flags;
    let sign = item#connect#activate ~callback:(fun () -> browser#set_menubar_visible (not browser#menubar_visible)) in
    menu_item_view_menubar := (item, sign) :: !menu_item_view_menubar;
  end;*)
  begin
    let item = GMenu.check_menu_item ~label:"Toolbar" ~active:browser#toolbar_visible ~packing:menu#add () in
    let sign = item#connect#activate ~callback:(fun () -> browser#set_toolbar_visible (not browser#toolbar_visible)) in
    menu_item_view_toolbar := (item, sign) :: !menu_item_view_toolbar;
  end;
  begin
    let item = GMenu.check_menu_item ~label:"Tabbar" ~active:editor#show_tabs ~packing:menu#add () in
    item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._b ~flags;
    let sign = item#connect#activate ~callback:(fun () -> browser#set_tabbar_visible (not editor#show_tabs)) in
    menu_item_view_tabbar := (item, sign) :: !menu_item_view_tabbar;
  end;
  begin
    let item = GMenu.check_menu_item ~label:"Structure" ~active:editor#show_outline ~packing:menu#add () in
    item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._z ~flags;
    let sign = item#connect#activate ~callback:(fun () -> browser#set_outline_visible (not editor#show_outline)) in
    menu_item_view_outline := (item, sign) :: !menu_item_view_outline;
  end;
  begin
    let item = GMenu.check_menu_item ~label:"Messages" ~active:Messages.vmessages#visible ~packing:menu#add () in
    item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._m ~flags;
    let sign = item#connect#activate ~callback:(fun () -> browser#set_vmessages_visible (not Messages.vmessages#visible)) in
    menu_item_view_messages := (item, sign) :: !menu_item_view_messages;
  end;
  begin
    let item = GMenu.check_menu_item ~label:"Messages (right pane)" ~active:Messages.hmessages#visible ~packing:menu#add () in
    let sign = item#connect#activate ~callback:(fun () -> browser#set_hmessages_visible (not Messages.hmessages#visible)) in
    menu_item_view_hmessages := (item, sign) :: !menu_item_view_hmessages;
  end;
  (** Workspaces *)
  let maximize = GMenu.image_menu_item ~label:"Workspaces" ~packing:menu#add () in
  maximize#set_image (GMisc.image ~stock:`FULLSCREEN ~icon_size:`MENU ())#coerce;
  let maximize_menu = GMenu.menu ~packing:maximize#set_submenu () in
  let maximize_1 = GMenu.image_menu_item ~label:"Workspace 1" ~packing:maximize_menu#add () in
  let maximize_2 = GMenu.image_menu_item ~label:"Workspace 2" ~packing:maximize_menu#add () in
  let maximize_0 = GMenu.image_menu_item ~label:"Reset Workspace" ~packing:maximize_menu#add () in
  let _ = maximize_1#connect#activate ~callback:(fun () -> browser#set_maximized_view `FIRST) in
  let _ = maximize_2#connect#activate ~callback:(fun () -> browser#set_maximized_view `SECOND) in
  let _ = maximize_0#connect#activate ~callback:(fun () -> browser#set_maximized_view `NONE) in
  maximize_1#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._comma ~flags;
  maximize_2#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._period ~flags;
  (** Remove All Messages *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let messages_remove = GMenu.menu_item ~label:"Remove All Messages" ~packing:menu#add () in
  ignore (messages_remove#connect#activate ~callback:(fun () -> ignore (Messages.vmessages#remove_all_tabs())));
  messages_remove#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._m ~flags;
  (** Code Folding *)
  let _ = GMenu.separator_item ~packing:menu#add () in
  let code_folding = GMenu.menu_item ~label:"Code Folding" ~packing:menu#add () in
  let code_folding_menu = GMenu.menu ~packing:code_folding#set_submenu () in
  let enable_code_folding = GMenu.check_menu_item ~label:"Enable Code Folding" ~packing:code_folding_menu#add () in
  ignore (enable_code_folding#connect#after#toggled ~callback:begin fun () ->
    editor#code_folding_enabled#set enable_code_folding#active;
    editor#with_current_page (fun page -> page#ocaml_view#code_folding#set_enabled enable_code_folding#active);
    Preferences.preferences#get.Preferences.pref_code_folding_enabled <- enable_code_folding#active;
    Preferences.save()
  end);
  let collapse_enclosing = GMenu.menu_item ~label:"Toggle Current Fold" ~packing:code_folding_menu#add () in
  ignore (collapse_enclosing#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page -> ignore (page#ocaml_view#code_folding#toggle_current_fold()))));
  collapse_enclosing#add_accelerator ~group ~modi:[`CONTROL;] GdkKeysyms._minus ~flags;
  (* Expand All folds *)
  let unfold_all = GMenu.menu_item ~label:"Expand All Folds" ~packing:code_folding_menu#add () in
  ignore (unfold_all#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page -> ignore (page#ocaml_view#code_folding#expand_all()))));
  (** Select in Structure Pane *)
  let select_in_outline = GMenu.image_menu_item
    ~image:(GMisc.image ~pixbuf:Icons.select_in_structure ~icon_size:`MENU ())
    ~label:"Select in Structure Pane" ~packing:menu#add () in
  ignore (select_in_outline#connect#activate ~callback:(fun () ->
    editor#with_current_page (fun page ->
      editor#set_show_outline true;
      Gaux.may page#outline ~f:(fun ol -> ol#select_from_buffer ?align:None (page#buffer#get_mark `INSERT)))));
  (** Show Whitespace Characters *)
  let show_whitespace_chars = GMenu.check_menu_item
    ~active:editor#show_whitespace_chars
    ~label:"Show Whitespace Characters" ~packing:menu#add () in
  let signal_show_whitespace_chars = show_whitespace_chars#connect#toggled
    ~callback:(Editor_menu.show_whitespace_characters_toggled ~editor) in
  (** Toggle Word-Wrap *)
  let toggle_word_wrap = GMenu.check_menu_item ~label:"Toggle Word-Wrap" ~packing:menu#add () in
  let signal_toggle_wrod_wrap = toggle_word_wrap#connect#toggled
    ~callback:(Editor_menu.toggle_word_wrap_toggled ~editor) in
  (** Callback *)
  ignore (view#misc#connect#state_changed ~callback:begin fun _ ->
    let page = editor#get_page `ACTIVE in
    let has_current_page = page <> None in
    let is_ml =
      match page with
        | None -> false
        | Some page ->
         let name = page#get_filename in
         name ^^ ".ml" || name ^^ ".mli"
    in
    code_folding#misc#set_sensitive is_ml;
    select_in_outline#misc#set_sensitive is_ml;
    enable_code_folding#set_active Preferences.preferences#get.Preferences.pref_code_folding_enabled;
    List.iter (fun x -> x#misc#set_sensitive enable_code_folding#active) [collapse_enclosing; unfold_all];
    show_whitespace_chars#misc#handler_block signal_show_whitespace_chars;
    show_whitespace_chars#set_active (editor#show_whitespace_chars);
    show_whitespace_chars#misc#handler_unblock signal_show_whitespace_chars;
    show_whitespace_chars#misc#set_sensitive has_current_page;
    toggle_word_wrap#misc#handler_block signal_toggle_wrod_wrap;
    toggle_word_wrap#set_active editor#word_wrap;
    toggle_word_wrap#misc#handler_unblock signal_toggle_wrod_wrap;
    toggle_word_wrap#misc#set_sensitive has_current_page;
  end);
  (*  *)
  view
;;

(** project *)
let project ~browser ~group ~flags items =
  let editor = browser#editor in
  let project = GMenu.menu_item ~label:"Project" () in
  let menu = items.project in
  let cursor = Gdk.Cursor.create `ARROW in
  ignore (menu#event#connect#expose ~callback:begin fun _ ->
    Gdk.Window.set_cursor menu#misc#window cursor;
    false
  end);
  project#set_submenu menu;
  (** Clean current *)
  let project_clean_current = GMenu.image_menu_item ~label:"Clean" ~packing:menu#add () in
  project_clean_current#set_image (Icons.create Icons.clear_build_16)#coerce;
  project_clean_current#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F9 ~flags;
  ignore (project_clean_current#connect#activate ~callback:begin fun () ->
    browser#with_current_project (fun _ ->
      browser#with_default_target (fun target ->
        ignore (Task_console.exec ~editor `CLEAN target)))
  end);
  (** Compile current *)
  let project_compile_only = GMenu.image_menu_item ~label:"Compile" ~packing:menu#add () in
  project_compile_only#set_image (Icons.create Icons.compile_all_16)#coerce;
  project_compile_only#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F10 ~flags;
  ignore (project_compile_only#connect#activate ~callback:begin fun () ->
    browser#with_current_project (fun _ ->
      browser#with_default_target (fun target ->
        if Preferences.preferences#get.Preferences.pref_editor_save_all_bef_comp then (browser#save_all());
        ignore (Task_console.exec ~editor `COMPILE_ONLY target)))
  end);
  (** Build current *)
  let project_build = GMenu.image_menu_item ~label:"Build" ~packing:menu#add () in
  project_build#set_image (Icons.create Icons.build_16)#coerce;
  ignore (project_build#connect#activate ~callback:begin fun () ->
    browser#with_current_project (fun _ ->
      browser#with_default_target (fun target ->
        if Preferences.preferences#get.Preferences.pref_editor_save_all_bef_comp then (browser#save_all());
        ignore (Task_console.exec ~editor `COMPILE target)))
  end);
  (** Run current *)
  let project_run = GMenu.image_menu_item ~label:"Run" ~packing:menu#add () in
  project_run#set_image (Icons.create Icons.start_16)#coerce;
  project_run#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F11 ~flags;
  ignore (project_run#connect#activate ~callback:begin fun () ->
    browser#with_current_project (fun project ->
      browser#with_default_runtime_config ~open_dialog:true (fun rc ->
        let bc = List.find (fun b -> b.Target.id = rc.Rconf.target_id) project.Prj.targets in
        ignore (Task_console.exec ~editor (`RCONF rc) bc)))
  end);
  (** Clean... *)
  let clean_item = GMenu.image_menu_item ~label:"Clean..." ~packing:menu#add () in
  let clean_menu = GMenu.menu ~packing:clean_item#set_submenu () in
  (** Build... *)
  let build_item = GMenu.image_menu_item ~label:"Build..." ~packing:(menu#insert ~pos:5) () in
  let build_menu = GMenu.menu ~packing:build_item#set_submenu () in
  (** Build with dependencies... *)
  let build_dep_item = GMenu.image_menu_item ~label:"Build with dependencies..." ~packing:(menu#insert ~pos:6) () in
  let build_dep_menu = GMenu.menu ~packing:build_dep_item#set_submenu () in
  (** Run... *)
  let run_item = GMenu.image_menu_item ~label:"Run..." ~packing:menu#add () in
  let run_menu = GMenu.menu ~packing:run_item#set_submenu () in
  (** Clean Project *)
  let project_clean = GMenu.image_menu_item ~label:"Clean Project" ~packing:menu#add () in
  project_clean#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._F9 ~flags;
  ignore (project_clean#connect#activate ~callback:begin fun () ->
    browser#with_current_project (fun project ->
      browser#with_default_target (fun target ->
        ignore (Task_console.exec ~editor `CLEANALL target);
        Project.clean_tmp project));
  end);
  let sep1 = GMenu.separator_item ~packing:menu#add () in
  (** Compile file *)
  let project_comp_file = GMenu.image_menu_item ~label:"Compile file" ~packing:menu#add () in
  project_comp_file#set_image (GMisc.image ~pixbuf:Icons.compile_file_16 ())#coerce;
  ignore (project_comp_file#connect#activate ~callback:begin fun () ->
    browser#editor#with_current_page (fun p -> p#compile_buffer ~commit:false ())
  end);
  let sep2 = GMenu.separator_item ~packing:menu#add () in
  (** Project Properties *)
  let dialog_project_properties = GMenu.image_menu_item ~label:"Properties" ~packing:menu#add () in
  dialog_project_properties#set_image (GMisc.image ~stock:`PROPERTIES ~icon_size:`MENU ())#coerce;
  ignore (dialog_project_properties#connect#activate ~callback:(fun () ->
    browser#dialog_project_properties ?page_num:(Some 0) ?show:(Some true) ()));
  dialog_project_properties#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._P ~flags;
  (** Targets *)
  let project_targets = GMenu.image_menu_item ~image:(Icons.create Icons.target_16)#coerce ~label:"Targets" ~packing:menu#add () in
  ignore (project_targets#connect#activate ~callback:(fun () ->
    browser#dialog_project_properties ?page_num:(Some 1) ?show:(Some true) ()));
  project_targets#add_accelerator ~group ~modi:[] GdkKeysyms._F12 ~flags;
  (** Generate build script *)
  let project_script = GMenu.image_menu_item ~label:"Generate Build Script" ~packing:menu#add () in
  ignore (project_script#connect#activate ~callback:(fun () ->
    browser#with_current_project begin fun project ->
      let dialog = Build_script_ui.window ~project () in
      Gaux.may (GWindow.toplevel editor) ~f:(fun w -> dialog#set_transient_for w#as_window);
    end));
  (** Project Refresh *)
  let project_refresh = GMenu.image_menu_item ~label:"Refresh" ~packing:menu#add () in
  project_refresh#set_image (GMisc.image ~stock:`REFRESH ~icon_size:`MENU ())#coerce;
  ignore (project_refresh#connect#activate ~callback:browser#refresh);
  (** Project Clear Cache *)
  let project_clear_cache = GMenu.image_menu_item ~label:"Clear Cache" ~packing:menu#add () in
  ignore (project_clear_cache#connect#activate ~callback:browser#clear_cache);
  (*  *)
  let sep3 = GMenu.separator_item ~packing:menu#add () in
  (** Callback *)
  ignore (project#misc#connect#state_changed ~callback:begin fun _ ->
    browser#with_default_target begin fun target ->
      kprintf (set_label project_clean_current) "Clean \xC2\xAB%s\xC2\xBB" target.Target.name;
      kprintf (set_label project_compile_only) "Compile \xC2\xAB%s\xC2\xBB" target.Target.name;
      kprintf (set_label project_build) "Build \xC2\xAB%s\xC2\xBB" target.Target.name;
    end;
    browser#with_default_runtime_config ~open_dialog:false begin fun rc ->
      kprintf (set_label project_run) "Run \xC2\xAB%s\xC2\xBB" rc.Rconf.name;
    end;
    editor#with_current_page begin fun page ->
      let name = Filename.basename page#get_filename in
      if name ^^ ".ml" || name ^^ ".mli" then begin
        kprintf (set_label project_comp_file) "Compile \xC2\xAB%s\xC2\xBB" name;
        project_comp_file#misc#set_sensitive true
      end else begin
        kprintf (set_label project_comp_file) "Compile";
        project_comp_file#misc#set_sensitive false
      end;
    end;
    (*  *)
    let has_current_project = browser#current_project#get <> None in
    project_clean_current#misc#set_sensitive has_current_project;
    project_clean_current#misc#set_property "sensitive" (`BOOL has_current_project);
    project_compile_only#misc#set_property "sensitive" (`BOOL has_current_project);
    project_build#misc#set_property "sensitive" (`BOOL has_current_project);
    project_run#misc#set_property "sensitive" (`BOOL has_current_project);
    clean_item#misc#set_property "sensitive" (`BOOL has_current_project);
    build_item#misc#set_property "sensitive" (`BOOL has_current_project);
    run_item#misc#set_property "sensitive" (`BOOL has_current_project);
    project_clean#misc#set_property "sensitive" (`BOOL has_current_project);
    project_refresh#misc#set_property "sensitive" (`BOOL has_current_project);
    project_targets#misc#set_property "sensitive" (`BOOL has_current_project);
    dialog_project_properties#misc#set_property "sensitive" (`BOOL has_current_project);
    project_comp_file#misc#set_property "sensitive" (`BOOL has_current_project);
    sep1#misc#set_property "sensitive" (`BOOL has_current_project);
    sep2#misc#set_property "sensitive" (`BOOL has_current_project);
    sep3#misc#set_property "visible" (`BOOL (browser#project_history.File_history.content <> []));
    (*  *)
    Gmisclib.Idle.add (fun () -> List.iter clean_menu#remove clean_menu#children);
    Gmisclib.Idle.add (fun () -> List.iter build_menu#remove build_menu#children);
    Gmisclib.Idle.add (fun () -> List.iter build_dep_menu#remove build_dep_menu#children);
    Gmisclib.Idle.add (fun () -> List.iter run_menu#remove run_menu#children);
    browser#with_current_project begin fun project ->
      let current_project_filename = Project.filename project in
      List.iter  begin fun (filename, item) ->
        items.project_history_signal_locked <- true;
        item#set_active (filename = current_project_filename);
        items.project_history_signal_locked <- false;
      end items.project_history;
      List.iter begin fun tg ->
        Gmisclib.Idle.add begin fun () ->
          let item = GMenu.menu_item ~label:tg.Target.name ~packing:clean_menu#add () in
          ignore (item#connect#activate ~callback:begin fun () ->
            ignore (Task_console.exec ~editor `CLEAN tg)
          end);
        end;
      end project.Prj.targets;
      Gmisclib.Idle.add begin fun () ->
        let item_all = GMenu.menu_item ~label:"All configurations" ~packing:build_menu#add () in
        let _ = item_all#connect#activate ~callback:(fun () -> browser#build_all project.Prj.targets) in
        item_all#add_accelerator ~group ~modi:[`CONTROL;`MOD1] GdkKeysyms._F10 ~flags;
        ignore (GMenu.separator_item ~packing:build_menu#add ());
      end;
      List.iter begin fun tg ->
        Gmisclib.Idle.add begin fun () ->
          let item = GMenu.menu_item ~label:tg.Target.name ~packing:build_menu#add () in
          ignore (item#connect#activate ~callback:begin fun () ->
            ignore (Task_console.exec ~editor `COMPILE tg)
          end);
          let item = GMenu.menu_item ~label:tg.Target.name ~packing:build_dep_menu#add () in
          ignore (item#connect#activate ~callback:begin fun () ->
            ignore (Task_console.exec ~editor ~with_deps:true `COMPILE tg)
          end);
        end
      end project.Prj.targets;
      List.iter begin fun rc ->
        Gmisclib.Idle.add begin fun () ->
          let item = GMenu.menu_item ~label:rc.Rconf.name ~packing:run_menu#add () in
          ignore (item#connect#activate ~callback:begin fun () ->
            try
              let bc = List.find (fun b -> b.Target.id = rc.Rconf.target_id) project.Prj.targets in
              ignore (Task_console.exec ~editor (`RCONF rc) bc)
            with Not_found -> ()
          end);
        end
      end project.Prj.executables;
    end
  end);
  project
;;

(** tools *)
let tools ~browser ~group ~flags items =
  let editor = browser#editor in
  let tools = GMenu.menu_item ~label:"Tools" () in
  let menu = GMenu.menu ~packing:tools#set_submenu () in
  let preferences = GMenu.image_menu_item ~label:"Preferences" ~packing:menu#add () in
  preferences#set_image (GMisc.image ~stock:`PREFERENCES ~icon_size:`MENU ())#coerce;
  ignore (preferences#connect#activate ~callback:(fun () -> ignore (Preferences_tool.create ~editor ())));
  let _ = GMenu.separator_item ~packing:menu#add () in
  let toplevel = GMenu.menu_item ~label:"OCaml Toplevel" ~packing:menu#add () in
  ignore (toplevel#connect#activate ~callback:browser#shell);
  let module_browser = GMenu.menu_item ~label:"Module Browser" ~packing:menu#add () in
  ignore (module_browser#connect#activate ~callback:(fun () ->
    browser#with_current_project (fun project ->
      Mbrowser_tool.append_to_messages ~project)));
  module_browser#add_accelerator ~group ~modi:[] GdkKeysyms._F7 ~flags;
  let dialog_external_tools = GMenu.menu_item ~label:"External Tools" ~packing:menu#add () in
  ignore (dialog_external_tools#connect#activate ~callback:browser#dialog_external_tools);
  let _ = GMenu.separator_item ~packing:menu#add () in
  let et_items = ref [] in
  let callback _ =
    browser#with_current_project begin fun project ->
      List.iter menu#remove !et_items;
      let _, _, translate_macro =
        External_tools.get_macros
          ~get_editor:(fun () -> editor)
          ~get_current_project:(fun () -> project) ()
      in
      et_items := External_tools.inject (External_tools.read()) menu translate_macro;
      toplevel#misc#set_sensitive true;
      module_browser#misc#set_sensitive true;
      dialog_external_tools#misc#set_sensitive true;
    end;
  in
  callback();
  ignore (tools#misc#connect#state_changed ~callback);
  tools
;;

(** window *)
let window ~browser ~group ~flags
    ~get_menu_item_nav_history_backward
    ~get_menu_item_nav_history_forward
    ~get_menu_item_nav_history_last
    items =
  let editor = browser#editor in
  let window = GMenu.menu_item ~label:"Window" () in
  let menu = items.window in
  window#set_submenu menu;
  (** window_switch *)
  let window_switch = GMenu.menu_item ~label:"Switch..." ~packing:menu#append () in
  ignore (window_switch#connect#activate ~callback:editor#dialog_file_select);
  window_switch#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._B ~flags;
  (*window_switch#misc#set_sensitive (List.length !items > 0);*)
  let _ = GMenu.separator_item ~packing:menu#add () in
  (** Navigation Backward *)
  let backward = GMenu.image_menu_item ~label:"Back" ~packing:menu#append () in
  let forward = GMenu.image_menu_item ~label:"Forward" ~packing:menu#append () in
  get_menu_item_nav_history_backward := (fun () -> backward);
  get_menu_item_nav_history_forward := (fun () -> forward);
  backward#set_image (GMisc.image ~pixbuf:Icons.go_back ~icon_size:`MENU ())#coerce;
  let backward_menu = GMenu.menu ~packing:backward#set_submenu () in
  let prev = GMenu.menu_item ~label:"Previous Location" ~packing:backward_menu#add () in
  prev#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._Left ~flags;
  ignore (prev#connect#activate ~callback:(fun () ->
    browser#goto_location `PREV; browser#set_menu_item_nav_history_sensitive()));
  let _ = GMenu.separator_item ~packing:backward_menu#add () in
  browser#create_menu_history `BACK ~menu:backward_menu;
  (** Navigation Forward *)
  forward#set_image (GMisc.image ~pixbuf:Icons.go_forward ~icon_size:`MENU ())#coerce;
  let forward_menu = GMenu.menu ~packing:forward#set_submenu () in
  let next = GMenu.menu_item ~label:"Next Location" ~packing:forward_menu#add () in
  next#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._Right ~flags;
  ignore (next#connect#activate ~callback:(fun () ->
    browser#goto_location `NEXT; browser#set_menu_item_nav_history_sensitive()));
  let _ = GMenu.separator_item ~packing:forward_menu#add () in
  browser#create_menu_history `FORWARD ~menu:forward_menu;
  (** Last Edit Location *)
  let last_edit_location = GMenu.image_menu_item ~label:"Last Edit Location" ~packing:menu#append () in
  last_edit_location#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._End ~flags;
  last_edit_location#set_image (GMisc.image ~pixbuf:Icons.goto_last ~icon_size:`MENU ())#coerce;
  ignore (last_edit_location#connect#activate ~callback:(fun () -> browser#goto_location `LAST));
  get_menu_item_nav_history_last := (fun () -> last_edit_location);
  (** Clear Location History *)
  let clear_location_history = GMenu.image_menu_item ~label:"Clear Location History" () in
  menu#append (clear_location_history :> GMenu.menu_item);
  ignore (clear_location_history#connect#activate ~callback:begin fun () ->
    Location_history.clear editor#location_history;
    browser#set_menu_item_nav_history_sensitive()
  end);
  browser#set_menu_item_nav_history_sensitive();
  (**  *)
  ignore (window#misc#connect#state_changed ~callback:begin fun _ ->
    window_switch#misc#set_sensitive (List.length items.window_pages > 0);
    clear_location_history#misc#set_sensitive (Location_history.length editor#location_history > 0);
    editor#with_current_page begin fun page ->
      let current_page_oid = page#misc#get_oid in
      items.window_signal_locked <- true;
      List.iter (fun (oid, item) -> item#set_active (oid = current_page_oid)) items.window_pages;
      items.window_signal_locked <- false;
    end;
  end);
  window
;;

(** help *)
let help ~browser ~group ~flags items =
  let editor = browser#editor in
  let help = GMenu.menu_item ~label:"Help" () in
  let menu = GMenu.menu ~packing:help#set_submenu () in
  let key_assist = GMenu.menu_item ~label:"Key Assist" ~packing:menu#add () in
  ignore (key_assist#connect#activate ~callback:Key_assist.window);
  let _ = GMenu.separator_item ~packing:menu#add () in
  let gc_compact = GMenu.menu_item ~label:"Force Garbage Collection" ~packing:menu#add () in
  ignore (gc_compact#connect#activate ~callback:Gc.compact);
  let clear_cache = GMenu.menu_item ~label:"Clear Editor Cache" ~packing:menu#add () in
  ignore (clear_cache#connect#activate ~callback:editor#clear_cache);
  if try int_of_string (List.assoc "debug" Common.application_param) >= 1 with Not_found -> false then begin
    let crono = GMenu.menu_item ~label:"Print Debug Info" ~packing:menu#add () in
    crono#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._apostrophe ~flags;
    ignore (crono#connect#activate ~callback:(Print_debug_info.print ~editor));
  end;
  let _ = GMenu.separator_item ~packing:menu#add () in
  let check_for_updates = GMenu.menu_item ~label:"Check for Updates..." ~packing:menu#add () in
  ignore (check_for_updates#connect#activate ~callback:(fun () -> browser#check_for_updates ?verbose:None ()));
  let license = GMenu.menu_item ~label:"License" ~packing:menu#add () in
  ignore (license#connect#activate ~callback:(fun () -> (License.window ())#present()));
  let about = GMenu.menu_item ~label:"About OCamlEditor..." ~packing:menu#add () in
  ignore (about#connect#activate ~callback:begin fun () ->
    let about = About.window browser#window#as_window ~name:Oe_config.title ~version:Oe_config.version() in
    about#present()
  end);
  help
;;

(** create *)
let create ~browser ~group
    ~get_menu_item_undo
    ~get_menu_item_redo
    ~get_menu_item_nav_history_backward
    ~get_menu_item_nav_history_forward
    ~get_menu_item_nav_history_last
    ~menu_item_view_menubar
    ~menu_item_view_toolbar
    ~menu_item_view_tabbar
    ~menu_item_view_outline
    ~menu_item_view_messages
    ~menu_item_view_hmessages
    () =
  let flags = [`VISIBLE; `LOCKED] in
  let items = {
    menu_items                    = [];
    file_rename                   = GMenu.menu_item ();
    file_recent_select            = GMenu.menu_item ~label:"Select File..." ();
    file_recent_clear             = GMenu.menu_item ~label:"Clear File History" ();
    file_recent_sep               = GMenu.separator_item ();
    file_switch                   = GMenu.menu_item ~label:"Switch to Implementation/Interface" ();
    file_close                    = GMenu.image_menu_item ~stock:`CLOSE ();
    file_close_all                = GMenu.menu_item ();
    file_revert                   = GMenu.image_menu_item ~stock:`REVERT_TO_SAVED ();
    file_delete                   = GMenu.image_menu_item ~stock:`DELETE ();
    window                        = GMenu.menu ();
    window_radio_group            = None;
    window_pages                  = [];
    window_signal_locked          = false;
    window_n_childs               = 1;
    project                       = GMenu.menu ();
    project_history               = [];
    project_history_signal_locked = false;
  } in
  items.menu_items <- List.rev [
    file ~browser ~group ~flags items;
    edit ~browser ~group ~flags
      ~get_menu_item_undo
      ~get_menu_item_redo
      items;
    search ~browser ~group ~flags items;
    view ~browser ~group ~flags
      ~menu_item_view_menubar
      ~menu_item_view_toolbar
      ~menu_item_view_tabbar
      ~menu_item_view_outline
      ~menu_item_view_messages
      ~menu_item_view_hmessages items;
    project ~browser ~group ~flags items;
    tools ~browser ~group ~flags items;
    window ~browser ~group ~flags
      ~get_menu_item_nav_history_backward
      ~get_menu_item_nav_history_forward
      ~get_menu_item_nav_history_last
      items;
    help ~browser ~group ~flags items;
  ];
  List.iter (fun i -> i#activate()) items.menu_items;
  items


