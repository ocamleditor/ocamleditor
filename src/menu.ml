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
open GdkKeysyms
open Browser_types

let create
    group
    get_menu_item_undo
    get_menu_item_redo
    get_menu_item_nav_history_backward
    get_menu_item_nav_history_forward
    get_menu_item_nav_history_last
    menu_item_view_menubar
    menu_item_view_toolbar
    menu_item_view_tabbar
    menu_item_view_messages
    browser =
  let flags = [`VISIBLE; `LOCKED] in [
  (** File Menu *)
  begin fun () ->
    let file = GMenu.menu_item ~label:"File" () in
    let menu = GMenu.menu ~packing:file#set_submenu () in
    let new_project = GMenu.menu_item ~label:"New Project..." ~packing:menu#add () in
    new_project#connect#activate ~callback:browser#dialog_project_new;
    let new_file = GMenu.image_menu_item ~image:(GMisc.image ~stock:`NEW ~icon_size:`MENU ())
      ~label:"New File..." ~packing:menu#add () in
    new_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._n ~flags;
    new_file#connect#activate ~callback:browser#dialog_file_new;
    let _ = GMenu.separator_item ~packing:menu#add () in
    let project_open = GMenu.image_menu_item
      ~label:"Open Project..." ~packing:menu#add () in
    project_open#connect#activate ~callback:browser#dialog_project_open;
    project_open#add_accelerator ~group ~modi:[`CONTROL;`SHIFT] GdkKeysyms._o ~flags;
    let open_file = GMenu.image_menu_item ~image:(GMisc.image ~stock:`OPEN ~icon_size:`MENU ())
      ~label:"Open File..." ~packing:menu#add () in
    open_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._o ~flags;
    open_file#connect#activate ~callback:browser#editor#dialog_file_open;
    let save_file = GMenu.image_menu_item ~label:"Save" ~packing:menu#add () in
    save_file#set_image (GMisc.image ~pixbuf:Icons.save_16 ())#coerce;
    save_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._s ~flags;
    save_file#connect#activate ~callback:begin fun () ->
      Gaux.may ~f:browser#editor#save (browser#editor#get_page Editor_types.Current)
    end;
    let save_as = GMenu.image_menu_item
      ~image:(GMisc.image ~pixbuf:Icons.save_as_16 ())
      ~label:"Save As..." ~packing:menu#add () in
    save_as#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._s ~flags;
    ignore (save_as#connect#activate ~callback:begin fun () ->
      Gaux.may ~f:browser#editor#dialog_save_as (browser#editor#get_page Editor_types.Current)
    end);
    let save_all = GMenu.image_menu_item ~label:"Save All" ~packing:menu#add () in
    save_all#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._a ~flags;
    save_all#connect#activate ~callback:browser#save_all;
    save_all#set_image (GMisc.image ~pixbuf:Icons.save_all_16 ())#coerce;
    let item_close_current = ref None in
    let item_close_except = ref None in
    let item_sep = ref None in
    let item_delete_current = ref None in
    let item_revert_current = ref None in
    let item_rename_current = ref None in
    let item_switch = ref None in
    let item_recent = ref None in
    let item_sep2 = ref None in
    let item_sep3 = ref None in
    let callback _ =
      let may_current_page f = Gaux.may (browser#editor#get_page Editor_types.Current) ~f in
      try
        begin
          (match !item_close_current with Some item -> menu#remove item; item_close_current := None | None -> ());
          (match !item_close_except with Some item -> menu#remove item; item_close_except := None | None -> ());
          (match !item_sep with Some item -> menu#remove item; item_sep := None | None -> ());
          (match !item_delete_current with Some item -> menu#remove item; item_delete_current := None | None -> ());
          (match !item_rename_current with Some item -> menu#remove item; item_rename_current := None | None -> ());
          (match !item_revert_current with Some item -> menu#remove item; item_revert_current := None | None -> ());
          (match !item_switch with Some item -> menu#remove item; item_switch := None | None -> ());
          (match !item_recent with Some item -> menu#remove item; item_recent := None | None -> ());
          (match !item_sep2 with Some item -> menu#remove item; item_sep2 := None | None -> ());
          (match !item_sep3 with Some item -> menu#remove item; item_sep3 := None | None -> ());
          let filename =
            match browser#editor#get_page Editor_types.Current with
              | None -> ""
              | Some cp -> (match cp#file with Some file -> file#name | _ -> "")
          in
          (** Open recent *)
          let open_recent = GMenu.menu_item ~label:"Recent Files" ~packing:(menu#insert ~pos:5) () in
          let open_recent_menu = GMenu.menu ~packing:open_recent#set_submenu () in
          if List.length browser#editor#file_history.File_history.content > 0 then begin
            let dialog_find_file = GMenu.menu_item ~label:"Select File..."
              ~packing:open_recent_menu#add () in
            dialog_find_file#connect#activate ~callback:begin fun () ->
              browser#dialog_find_file ?all:(Some false) ()
            end;
            dialog_find_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._K ~flags;
          end;
          let count = ref 0 in
          begin
            try
              List.iter begin fun filename ->
                incr count;
                let label = filename in
                let mi = GMenu.menu_item ~label ~packing:open_recent_menu#add () in
                ignore (mi#connect#activate ~callback:(fun () ->
                  ignore (browser#editor#open_file ~active:true ~offset:0 filename)));
                if !count > 30 then (raise Exit)
              end (browser#editor#file_history.File_history.content);
            with Exit -> ()
          end;
          if List.length browser#editor#file_history.File_history.content > 0 then begin
            let clear_file_history = GMenu.menu_item ~label:"Clear File History"
              ~packing:open_recent_menu#add () in
            ignore (clear_file_history#connect#activate
              ~callback:(fun () -> File_history.clear browser#editor#file_history));
          end;
          item_recent := Some open_recent;
          (** Switch Implementation/Interface *)
          let label = sprintf "Switch to Implementation/Interface" in
          let switch = GMenu.menu_item ~label ~packing:(menu#insert ~pos:6) () in
          switch#connect#activate ~callback:(fun () -> may_current_page browser#editor#switch_mli_ml);
          switch#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._I ~flags;
          item_switch := Some switch;
          switch#misc#set_sensitive ((Filename.check_suffix filename ".ml")
            || (Filename.check_suffix filename ".mli"));
          let sep = GMenu.separator_item ~packing:(menu#insert ~pos:7) () in
          item_sep := Some sep;
          (** Rename *)
          let label = sprintf "Rename \xC2\xAB%s\xC2\xBB" filename in
          let rename_current = GMenu.menu_item ~label ~packing:(menu#insert ~pos:11) () in
          ignore (rename_current#connect#activate ~callback:begin fun () ->
            may_current_page begin fun current_page ->
              browser#editor#dialog_rename current_page;
              browser#set_title browser#editor
            end
          end);
          item_rename_current := Some rename_current;

          let sep = GMenu.separator_item ~packing:(menu#insert ~pos:12) () in
          item_sep2 := Some sep;
          (** Close *)
          let close_current = GMenu.image_menu_item ~image:(GMisc.image ~stock:`CLOSE ~icon_size:`MENU ())
            ~label:(sprintf "Close \xC2\xAB%s\xC2\xBB" filename) ~packing:(menu#insert ~pos:13) () in
          close_current#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F4 ~flags;
          ignore (close_current#connect#activate ~callback:begin fun () ->
            may_current_page begin fun current_page ->
              (browser#editor#dialog_confirm_close current_page);
            end
          end);
          item_close_current := Some (close_current :> GMenu.menu_item);
          (** Close All Except *)
          let label = sprintf "Close All Except \xC2\xAB%s\xC2\xBB" filename in
          let close_except = GMenu.menu_item ~label ~packing:(menu#insert ~pos:14) () in
          close_except#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._F4 ~flags;
          close_except#connect#activate ~callback:begin fun () ->
            may_current_page begin fun current_page ->
              browser#editor#close_all ?except:(Some current_page) ()
            end
          end;
          item_close_except := Some close_except;
          (** Revert *)
          let label = sprintf "Revert \xC2\xAB%s\xC2\xBB" filename in
          let revert_current = GMenu.image_menu_item
            ~image:(GMisc.image ~pixbuf:Icons.revert_to_saved_16 ()) ~label
            ~packing:(menu#insert ~pos:15) () in
          revert_current#connect#activate ~callback:begin fun () ->
            may_current_page browser#editor#revert
          end;
          item_revert_current := Some (revert_current :> GMenu.menu_item);
          let sep = GMenu.separator_item ~packing:(menu#insert ~pos:16) () in
          item_sep3 := Some sep;
          (** Delete *)
          let label = sprintf "Delete \xC2\xAB%s\xC2\xBB" filename in
          let delete_current = GMenu.image_menu_item
            ~image:(GMisc.image ~stock:`DELETE ~icon_size:`MENU ()) ~label
            ~packing:(menu#insert ~pos:17) () in
          delete_current#connect#activate ~callback:browser#editor#delete_current;
          item_delete_current := Some (delete_current :> GMenu.menu_item);
        end
      with _ -> ()
    in
    callback();
    file#connect#activate ~callback;
    (* Exit *)
    let _ = GMenu.separator_item ~packing:menu#add () in
    let quit = GMenu.image_menu_item ~label:"Exit" ~packing:menu#add () in
    quit#set_image (GMisc.image ~stock:`QUIT ~icon_size:`MENU ())#coerce;
    quit#connect#activate ~callback:(fun () -> browser#exit browser#editor ());
    (*  *)
    let callback _ =
      try
        ignore (browser#current_project);
        let has_current_page = (browser#editor#get_page Editor_types.Current) <> None in
        let force_sensitive c =
          List.mem c#misc#get_oid [
            new_project#misc#get_oid;
            project_open#misc#get_oid;
            new_file#misc#get_oid;
            open_file#misc#get_oid;
            quit#misc#get_oid;
          ]
        in
        List.iter (fun c -> c#misc#set_sensitive
          (has_current_page || force_sensitive c)) menu#children;
        (match !item_recent with
          | Some mi ->
            mi#misc#set_sensitive (List.length browser#editor#file_history.File_history.content > 0)
          | _ -> ())
      with No_current_project -> begin
        List.iter (fun c -> c#misc#set_sensitive false) menu#children;
        new_project#misc#set_sensitive true;
        project_open#misc#set_sensitive true;
        quit#misc#set_sensitive true;
      end
    in
    callback();
    file#connect#activate ~callback;
    file
  end;

  (** Edit Menu *)
  begin fun () ->
    let edit = GMenu.menu_item ~label:"Edit" () in
    let may f = match browser#editor#get_page Editor_types.Current with
      | None -> ()
      | Some current -> f current
    in
    let menu = GMenu.menu ~packing:edit#set_submenu () in
    (** Undo *)
    let undo = GMenu.image_menu_item ~label:"Undo" ~packing:menu#add () in
    undo#set_image (GMisc.image ~stock:`UNDO ~icon_size:`MENU ())#coerce;
    undo#connect#activate ~callback:(fun () -> may (fun page -> page#undo()));
    undo#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._z ~flags;
    get_menu_item_undo := (fun () -> undo);
    (** Redo *)
    let redo = GMenu.image_menu_item ~label:"Redo" ~packing:menu#add () in
    redo#set_image (GMisc.image ~stock:`REDO ~icon_size:`MENU ())#coerce;
    redo#connect#activate ~callback:(fun () -> may (fun page -> page#redo()));
    redo#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._z ~flags;
    get_menu_item_redo := (fun () -> redo);
    let _ = GMenu.separator_item ~packing:menu#add () in
    (** Select Word *)
    let select_word = GMenu.menu_item ~label:"Select Word" ~packing:menu#add () in
    select_word#connect#activate ~callback:(fun () ->
      may (fun x -> ignore (x#ocaml_view#obuffer#select_ocaml_word ?pat:None ())));
    select_word#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._w ~flags;
    (** Move to Matching Delimiter *)
    let move_par_expr = GMenu.menu_item ~label:"Move to Matching Delimiter" ~packing:menu#add () in
    move_par_expr#connect#activate ~callback:(fun () -> may (fun x -> x#view#matching_delim_goto ?select:None ?strict:None ()));
    move_par_expr#add_accelerator ~group ~modi:[`CONTROL;] GdkKeysyms._d ~flags;
    (** Select to Matching Delimiter *)
    let select_par_expr = GMenu.menu_item ~label:"Select to Matching Delimiter" ~packing:menu#add () in
    select_par_expr#connect#activate ~callback:(fun () ->
      may (fun x -> x#view#matching_delim_goto ?select:(Some true) ?strict:None ()));
    select_par_expr#add_accelerator ~group ~modi:[`CONTROL;`SHIFT] GdkKeysyms._d ~flags;
    (** Comment/Uncomment *)
    let comment = GMenu.menu_item ~label:"Comment Block" ~packing:menu#add () in
    comment#connect#activate ~callback:(fun () -> may (fun x -> x#ocaml_view#toggle_comment ()));
    comment#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._q ~flags;
    (** Change To Uppercase/Lowercase *)
    let toggle_case = GMenu.menu_item ~label:"Change To Uppercase/Lowercase" ~packing:menu#add () in
    toggle_case#connect#activate ~callback:(fun () -> may (fun x -> x#buffer#toggle_case ()));
    toggle_case#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._u ~flags;
    (** Templates *)
    let templates = GMenu.menu_item ~label:"Templates..." ~packing:menu#add () in
    templates#connect#activate ~callback:(fun () ->
      browser#editor#with_current_page (fun page -> Template.popup browser#current_project page#ocaml_view));
    templates#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._j ~flags;
    (** Completion *)
    let complet = GMenu.menu_item ~label:"Completion" ~packing:menu#add () in
    complet#connect#activate ~callback:(fun () -> may (fun x -> x#ocaml_view#completion ()));
    complet#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._space ~flags;
    (** Inferred Types *)
    let annot_type = GMenu.menu_item ~label:"Inferred Types" ~packing:menu#add () in
    let annot_type_menu = GMenu.menu ~packing:annot_type#set_submenu () in
    let annot_type_show = GMenu.menu_item ~label:"Show Type at Cursor" ~packing:annot_type_menu#add () in
    annot_type_show#connect#activate ~callback:browser#annot_type;
    annot_type_show#add_accelerator ~group ~modi:[] GdkKeysyms._F2 ~flags;
    let annot_type_copy = GMenu.menu_item ~label:"Copy Type at Cursor to Clipboard" ~packing:annot_type_menu#add () in
    annot_type_copy#connect#activate ~callback:browser#annot_type_copy;
    annot_type_copy#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F2 ~flags;
    let annot_type_tooltips = GMenu.check_menu_item
      ~label:"Enable On-Mouse-Hover"
      ~active:!Preferences.preferences.Preferences.pref_annot_type_tooltips_enabled
      ~packing:annot_type_menu#add ()
    in
    annot_type_tooltips#connect#toggled ~callback:(fun () -> browser#annot_type_set_tooltips annot_type_tooltips#active);
    annot_type_tooltips#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._F2 ~flags;
    let _ = GMenu.separator_item ~packing:menu#add () in
    (** Eval in Toplevel *)
    let to_shell = GMenu.image_menu_item ~label:"Eval in Toplevel" ~packing:menu#add () in
    to_shell#set_image (GMisc.image ~stock:`EXECUTE ~icon_size:`MENU ())#coerce;
    to_shell#connect#activate ~callback:(fun () ->
      may (fun x -> x#ocaml_view#obuffer#send_to_shell ()));
    to_shell#add_accelerator ~group ~modi:[] GdkKeysyms._F8 ~flags;
    let _ = GMenu.separator_item ~packing:menu#add () in
    (** Select All *)
    let select_all = GMenu.menu_item ~label:"Select All" ~packing:menu#add () in
    select_all#connect#activate ~callback:(fun () ->
      may (fun x -> ignore (x#buffer#select_range x#buffer#start_iter x#buffer#end_iter)));
    select_all#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._a ~flags;
    (*  *)
    let callback _ =
      try
        ignore (browser#current_project);
        let has_current_page = (browser#editor#get_page Editor_types.Current) <> None in
        let force_sensitive c = List.mem c#misc#get_oid [] in
        List.iter (fun c -> c#misc#set_sensitive
          (has_current_page || force_sensitive c)) menu#children;
        may begin fun page ->
          undo#misc#set_sensitive page#buffer#undo#can_undo;
          redo#misc#set_sensitive page#buffer#undo#can_redo;
          (*align_definitions#misc#set_sensitive page#buffer#has_selection;*)
        end;
      with No_current_project -> begin
        List.iter (fun c -> c#misc#set_sensitive false) menu#children
      end
    in
    callback();
    edit#connect#activate ~callback;
    edit
  end;

  (** Search Menu *)
  begin fun () ->
    let may f = match browser#editor#get_page Editor_types.Current with
      | None -> ()
      | Some current -> f current
    in
    let search_item = GMenu.menu_item ~label:"Search" () in
    let menu = GMenu.menu ~packing:search_item#set_submenu () in
    (** Find and Replace *)
    let find_repl = GMenu.image_menu_item ~label:"Find and Replace" ~packing:menu#add () in
    find_repl#set_image (GMisc.image ~stock:`FIND_AND_REPLACE ~icon_size:`MENU ())#coerce;
    find_repl#connect#activate ~callback:(fun () -> browser#find_and_replace ?find_all:None ?search_word_at_cursor:None ());
    find_repl#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._f ~flags;
    (** Find Next *)
    let find_next = GMenu.image_menu_item ~label:"Find Next" ~packing:menu#add () in
    find_next#connect#activate ~callback:browser#find_next;
    find_next#add_accelerator ~group GdkKeysyms._F3 ~flags;
    (** Find Previous *)
    let find_prev = GMenu.image_menu_item ~label:"Find Previous" ~packing:menu#add () in
    ignore (find_prev#connect#activate ~callback:begin fun () ->
      try
        may (fun page -> Find_text_in_buffer.find Find_text.Backward
          ~view:page#view ~canceled:(fun () -> false))
      with Find_text.No_current_regexp -> ()
    end);
    find_prev#add_accelerator ~group ~modi:[`SHIFT] GdkKeysyms._F3 ~flags;
    (** Search Again *)
    let search_again = GMenu.image_menu_item ~label:"Search Again" ~packing:menu#add () in
    search_again#set_image (GMisc.image ~pixbuf:Icons.search_again_16 ())#coerce;
    search_again#connect#activate ~callback:browser#search_again;
    search_again#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F3 ~flags;
    (** Find All *)
    let find_all = GMenu.image_menu_item ~label:"Find All" ~packing:menu#add () in
    find_all#set_image (GMisc.image ~pixbuf:Icons.search_results_16 ())#coerce;
    find_all#connect#activate
      ~callback:(fun () -> browser#find_and_replace ?find_all:(Some true) ?search_word_at_cursor:(Some true) ());
    find_all#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._f ~flags;
    (** Search Incremental *)
    let i_search = GMenu.image_menu_item ~label:"Search Incremental" ~packing:menu#add () in
    i_search#set_image (GMisc.image ~stock:`FIND ~icon_size:`MENU ())#coerce;
    i_search#connect#activate ~callback:browser#editor#i_search;
    i_search#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._e ~flags;

    (** Find/Replace in Path *)
    let find_in_path = GMenu.image_menu_item ~label:"Find/Replace in Path" ~packing:menu#add () in
    ignore (find_in_path#connect#activate ~callback:begin fun () ->
      let finish = ref (fun _ -> ()) in
      let dialog, res = Find_text_dialog.create ~editor:browser#editor ~project:browser#current_project () in
      let hbox = GPack.hbox ~spacing:3 () in
      let _icon = GMisc.image ~pixbuf:Icons.search_results_16 ~packing:hbox#pack () in
      let label = GMisc.label ~packing:hbox#pack () in
      res#connect#search_started ~callback:begin fun () ->
        if res#misc#parent = None then
          (finish := fst (Messages.messages#append_page "" ~label_widget:hbox#coerce res#coerce));
        label#set_text res#text_to_find;
        Messages.messages#present res#coerce;
      end;
      ignore (res#connect#search_finished ~callback:begin fun () ->
        !finish true;
        Messages.messages#present res#coerce;
      end);
      dialog#show();
    end);
    find_in_path#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._p ~flags;
    (** Clear find/replace history *)
    let clear_find_history = GMenu.image_menu_item ~label:"Clear Find/Replace History" ~packing:menu#add () in
    clear_find_history#connect#activate ~callback:Find_text.clear_history;
    (*clear_find_history#set_image (GMisc.image ~stock:`CLEAR ~icon_size:`MENU ())#coerce;*)
    let _ = GMenu.separator_item ~packing:menu#add () in
    (** Go to Line *)
    let goto_line = GMenu.image_menu_item
      ~image:(GMisc.image ~stock:`JUMP_TO ~icon_size:`MENU ())
      ~label:"Go to Line..." ~packing:menu#add () in
    goto_line#connect#activate ~callback:(fun () -> may (fun x -> x#ocaml_view#goto ()));
    goto_line#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._g ~flags;
    (** Find definition *)
    let find_definition = GMenu.image_menu_item
      (*~image:(GMisc.image ~stock:`JUMP_TO ~icon_size:`MENU ())*)
      ~label:"Find Definition" ~packing:menu#add () in
    find_definition#connect#activate ~callback:(fun () -> may (fun x ->
      ignore (browser#editor#scroll_to_definition (x#buffer#get_iter `INSERT))));
    find_definition#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._Return ~flags;
    (** Find references *)
    let find_references = GMenu.image_menu_item
      (*~image:(GMisc.image ~stock:`JUMP_TO ~icon_size:`MENU ())*)
      ~label:"Find References" ~packing:menu#add () in
    find_references#connect#activate ~callback:(fun () -> may (fun x -> ignore (browser#editor#find_references (x#buffer#get_iter `INSERT))));
    find_references#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._Return ~flags;
    (** Find file *)
    let dialog_find_file = GMenu.menu_item ~label:"Find File..." ~packing:menu#add () in
    dialog_find_file#connect#activate ~callback:(fun () -> browser#dialog_find_file ?all:None ());
    dialog_find_file#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._L ~flags;
    let _ = GMenu.separator_item ~packing:menu#add () in
    (** Bookmarks *)
    let bookmarks = GMenu.image_menu_item
      ~image:(GMisc.image ~pixbuf:Icons.bB ~icon_size:`MENU ())
      ~label:"Bookmarks" ~packing:menu#add () in
    let bookmark_menu = GMenu.menu ~packing:bookmarks#set_submenu () in
    let _  = GMenu.tearoff_item ~packing:bookmark_menu#add () in
    List.iter2 begin fun num key ->
      let mi = GMenu.menu_item ~label:(sprintf "Goto Bookmark %d" num) ~packing:bookmark_menu#add () in
      mi#connect#activate ~callback:(fun () -> browser#editor#bookmark_goto ~num);
      mi#add_accelerator ~group ~modi:[`CONTROL] key ~flags;
    end [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] [_1; _2; _3; _4; _5; _6; _7; _8; _9; _0];
    let _ = GMenu.separator_item ~packing:bookmark_menu#add () in
    List.iter2 begin fun num key ->
      let mi = GMenu.menu_item ~label:(sprintf "Bookmark %d" num) ~packing:bookmark_menu#add () in
      mi#connect#activate ~callback:(fun () -> browser#editor#bookmark_create num);
      mi#add_accelerator ~group ~modi:[`MOD1; `CONTROL] key ~flags;
    end [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] [_1; _2; _3; _4; _5; _6; _7; _8; _9; _0];
    (*  *)
    let callback _ =
      try
        ignore (browser#current_project);
        let has_current_page = (browser#editor#get_page Editor_types.Current) <> None in
        let force_sensitive c =
          List.mem c#misc#get_oid [dialog_find_file#misc#get_oid; find_in_path#misc#get_oid]
        in
        List.iter (fun c -> c#misc#set_sensitive
          (has_current_page || force_sensitive c)) menu#children;
        let def_ref_sensitive = match browser#editor#get_page Editor_types.Current with None -> false
            | Some page -> Definition.find_definition ~project:browser#editor#project ~page ~iter:(page#buffer#get_iter `INSERT) <> None
        in
        find_definition#misc#set_sensitive def_ref_sensitive;
        find_references#misc#set_sensitive def_ref_sensitive;
      with No_current_project -> begin
        List.iter (fun c -> c#misc#set_sensitive false) menu#children
      end
    in
    callback();
    search_item#connect#activate ~callback;
    find_definition#misc#set_sensitive true;
    find_references#misc#set_sensitive true;
    search_item
  end;

  (** View Menu *)
  begin fun () ->
    let may f = match browser#editor#get_page Editor_types.Current with
      | None -> ()
      | Some current -> f current
    in
    let view = GMenu.menu_item ~label:"View" () in
    let menu = GMenu.menu ~packing:view#set_submenu () in
    begin
      let item = GMenu.check_menu_item ~label:"Menubar" ~active:browser#menubar_visible ~packing:menu#add () in
      item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._n ~flags;
      let sign = item#connect#activate ~callback:(fun () -> browser#set_menubar_visible (not browser#menubar_visible)) in
      menu_item_view_menubar := (item, sign) :: !menu_item_view_menubar;
    end;
    begin
      let item = GMenu.check_menu_item ~label:"Toolbar" ~active:browser#toolbar_visible ~packing:menu#add () in
      let sign = item#connect#activate ~callback:(fun () -> browser#set_toolbar_visible (not browser#toolbar_visible)) in
      menu_item_view_toolbar := (item, sign) :: !menu_item_view_toolbar;
    end;
    begin
      let item = GMenu.check_menu_item ~label:"Tabbar" ~active:browser#editor#show_tabs ~packing:menu#add () in
      item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._b ~flags;
      let sign = item#connect#activate ~callback:(fun () -> browser#set_tabbar_visible (not browser#editor#show_tabs)) in
      menu_item_view_tabbar := (item, sign) :: !menu_item_view_tabbar;
    end;
    begin
      let item = GMenu.check_menu_item ~label:"Messages" ~active:browser#messages#visible ~packing:menu#add () in
      item#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._m ~flags;
      let sign = item#connect#activate ~callback:(fun () -> browser#set_messages_visible (not browser#messages#visible)) in
      menu_item_view_messages := (item, sign) :: !menu_item_view_messages;
    end;
    (** Workspaces *)
    let maximize = GMenu.image_menu_item ~label:"Workspaces" ~packing:menu#add () in
    maximize#set_image (GMisc.image ~stock:`FULLSCREEN ~icon_size:`MENU ())#coerce;
    let maximize_menu = GMenu.menu ~packing:maximize#set_submenu () in
    let maximize_1 = GMenu.image_menu_item ~label:"Workspace 1" ~packing:maximize_menu#add () in
    let maximize_2 = GMenu.image_menu_item ~label:"Workspace 2" ~packing:maximize_menu#add () in
    let maximize_0 = GMenu.image_menu_item ~label:"Reset Workspace" ~packing:maximize_menu#add () in
    let _ = maximize_1#connect#activate ~callback:(browser#set_maximized_view `FIRST) in
    let _ = maximize_2#connect#activate ~callback:(browser#set_maximized_view `SECOND) in
    let _ = maximize_0#connect#activate ~callback:(browser#set_maximized_view `NONE) in
    maximize_1#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._comma ~flags;
    maximize_2#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._period ~flags;
    (** Remove All Messages *)
    let _ = GMenu.separator_item ~packing:menu#add () in
    let messages_remove = GMenu.menu_item ~label:"Remove All Messages" ~packing:menu#add () in
    messages_remove#connect#activate ~callback:(fun () -> ignore (browser#messages#remove_all_tabs()));
    messages_remove#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._m ~flags;
    (** Code Folding *)
    let _ = GMenu.separator_item ~packing:menu#add () in
    let code_folding = GMenu.menu_item ~label:"Code Folding" ~packing:menu#add () in
    let code_folding_menu = GMenu.menu ~packing:code_folding#set_submenu () in
    let enable_code_folding = GMenu.check_menu_item ~label:"Enable Code Folding" ~packing:code_folding_menu#add () in
    ignore (enable_code_folding#connect#after#toggled ~callback:begin fun () ->
      browser#editor#code_folding_enabled#set enable_code_folding#active;
      may (fun x -> x#ocaml_view#code_folding#set_enabled enable_code_folding#active);
      !Preferences.preferences.Preferences.pref_code_folding_enabled <- enable_code_folding#active;
      Preferences.save()
    end);
    let collapse_enclosing = GMenu.menu_item ~label:"Toggle Current Fold" ~packing:code_folding_menu#add () in
    ignore (collapse_enclosing#connect#activate ~callback:(fun () ->
      may (fun x ->  ignore (x#ocaml_view#code_folding#toggle_current_fold()))));
    collapse_enclosing#add_accelerator ~group ~modi:[`CONTROL;] GdkKeysyms._minus ~flags;
    (* Expand All folds *)
    let unfold_all = GMenu.menu_item ~label:"Expand All Folds" ~packing:code_folding_menu#add () in
    ignore (unfold_all#connect#activate ~callback:(fun () ->
      may (fun x -> ignore (x#ocaml_view#code_folding#expand_all()))));
    (** Show Whitespace Characters *)
    let _ = GMenu.separator_item ~packing:menu#add () in
    let show_whitespace_chars = GMenu.check_menu_item ~active:browser#editor#show_whitespace_chars
      ~label:"Show Whitespace Characters" ~packing:menu#add () in
    let signal_show_whitespace_chars = show_whitespace_chars#connect#toggled ~callback:begin fun () ->
      let show = not (browser#editor#show_whitespace_chars) in
      browser#editor#set_show_whitespace_chars show;
      !Preferences.preferences.Preferences.pref_show_whitespace_chars <- show;
      Preferences.save();
    end in
    (** Toggle Word-Wrap *)
    let toggle_word_wrap = GMenu.check_menu_item ~active:browser#editor#word_wrap
      ~label:"Toggle Word-Wrap" ~packing:menu#add () in
    let signal_toggle_word_wrap = toggle_word_wrap#connect#toggled ~callback:begin fun () ->
      let wrap = not (browser#editor#word_wrap) in
      browser#editor#set_word_wrap wrap;
      !Preferences.preferences.Preferences.pref_editor_wrap <- wrap;
      Preferences.save();
    end in
    (** Callback *)
    let callback () =
      enable_code_folding#set_active !Preferences.preferences.Preferences.pref_code_folding_enabled;
      List.iter (fun x -> x#misc#set_sensitive enable_code_folding#active) [collapse_enclosing; unfold_all];
      show_whitespace_chars#misc#handler_block signal_show_whitespace_chars;
      show_whitespace_chars#set_active (browser#editor#show_whitespace_chars);
      show_whitespace_chars#misc#handler_unblock signal_show_whitespace_chars;
    in
    callback();
    view#connect#activate ~callback;
    (*  *)
    view
  end;

  (** Project Menu *)
  begin fun () ->
    let project = GMenu.menu_item ~label:"Project" () in
    let menu = GMenu.menu ~packing:project#set_submenu () in
    let item_clean = ref None in
    let item_compile = ref None in
    let item_compile_only = ref None in
    let item_run = ref None in
    let item_sep = ref None in
    let items_build = ref [] in
    let callback _ =
      try
        (match !item_clean with Some item -> menu#remove item; item_clean := None | None -> ());
        (match !item_compile_only with Some item -> menu#remove item; item_compile_only := None | None -> ());
        (match !item_compile with Some item -> menu#remove item; item_compile := None | None -> ());
        (match !item_run with Some item -> menu#remove item; item_run := None | None -> ());
        (match !item_sep with Some item -> menu#remove item; item_sep := None | None -> ());
        begin
          match browser#get_default_build_config () with None -> ()
          | Some default_bconf ->
            (** Clean current *)
            let title = sprintf "Clean \xC2\xAB%s\xC2\xBB" default_bconf.Bconf.name in
            let project_clean = GMenu.image_menu_item
              ~label:title ~packing:(menu#insert ~pos:0) () in
            project_clean#set_image (Icons.create Icons.clear_build_16)#coerce;
            ignore (project_clean#connect#activate ~callback:ignore);
            project_clean#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F9 ~flags;
            ignore (project_clean#connect#activate ~callback:begin fun () ->
              Gaux.may (browser#get_default_build_config ()) ~f:begin fun default_bconf ->
                ignore (Bconf_console.exec ~project:browser#current_project ~editor:browser#editor `CLEAN default_bconf)
              end
            end);
            item_clean := Some (project_clean :> GMenu.menu_item);
            (** Compile current *)
            let title = sprintf "Compile \xC2\xAB%s\xC2\xBB" default_bconf.Bconf.name in
            let project_compile_only = GMenu.image_menu_item
              ~label:title ~packing:(menu#insert ~pos:1) () in
            project_compile_only#set_image (Icons.create Icons.compile_all_16)#coerce;
            project_compile_only#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F10 ~flags;
            ignore (project_compile_only#connect#activate ~callback:begin fun () ->
              Gaux.may (browser#get_default_build_config ()) ~f:begin fun default_bconf ->
                if Oe_config.save_all_before_compiling then (browser#save_all());
                ignore (Bconf_console.exec ~project:browser#current_project ~editor:browser#editor `COMPILE_ONLY default_bconf)
              end
            end);
            item_compile_only := Some (project_compile_only :> GMenu.menu_item);
            (** Build current *)
            let title = sprintf "Build \xC2\xAB%s\xC2\xBB" default_bconf.Bconf.name in
            let project_compile = GMenu.image_menu_item
              ~label:title ~packing:(menu#insert ~pos:2) () in
            project_compile#set_image (Icons.create Icons.build_16)#coerce;
            (*project_compile#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F10 ~flags;*)
            ignore (project_compile#connect#activate ~callback:begin fun () ->
              Gaux.may (browser#get_default_build_config ()) ~f:begin fun default_bconf ->
                if Oe_config.save_all_before_compiling then (browser#save_all());
                ignore (Bconf_console.exec ~project:browser#current_project ~editor:browser#editor `COMPILE default_bconf)
              end
            end);
            item_compile := Some (project_compile :> GMenu.menu_item);
            (** Run current *)
            let title = sprintf "Run \xC2\xAB%s\xC2\xBB"
              (match browser#get_default_runtime_config () with Some x -> x.Rconf.name | _ -> "_none_") in
            let project_run = GMenu.image_menu_item
              ~label:title ~packing:(menu#insert ~pos:3) () in
            project_run#set_image (Icons.create Icons.start_16)#coerce;
            project_run#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._F11 ~flags;
            ignore (project_run#connect#activate ~callback:begin fun () ->
              Gaux.may (browser#get_default_runtime_config ()) ~f:begin fun rc ->
                let bc = List.find (fun b -> b.Bconf.id = rc.Rconf.id_build) browser#current_project.Project.build in
                ignore (Bconf_console.exec ~project:browser#current_project ~editor:browser#editor (`RCONF rc) bc)
              end
            end);
            item_run := Some (project_run :> GMenu.menu_item);
        end;
        (** Clean..., Build..., Run... *)
        List.iter menu#remove !items_build;
        items_build := [];
        let bconfigs = browser#current_project.Project.build in
        if List.length bconfigs > 1 then begin
          (*  *)
          let clean_conf_item = GMenu.image_menu_item ~label:"Clean..." ~packing:(menu#insert ~pos:4) () in
          items_build := (clean_conf_item :> GMenu.menu_item) :: !items_build;
          let clean_menu = GMenu.menu ~packing:clean_conf_item#set_submenu () in
          List.iter begin fun tg ->
            let item = GMenu.menu_item ~label:tg.Bconf.name ~packing:clean_menu#add () in
            ignore (item#connect#activate ~callback:begin fun () ->
              ignore (Bconf_console.exec ~project:browser#current_project ~editor:browser#editor `CLEAN tg)
            end);
          end bconfigs;
          (*  *)
          let compile_item = GMenu.image_menu_item ~label:"Build..." ~packing:(menu#insert ~pos:5) () in
          items_build := (compile_item :> GMenu.menu_item) :: !items_build;
          let compile_menu = GMenu.menu ~packing:compile_item#set_submenu () in
          let item_all = GMenu.menu_item ~label:"All configurations" ~packing:compile_menu#add () in
          let _ = item_all#connect#activate ~callback:(fun () -> browser#build_all bconfigs) in
          item_all#add_accelerator ~group ~modi:[`CONTROL;`MOD1] GdkKeysyms._F10 ~flags;
          ignore (GMenu.separator_item ~packing:compile_menu#add ());
          List.iter begin fun tg ->
            let item = GMenu.menu_item ~label:tg.Bconf.name ~packing:compile_menu#add () in
            ignore (item#connect#activate ~callback:begin fun () ->
              ignore (Bconf_console.exec ~project:browser#current_project ~editor:browser#editor `COMPILE tg)
            end);
          end bconfigs;
          (*  *)
          let run_item = GMenu.image_menu_item ~label:"Run..." ~packing:(menu#insert ~pos:6) () in
          items_build := (run_item :> GMenu.menu_item) :: !items_build;
          let run_menu = GMenu.menu ~packing:run_item#set_submenu () in
          List.iter begin fun rc ->
            let item = GMenu.menu_item ~label:rc.Rconf.name ~packing:run_menu#add () in
            ignore (item#connect#activate ~callback:begin fun () ->
              try
                let bc = List.find (fun b -> b.Bconf.id = rc.Rconf.id_build) bconfigs in
                ignore (Bconf_console.exec
                  ~project:browser#current_project ~editor:browser#editor (`RCONF rc) bc)
              with Not_found -> ()
            end);
          end browser#current_project.Project.runtime;
  (*              items_build := (GMenu.separator_item ~packing:(menu#insert ~pos:5) ()) :: !items_build;*)
        end;
        List.iter (fun c -> c#misc#set_sensitive true) menu#children
      with
        | Not_found -> ()
        | No_current_project -> begin
          List.iter (fun c -> c#misc#set_sensitive false) menu#children
        end
    in
    callback();
    (** Clean Project *)
    let title = sprintf "Clean Project" in
    let project_clean = GMenu.image_menu_item ~label:title ~packing:(menu#insert ~pos:0) () in
    project_clean#connect#activate ~callback:ignore;
    project_clean#add_accelerator ~group ~modi:[`CONTROL; `MOD1] GdkKeysyms._F9 ~flags;
    ignore (project_clean#connect#activate ~callback:begin fun () ->
      Gaux.may (browser#get_default_build_config ()) ~f:begin fun default_bconf ->
        ignore (Bconf_console.exec ~project:browser#current_project ~editor:browser#editor `CLEANALL default_bconf);
        Project.clean_tmp browser#current_project;
      end
    end);
    project#connect#activate ~callback;
    browser#connect#switch_project ~callback;
    (**  *)
    let _ = GMenu.separator_item ~packing:(menu#insert ~pos:6) () in
    (** Project Refresh *)
    let project_refresh = GMenu.image_menu_item ~label:"Refresh" ~packing:menu#add () in
    project_refresh#set_image (GMisc.image ~stock:`REFRESH ~icon_size:`MENU ())#coerce;
    project_refresh#connect#activate ~callback:browser#refresh;
    (** Build Configurations... *)
    let project_targets = GMenu.image_menu_item ~label:"Build Configurations..." ~packing:menu#add () in
    project_targets#connect#activate ~callback:(fun () -> browser#dialog_project_properties ?page:(Some 1) ());
    project_targets#add_accelerator ~group ~modi:[] GdkKeysyms._F12 ~flags;
    (** Project Properties *)
    let dialog_project_properties = GMenu.image_menu_item ~label:"Properties" ~packing:menu#add () in
    dialog_project_properties#set_image (GMisc.image ~stock:`PROPERTIES ~icon_size:`MENU ())#coerce;
    dialog_project_properties#connect#activate ~callback:(fun () -> browser#dialog_project_properties ?page:(Some 0) ());
    dialog_project_properties#add_accelerator ~group ~modi:[`CONTROL; `SHIFT] GdkKeysyms._P ~flags;
  (*        let generate_build_script = GMenu.menu_item ~label:"Generate a Build Script" ~packing:menu#add () in
    generate_build_script#connect#activate ~callback:begin fun () ->
      try
        browser#generate_build_script ~proj:browser#current_project
      with No_current_project -> ()
    end;*)
    (** Project history *)
    let items_project = ref [] in
    let callback _ =
      try
        List.iter menu#remove !items_project;
        items_project := [];
        let history = browser#project_history.File_history.content in
        let history = List.map (fun n -> (Filename.chop_extension (Filename.basename n), n)) history in
        let history = List.sort (fun (x1, _) (x2, _) ->
          compare (String.lowercase x1) (String.lowercase x2)) history in
        if List.length history > 0 then begin
          items_project := (GMenu.separator_item ~packing:menu#add ()) :: !items_project;
        end;
        List.iter begin fun (label, filename) ->
          if Sys.file_exists filename then begin
            let item = GMenu.check_menu_item
              ~active:(filename = Project.filename browser#current_project)
              ~label ~packing:menu#add () in
            items_project := (item :> GMenu.menu_item) :: !items_project;
            ignore (item#connect#toggled ~callback:(fun () -> browser#project_open' filename));
          end
        end history;
      with Not_found -> ()
    in
    callback();
    project#connect#activate ~callback;
    project
  end;

  (** Tools Menu *)
  begin fun () ->
    let tools = GMenu.menu_item ~label:"Tools" () in
    let menu = GMenu.menu ~packing:tools#set_submenu () in
    let preferences = GMenu.image_menu_item ~label:"Preferences" ~packing:menu#add () in
    preferences#set_image (GMisc.image ~stock:`PREFERENCES ~icon_size:`MENU ())#coerce;
    preferences#connect#activate ~callback:(fun () -> ignore (Preferences_tool.create ~editor:browser#editor ()));
    let _ = GMenu.separator_item ~packing:menu#add () in
    let toplevel = GMenu.menu_item ~label:"OCaml Toplevel" ~packing:menu#add () in
    toplevel#connect#activate ~callback:browser#shell;
    let dialog_external_tools = GMenu.menu_item ~label:"External Tools" ~packing:menu#add () in
    dialog_external_tools#connect#activate ~callback:(fun () -> browser#dialog_external_tools ~menu);
    let _ = GMenu.separator_item ~packing:menu#add () in
    let et_items = ref [] in
    let callback () =
      begin
        try
          List.iter menu#remove !et_items;
          let _, _, translate_macro =
            External_tools.get_macros
              ~get_editor:(fun () -> browser#editor)
              ~get_current_project:(fun () -> browser#current_project) ()
          in
          et_items := External_tools.inject (External_tools.read()) menu translate_macro;
          toplevel#misc#set_sensitive true;
          dialog_external_tools#misc#set_sensitive true;
        with No_current_project -> begin
          toplevel#misc#set_sensitive false;
          dialog_external_tools#misc#set_sensitive false;
        end;
      end;
    in
    callback();
    tools#connect#activate ~callback;
    tools
  end;

  (** Window Menu *)
  begin fun () ->
    let window = GMenu.menu_item ~label:"Window" () in
    let menu = GMenu.menu ~packing:window#set_submenu () in
    let items = ref [] in
    let callback () =
      List.iter menu#remove !items;
      items := [];
      let rgroup : (Gtk.radio_menu_item Gtk.group) option ref = ref None in
      let bindings = List.map begin fun p ->
        let file = File.create p#get_filename () in
        let is_current = match browser#editor#get_page Editor_types.Current with
          | None -> (fun _ -> false)
          | Some current -> (fun x -> x = current)
        in
        match browser#editor#get_page (Editor_types.File file) with
          | None -> (fun () -> ())
          | Some page ->
            let label = sprintf "%s%s" (if page#buffer#modified then "*" else "") file#name in
            let item = GMenu.radio_menu_item ?group:!rgroup ~active:(is_current page)
              ~label ~packing:menu#append () in
            rgroup := Some item#group;
            items := (item :> GMenu.menu_item) :: !items;
            fun () -> ignore (item#connect#activate
              ~callback:(fun () -> ignore (browser#editor#open_file ~active:true ~offset:0 file#path)));
      end browser#editor#pages in
      List.iter (fun f -> f()) bindings;
      (* Switch *)
      let select_file = GMenu.menu_item ~label:"Switch..." ~packing:menu#append () in
      select_file#connect#activate ~callback:browser#editor#dialog_file_select;
      select_file#add_accelerator ~group ~modi:[`CONTROL] GdkKeysyms._B ~flags;
      select_file#misc#set_sensitive (List.length !items > 0);
      (* Navigation Backward *)
      let sep = GMenu.separator_item ~packing:menu#add () in
      let backward = GMenu.image_menu_item ~label:"Back" ~packing:menu#append () in
      let forward = GMenu.image_menu_item ~label:"Forward" ~packing:menu#append () in
      get_menu_item_nav_history_backward := (fun () -> backward);
      get_menu_item_nav_history_forward := (fun () -> forward);
      backward#set_image (GMisc.image ~stock:`GO_BACK ~icon_size:`MENU ())#coerce;
      let backward_menu = GMenu.menu ~packing:backward#set_submenu () in
      let prev = GMenu.menu_item ~label:"Previous Location" ~packing:backward_menu#add () in
      prev#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._Left ~flags;
      ignore (prev#connect#activate ~callback:(fun () ->
        browser#goto_location `PREV; browser#set_menu_item_nav_history_sensitive()));
      let _ = GMenu.separator_item ~packing:backward_menu#add () in
      browser#create_menu_history `BACK ~menu:backward_menu;
      (* Navigation Forward *)
      forward#set_image (GMisc.image ~stock:`GO_FORWARD ~icon_size:`MENU ())#coerce;
      let forward_menu = GMenu.menu ~packing:forward#set_submenu () in
      let next = GMenu.menu_item ~label:"Next Location" ~packing:forward_menu#add () in
      next#add_accelerator ~group ~modi:[`MOD1] GdkKeysyms._Right ~flags;
      ignore (next#connect#activate ~callback:(fun () ->
        browser#goto_location `NEXT; browser#set_menu_item_nav_history_sensitive()));
      let _ = GMenu.separator_item ~packing:forward_menu#add () in
      browser#create_menu_history `FORWARD ~menu:forward_menu;
      (* Last Edit Location *)
      let last_edit_location = GMenu.image_menu_item ~label:"Last Edit Location" ~packing:menu#append () in
      last_edit_location#set_image (GMisc.image ~stock:`GOTO_LAST ~icon_size:`MENU ())#coerce;
      last_edit_location#connect#activate ~callback:(fun () -> browser#goto_location `LAST);
      get_menu_item_nav_history_last := (fun () -> last_edit_location);
      (* Clear Location History *)
      let clear_location_history = GMenu.image_menu_item ~label:"Clear Location History" ~packing:menu#append () in
      ignore (clear_location_history#connect#activate ~callback:begin fun () ->
        Location_history.clear browser#editor#location_history;
        browser#set_menu_item_nav_history_sensitive()
      end);
      (*  *)
      items := (select_file :> GMenu.menu_item) :: !items;
      items := (backward :> GMenu.menu_item) :: !items;
      items := (forward :> GMenu.menu_item) :: !items;
      items := (last_edit_location :> GMenu.menu_item) :: !items;
      items := (clear_location_history :> GMenu.menu_item) :: !items;
      items := (sep :> GMenu.menu_item) :: !items;
      browser#set_menu_item_nav_history_sensitive();
    in
    callback();
    window#connect#activate ~callback;
    window
  end;

  (** Help Menu *)
  begin fun () ->
    let help = GMenu.menu_item ~label:"?" () in
    let menu = GMenu.menu ~packing:help#set_submenu () in
    let check_for_updates = GMenu.menu_item ~label:"Check for Updates..." ~packing:menu#add () in
    check_for_updates#connect#activate ~callback:(fun () -> browser#check_for_updates ?verbose:None ());
    let gc_compact = GMenu.menu_item ~label:"Force Garbage Collection" ~packing:menu#add () in
    gc_compact#connect#activate ~callback:Gc.compact;
    let _ = GMenu.separator_item ~packing:menu#add () in
    let license = GMenu.menu_item ~label:"License" ~packing:menu#add () in
    ignore (license#connect#activate ~callback:begin fun () ->
      (License.window ())#present()
    end);
    let about = GMenu.menu_item
      ~label:"About OCamlEditor..." ~packing:menu#add () in
    ignore (about#connect#activate ~callback:begin fun () ->
      let about = About.window () in
      about#present()
    end);
(*    let crono = GMenu.menu_item ~label:"Print debug info" ~packing:menu#add () in
    crono#connect#activate ~callback:Prf.print;*)
    help
  end;
]

