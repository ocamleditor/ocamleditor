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
open Preferences

(** find_replace *)
let find_replace
    ?(find_all=false)
    ?(find_in_buffer=true)
    ?(search_word_at_cursor=(Preferences.preferences#get.editor_search_word_at_cursor))
    editor =
  editor#with_current_page begin fun page ->
    let buffer = if find_in_buffer then Some page#view#buffer else None in
    let dialog, page = Find_text_dialog.create ?buffer ~editor
        ~project:editor#project ~find_all ~search_word_at_cursor ()
    in
    Gaux.may (GWindow.toplevel editor) ~f:(fun tl -> dialog#set_transient_for tl#as_window);
    let hbox = GPack.hbox ~spacing:1 () in
    let _ = GMisc.image ~pixbuf:(??? Icons.search_results_16) ~packing:hbox#pack () in
    let label = GMisc.label ~packing:hbox#pack () in
    ignore (page#connect#search_started ~callback:begin fun () ->
        try
          if page#misc#parent = None then
            (ignore (Messages.vmessages#append_page ~label_widget:hbox#coerce page#as_page));
          label#set_text page#text_to_find;
          page#set_title page#text_to_find;
          if page#icon = None then page#set_icon (Some (??? Icons.search_results_16));
          page#present ();
        with ex -> Printf.eprintf "File \"menu_search.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      end);
    ignore (page#connect#search_finished ~callback:begin fun () ->
        page#is_working#set false;
        page#present();
      end);
    if not find_all then (dialog#show())
  end

(** find_prev *)
let find_prev editor =
  try
    editor#with_current_page (fun page ->
        Find_text_in_buffer.find Find_text.Backward
          ~view:page#view ~canceled:(fun () -> false))
  with Find_text.No_current_regexp -> ()

(** find_next *)
let find_next editor =
  editor#with_current_page begin fun page ->
    try
      Find_text_in_buffer.find Find_text.Forward
        ~view:page#view ~canceled:(fun () -> false)
    with Find_text.No_current_regexp -> find_replace editor
  end

(** search_again *)
let search_again editor =
  try
    editor#with_current_page begin fun page ->
      Find_text_in_buffer.find Find_text.status.Find_text.direction
        ~view:page#view ~canceled:(fun () -> false)
    end
  with Find_text.No_current_regexp -> find_replace editor

(** set_has_definition *)
let set_has_definition editor item =
  editor#with_current_page begin fun page ->
    let def =
      Definition.find
        ~filename:page#get_filename
        ~buffer:(page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None ())
        ~iter: (page#buffer#get_iter `INSERT)
        (*|> Async.await*)
    in
    match def with
    | Merlin.Ok def -> item#misc#set_sensitive (def <> None);
    | Merlin.Failure _ -> ()
    | Merlin.Error _ -> ()
  end

(** set_has_references *)
let set_has_references editor item =
  editor#with_current_page begin fun page ->
    let project = editor#project in
    let filename = page#get_filename in
    let offset = (page#buffer#get_iter `INSERT)#offset in
    let has =
      match
        Binannot_ident.find_definition_and_references
          ~project
          ~filename
          ~offset
          ~compile_buffer:(fun () -> page#compile_buffer ?join:(Some true))  ()
      with
      | Some res -> res.Binannot_ident.bai_refs <> []
      | _ -> false
    in
    item#misc#set_sensitive has
  end

(** create_search_results_pane *)
let create_search_results_pane ~pixbuf ~editor ~page =
  let widget = new Search_results.widget ~editor () in
  widget#button_new_search#misc#set_sensitive false;
  let hbox = GPack.hbox ~spacing:1 () in
  let _ = GMisc.image ~pixbuf ~packing:hbox#pack () in
  let label = GMisc.label ~packing:hbox#pack () in
  let iter = page#buffer#get_iter `INSERT in
  let mark = page#buffer#create_mark ?name:None ?left_gravity:None iter in
  ignore (widget#misc#connect#destroy ~callback:(fun () -> page#buffer#delete_mark (`MARK mark)));
  ignore (widget#connect#after#search_started ~callback:begin fun () ->
      if widget#misc#parent = None then
        (ignore (Messages.vmessages#append_page ~label_widget:hbox#coerce widget#as_page));
      widget#present ();
    end);
  widget, mark, label

(** find_definition_references *)
let find_definition_references editor =
  editor#with_current_page begin fun page ->
    let widget, mark, label = create_search_results_pane ~pixbuf:(??? Icons.references) ~editor ~page in
    widget#connect#search_started ~callback:begin fun () ->
      let project = page#project in
      let filename = page#get_filename in
      let results =
        Binannot_ident.find_definition_and_references
          ~project
          ~filename
          ~offset:(page#buffer#get_iter (`MARK mark))#offset
          ~compile_buffer:(fun () -> page#compile_buffer ?join:(Some true))  ()
      in
      let def_name = ref "" in
      let results =
        let open Binannot_ident in
        let open Search_results in
        let open Location in
        match results with
        | None -> []
        | Some {bai_def; bai_refs} ->
            def_name := (match bai_refs with ident :: _ -> ident.ident_loc.txt | [] -> "");
            let results = (match bai_def with | Some x -> [x] | _ -> []) @ bai_refs in
            let results = List.map (fun ident -> ident.ident_fname, ident) results in
            let results = List.rev (Utils.ListExt.group_assoc results) in
            List.map begin fun (filename, idents) ->
              let real_filenames = ref [] in
              let locations =
                List.map begin fun ident ->
                  let pixbuf =
                    match ident.ident_kind with
                    | Def _ | Def_constr _ | Def_module _ ->
                        Some (??? Icons.edit)
                    (*Some (widget#misc#render_icon ~size:`MENU `EDIT)*)
                    | Int_ref _ | Ext_ref | Open _ -> None
                  in
                  let fn = ident.ident_loc.loc.loc_start.Lexing.pos_fname in
                  if not (List.mem fn !real_filenames) then (real_filenames := fn :: !real_filenames);
                  (pixbuf, ident.ident_loc)
                end (List.rev idents)
              in
              let real_filename = match !real_filenames with fn :: [] -> fn | _ -> assert false in
              let timestamp = (Unix.stat real_filename).Unix.st_mtime in
              {filename; real_filename; locations = Offset locations; timestamp}
            end results
      in
      widget#set_results results;
      label#set_text !def_name;
      widget#set_title !def_name;
      if widget#icon = None then widget#set_icon (Some (??? Icons.references));
      kprintf widget#label_message#set_label "References to identifier <tt>%s</tt>" (Glib.Markup.escape_text !def_name);
    end |> ignore;
    widget#start_search();
  end

let local_refs editor =
  match editor#get_page `ACTIVE with
  | Some page ->
      let iter = page#buffer#get_iter `INSERT in
      Merlin.occurrences ~identifier_at:(iter#line + 1, iter#line_offset)
        ~filename:page#get_filename
        ~scope:`Buffer
        ~buffer:(page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None ()) ()
      |> Async.start_with_continuation begin function
      | Merlin.Ok ranges ->
          let open Merlin_j in
          ranges
          |> List.iter begin fun range ->
            Printf.printf "%s %d:%d\n%!"
              (match range.file with Some x -> x | _ -> "LOCAL")
              range.start.line range.start.col;
          end
      (*GtkThread.async begin fun () ->
        editor#location_history_add ~page ~iter ~kind:`BROWSE ();
        (*editor#goto_location !file !ln !col*)
        end ()*)
      | Merlin.Failure _ | Merlin.Error _ -> ()
      end
  | _ -> ()

(** update_items_visibility *)
let update_items_visibility
    (*~label_find_used_components
      ~find_used_components*)
    ~find_definition
    ~find_references
    editor =
  Gmisclib.Idle.add ~prio:100 begin fun () ->
    (*set_has_used_components editor label_find_used_components find_used_components;*)
    set_has_definition editor find_definition;
    set_has_references editor find_references;
  end
;;

