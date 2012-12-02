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

(** find_definition_references *)
let find_definition_references editor =
  editor#with_current_page begin fun page ->
    let widget = new Search_results.widget ~editor () in
    widget#button_new_search#misc#set_sensitive false;
    let hbox = GPack.hbox ~spacing:3 () in
    let _ = GMisc.image ~pixbuf:Icons.search_results_16 ~packing:hbox#pack () in
    let label = GMisc.label ~packing:hbox#pack () in
    let iter = page#buffer#get_iter `INSERT in
    let mark = page#buffer#create_mark iter in
    ignore (widget#misc#connect#destroy ~callback:(fun () -> page#buffer#delete_mark (`MARK mark)));
    ignore (widget#connect#search_started ~callback:begin fun () ->
      let project = page#project in
      let filename = page#get_filename in
      let results =
        Binannot_ident.find_ident
          ~project
          ~filename
          ~offset:(page#buffer#get_iter (`MARK mark))#offset
          ~compile_buffer:(fun () -> page#compile_buffer ?join:(Some true))  ()
      in
      let def_name = ref "" in
      let results =
        let open Binannot in
        let open Binannot_ident in
        let open Search_results in
        match results with
          | None -> []
          | Some {bai_def; bai_refs} ->
            def_name := (match bai_refs with ident :: _ -> ident.ident_name | [] -> "");
            let results = (match bai_def with | Some x -> [x] | _ -> []) @ bai_refs in
            let results = List.map (fun ident -> ident.ident_fname, ident) results in
            let results = List.rev (Miscellanea.Xlist.group_assoc results) in
            List.map begin fun (filename, idents) ->
              let real_filenames = ref [] in
              let locations =
                List.map begin fun ident ->
                  let pixbuf =
                    match ident.ident_kind with
                      | Def _ -> Some (widget#misc#render_icon ~size:`MENU `EDIT)
                      | Int_ref _ | Ext_ref -> None
                  in
                  let fn = ident.ident_loc.Location.loc_start.Lexing.pos_fname in
                  if not (List.mem fn !real_filenames) then (real_filenames := fn :: !real_filenames);
                  pixbuf, ident.ident_loc
                end (List.rev idents)
              in
              let real_filename = match !real_filenames with fn :: [] -> fn | _ -> assert false in
              let timestamp = (Unix.stat real_filename).Unix.st_mtime in
              {filename; real_filename; locations; timestamp}
            end results
      in
      widget#set_results results;
      (*  *)
      if widget#misc#parent = None then
        (ignore (Messages.vmessages#append_page ~label_widget:hbox#coerce widget#as_page));
      label#set_text !def_name;
      kprintf widget#label_message#set_label "References to identifier <tt>%s</tt>" (Glib.Markup.escape_text !def_name);
      widget#present ();
    end);
    widget#start_search();
  end
;;

(** has_definition *)
let has_definition editor item =
  editor#with_current_page begin fun page ->
    let project = editor#project in
    let filename = page#get_filename in
    let offset = (page#buffer#get_iter `INSERT)#offset in
    let def =
      Binannot_ident.find_definition
        ~project
        ~filename
        ~offset
        ~compile_buffer:(fun () -> page#compile_buffer ?join:(Some true))  ()
    in
    item#misc#set_sensitive (def <> None);
  end
;;

(** has_references *)
let has_references editor item =
  editor#with_current_page begin fun page ->
    let project = editor#project in
    let filename = page#get_filename in
    let offset = (page#buffer#get_iter `INSERT)#offset in
    let has =
      match
        Binannot_ident.find_ident
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
;;

