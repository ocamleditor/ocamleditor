(*

  OCamlEditor
  Copyright (C) 2010-2013 Francesco Tovagliari

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

open Project
open Prj
open Miscellanea
open Tree



let filename = Filename.concat Oe_config.ocamleditor_user_home "externalTools.ocaml"

let write tools =
  let ochan = open_out_bin filename in
  lazy (output_value ochan tools) @$ (lazy (close_out ochan))

let rec read () =
  let ichan = open_in_gen [Open_binary; Open_creat] 0o777 filename in
  let tools = lazy (try input_value ichan with End_of_file -> []) @$ (lazy (close_in ichan)) in
  let tools = if tools = [] then begin
    let tools = (if Oe_config.is_win32 then [
      ("1000", "Console", "start \"$(project_name)\" cmd /K cd \"$(project_source)\"");
      ("1001", "Explorer", "explorer \"$(project_source)\"");
      ("1002", "Open with Notepad", "notepad \"$(current_filename)\"");
    ] else []) @ [
      ("2002", "OCamlBrowser", "ocamlbrowser -I \"$(project_source)\"")
    ] in
    write tools;
    read ()
  end else tools in
  tools

let inject tools menu translate_macro =
  List.map begin fun (id, label, cmd) ->
    let callback = begin fun () ->
      let cmd = translate_macro cmd in
      Printf.printf "%s\n%!" cmd;
      ignore (Thread.create (fun () -> ignore (Sys.command cmd)) ())
    end in
    let item = GMenu.menu_item ~label ~packing:menu#add () in
    ignore (item#connect#activate ~callback);
    item
  end tools

let get_macros ~get_editor ~get_current_project () =
  let project = get_current_project() in
    let editor : Editor.editor = get_editor () in
    let current_filename =
      match editor#get_page `ACTIVE with
        | None -> ""
        | Some page -> (match page#file with None -> "" | Some file -> file#path)
    in
    let macros = [
      "$(project_home)", project.root;
      "$(project_source)", (project.root // Project.src);
      "$(project_name)", project.name;
      "$(current_filename)", current_filename;
    ]in
    let translate_macro = replace_all ~regexp:false macros in
    let macros_help () = "Macros: " ^ (String.concat ", " (List.map fst macros)) in
    macros, macros_help, translate_macro

let create ~get_editor ~get_current_project () =
    let macros, macros_help, translate_macro = get_macros ~get_editor ~get_current_project () in
    (* Tools *)
    let tools = ref (read ()) in
    let window = GWindow.window ~type_hint:`DIALOG ~icon:Icons.oe ~width:640 ~height:400
      ~title:"Configure External Tools" ~position:`CENTER ~modal:true ~show:true () in
    let vbox = GPack.vbox ~spacing:8 ~border_width:5 ~packing:window#add () in
    (* Lista *)
    let cols = ["Id", Tree.STRING; "Name", Tree.STRING; "Command", Tree.STRING] in
    let frame = GBin.frame ~shadow_type:`IN ~packing:vbox#add () in
    let tool_list = Tree.create ~store:Tree.Store.list ~cols ~packing:frame#add () in
    let columns = Tree.columns tool_list in
    let model = Tree.model tool_list in
    (* Aggiungo elementi inziali. *)
    List.iter begin fun (id, name, cmd) ->
      ignore (Tree.append ~data:[String id; String name; String cmd] tool_list)
    end !tools;
    (Tree.view tool_list)#selection#set_mode `MULTIPLE;
    (* Modifica righe *)
    let values_getters = List.map
      begin fun col -> match fst col with
        | C_string (column, text_renderer) ->
          (* Renderer *)
          text_renderer#set_properties [`EDITABLE (column.GTree.index > 0)];
          ignore (text_renderer#connect#edited ~callback:begin fun path txt ->
            let row = model#get_iter path in
            model#set ~row ~column txt;
          end);
          let view_column = snd col in
          view_column#set_visible (column.GTree.index > 0);
          (* Funzione per estrarre gli elementi dalla lista *)
          (fun () -> Tree.values ~column tool_list)
        | _ -> assert false
      end columns
    in
    let _ = GMisc.label ~text:(macros_help ()) ~xalign:0.0 ~packing:(vbox#pack ~expand:false) () in
    (* Bottoni *)
    let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~spacing:8 ~packing:(vbox#pack ~expand:false) () in
    let butt_add = GButton.button ~stock:`ADD ~packing:bbox#add () in
    let butt_remove = GButton.button ~stock:`REMOVE ~packing:bbox#add () in
    let butt_ok = GButton.button ~stock:`OK ~packing:bbox#add () in
    let butt_cancel = GButton.button ~stock:`CANCEL ~packing:bbox#add () in
    bbox#set_child_secondary butt_add#coerce true;
    bbox#set_child_secondary butt_remove#coerce true;
    (* Add *)
    ignore (butt_add#connect#clicked ~callback:begin fun () ->
      let iter = Tree.append ~data:[String (string_of_float (Unix.gettimeofday ())); String "New tool"; String ""] tool_list in
      let view = Tree.view tool_list in
      let model = Tree.model tool_list in
      let path = model#get_path iter in
      let col_name = List.nth (Tree.columns tool_list) 1 in
      view#misc#grab_focus();
      view#set_cursor ~edit:true path (snd col_name)
    end);
    (* Conferma *)
    ignore (butt_ok#connect#clicked ~callback:begin fun () ->
      let values = List.map (fun f -> f ()) values_getters in
      let ids, names, commands =
        match List.map (List.nth values) [0; 1; 2] with
          | [ids; names; commands] -> ids, names, commands
          | _ -> assert false
      in
      tools := [];
      let i = ref 0 in
      List.iter begin fun id ->
        tools := (id, (List.nth names !i), (List.nth commands !i)) :: !tools;
        incr i;
      end ids;
      tools := List.rev !tools;
      write !tools;
      window#destroy()
    end);
    (* Eliminazione *)
    ignore (butt_remove#connect#clicked ~callback:begin fun () ->
      let view = Tree.view tool_list in
      let model = Tree.model tool_list in
      let paths = view#selection#get_selected_rows in
      Tree.remove ~paths tool_list;
      let path = List.hd paths in
      (try ignore(model#get_iter path) with Failure _ -> ignore (GTree.Path.prev path));
      view#selection#select_path path;
    end);
    ignore (butt_cancel#connect#clicked ~callback:window#destroy)









