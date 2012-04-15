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
open Miscellanea

(** Class OCaml Shell *)
class ocaml_shell ?project () =
  let vbox = GPack.vbox () in
  let hbox = GPack.hbox ~packing:vbox#add () in
  let toolbar = GButton.toolbar ~tooltips:true ~orientation:`VERTICAL ~style:`ICONS ~packing:hbox#pack () in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add () in
  let _ = toolbar#set_icon_size `MENU in
(*  let b_prev = GButton.tool_button ~stock:`GO_UP ~packing:toolbar#insert () in
  let b_next = GButton.tool_button ~stock:`GO_DOWN ~packing:toolbar#insert () in
  let b_send = GButton.tool_button ~stock:`OK ~packing:toolbar#insert () in
  let _ = GButton.separator_tool_item ~packing:toolbar#insert () in*)
  let b_use = GButton.tool_button ~packing:toolbar#insert () in
  let _ = b_use#set_icon_widget (Icons.create Icons.file_ml)#coerce in
  let b_load = GButton.tool_button ~packing:toolbar#insert () in
  let _ = b_load#set_icon_widget (Icons.create Icons.file_cm)#coerce in
  let b_directory = GButton.tool_button ~packing:toolbar#insert () in
  let _ = b_directory#set_icon_widget (Icons.create Icons.dir)#coerce in
  let b_load_path = GButton.tool_button ~packing:toolbar#insert () in
  let _ = b_load_path#set_icon_widget (Icons.create Icons.load_proj)#coerce in
  let _ = GButton.separator_tool_item ~packing:toolbar#insert () in
  let b_kill = GButton.tool_button ~stock:`STOP ~packing:toolbar#insert () in
  (*  *)
  let tooltips = GData.tooltips () in
(*  let _ = tooltips#set_tip ~text:"Previous phrase (Ctrl+Up)" b_prev#coerce in
  let _ = tooltips#set_tip ~text:"Next phrase (Ctrl+Down)" b_next#coerce in
  let _ = tooltips#set_tip ~text:"Send phrase (Return)" b_send#coerce in*)
  let _ = tooltips#set_tip ~text:"Use file..." b_use#coerce in
  let _ = tooltips#set_tip ~text:"Load bytecode..." b_load#coerce in
  let _ = tooltips#set_tip ~text:"Import directory..." b_directory#coerce in
  let _ = tooltips#set_tip ~text:"Load project path" b_load_path#coerce in
  let _ = tooltips#set_tip ~text:"Kill process" b_kill#coerce in
  (*  *)
  let prog = Ocaml_config.ocaml() in
  let sh = new Shell.ocaml ~prog ~env:(Unix.environment()) ~args:[] ~packing:sw#add () in
  let _ = Ocaml_text.shells := sh :: !Ocaml_text.shells in
object (self)
  inherit GObj.widget vbox#as_widget as super
  inherit Messages.page

  method parent_changed m =
    toolbar#misc#hide();
    if m = Messages.vmessages then begin
      toolbar#set_orientation `VERTICAL;
      toolbar#misc#reparent hbox#coerce;
      hbox#set_child_packing ~expand:false ~fill:false toolbar#coerce;
      hbox#reorder_child ~pos:0 toolbar#coerce;
    end else begin
      toolbar#set_orientation `HORIZONTAL;
      toolbar#misc#reparent vbox#coerce;
      vbox#set_child_packing ~expand:false ~fill:false toolbar#coerce;
    end;
    hbox#set_child_packing ~expand:true ~fill:true sw#coerce;
    toolbar#misc#show();

  method textview = sh#textview
  method label = "OCaml Toplevel"
  method alive = sh#alive
  method quit () =
    Ocaml_text.shells := List.filter ((<>) sh) !Ocaml_text.shells;
    sh#quit ()
  initializer
    let path = match project with None -> assert false
      | Some p -> (p.Project.root // Project.src) in
    let filechooser ~title ~filters phrase () =
      let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~width:600 ~height:600
        ~title:"Select project..." ~position:`CENTER ~show:false () in
      List.iter begin fun (name, patterns) ->
        dialog#add_filter (GFile.filter ~name ~patterns ())
      end filters;
      dialog#add_select_button_stock `OK `OK;
      dialog#add_button_stock `CANCEL `CANCEL;
      dialog#set_current_folder path;
      dialog#set_select_multiple true;
      match dialog#run () with
        | `OK ->
          List.iter begin fun filename ->
            sh#insert (phrase filename);
            sh#return();
          end dialog#get_filenames;
          dialog#destroy()
        | _ -> dialog#destroy()
    in
(*    ignore (b_prev#connect#clicked ~callback:(fun () -> sh#history `previous));
    ignore (b_next#connect#clicked ~callback:(fun () -> sh#history `next));
    ignore (b_send#connect#clicked ~callback:(fun () -> sh#return()));*)
    ignore (b_use#connect#clicked ~callback:begin
      filechooser ~title:"Use file..." ~filters:["*.ml", ["*.ml"]] begin fun filename ->
        sh#delete_input_line();
        sprintf "#use %S;;\n" filename
      end
    end);
    ignore (b_load#connect#clicked ~callback:begin
      filechooser ~title:"Load bytecode..." ~filters:["*.cm[oa]", ["*.cm[oa]"]] begin fun filename ->
        sh#delete_input_line();
        sprintf "#load %S;;\n"filename;
      end
    end);
    ignore (b_directory#connect#clicked ~callback:begin
      filechooser ~title:"Import directory..." ~filters:["", [""]] begin fun filename ->
        sh#delete_input_line();
        sprintf "#directory %S;;\n" filename
      end
    end);
    ignore (b_load_path#connect#clicked ~callback:begin
      match project with None -> fun () -> ()
        | Some p -> begin fun () ->
          sh#delete_input_line();
          List.iter begin fun dir ->
            kprintf sh#insert "#directory %S;;\n" dir;
          end (Project.get_load_path p);
          List.iter begin fun lib ->
            if not (Filename.check_suffix lib ".o") then begin
              kprintf sh#insert "#load \"%s.cma\";;\n" (String.escaped lib);
            end
          end (Project.get_libraries p);
          sh#return()
      end
    end);
    ignore (b_kill#connect#clicked ~callback:begin fun () ->
      self#quit();
      List.iter (fun x -> x#misc#set_sensitive false) toolbar#children;
    end)
end

let append_page ?project (messages : Messages.messages) =
  let sh = new ocaml_shell ?project () in
  let label_widget =
    let hbox = GPack.hbox () in
    let ebox = GBin.event_box ~packing:hbox#add () in
    ebox#misc#set_property "visible-window" (`BOOL false);
    let label = GMisc.label ~text:sh#label ~packing:ebox#add ~show:true () in
    let entry = GEdit.entry ~packing:hbox#add ~show:false () in
    let is_label = ref true in
    let toggle _ =
      if !is_label then begin
        entry#set_text label#text;
        entry#set_width_chars (String.length label#text + 5);
        ebox#misc#hide();
        entry#misc#show();
        entry#misc#grab_focus()
      end else begin
        label#set_text entry#text;
        entry#misc#hide();
        ebox#misc#show();
      end;
      is_label := not !is_label;
      true;
    in
    entry#event#connect#key_press ~callback:begin fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._Return then (toggle())
      else false
    end;
    entry#event#connect#focus_out ~callback:(fun _ -> ignore (toggle()); false);
    ebox#event#connect#button_press ~callback:begin let time = ref 1l in fun ev ->
      let t = GdkEvent.Button.time ev in
      if !time = 0l then (time := t);
      let dif = Int32.sub t !time in
      if dif > 0l && dif <= 500l then (time := 0l)
      else begin
        if dif > 0l then (time := t) else (ignore (toggle()));
      end;
      false
    end;
    hbox#coerce
  in
  messages#append_page ~label_widget sh#as_page;
  sh#present ();
  sh#misc#connect#destroy ~callback:sh#quit;
  sh#active#set false;
  let ask ~cancel () = Dialog.process_still_active ~name:sh#label ~ok:sh#quit ~cancel () in
  sh#set_close_tab_func (fun () -> if sh#alive then (sh#quit()));
  ignore (messages#connect#remove_page ~callback:begin fun child ->
    if sh#alive && child#misc#get_oid = sh#misc#get_oid then
      (ask ~cancel:(fun () -> raise Messages.Cancel_process_termination)())
  end);;
