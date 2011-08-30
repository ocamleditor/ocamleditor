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

open GUtil
open GdkKeysyms
open Project
open Preferences
open Printf

exception Cancel_process_termination

let (//) = Filename.concat

class messages ~(paned : GPack.paned) () =
  let nb = GPack.notebook ~scrollable:true ~tab_border:0 () in
  let remove_page = new remove_page () in
  let visible_changed = new visible_changed () in
object (self)
  val mutable visible = true;
  val mutable memo = []
  method visible = visible
  method empty = List.length (nb#children) = 0

  method set_visible x =
    if x <> visible then begin
      if x then (paned#child2#misc#show ()) else (paned#child2#misc#hide ());
      visible <- not visible;
    end;
    visible_changed#call visible;

  method add_ocaml_shell ?project () =
    let sh = new ocaml_shell ?project () in
    let label_widget =
      let hbox = GPack.hbox () in
      let ebox = GBin.event_box ~packing:hbox#add () in
      Gmisclib.Util.set_ebox_invisible ebox;
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
    let finish, button_close = self#append_page sh#label ~label_widget sh#coerce in
    self#present sh#coerce;
    sh#misc#connect#destroy ~callback:sh#quit;
(*    sh#textview#misc#grab_focus();*)
    finish true;
    let ask ~cancel () =
      Dialog.process_still_active ~name:sh#label
        ~ok:sh#quit ~cancel ()
    in
    button_close#connect#clicked ~callback:begin fun () ->
      if sh#alive then (sh#quit())
    end;
    ignore (self#connect#remove_page ~callback:begin fun child ->
      if sh#alive && child#misc#get_oid = sh#misc#get_oid then
        (ask ~cancel:(fun () -> raise Cancel_process_termination)())
    end);

  method set_position x = paned#set_position x

  method remove_all_tabs () =
    try
      List.iter (fun c -> (remove_page#call c); nb#remove c; c#destroy()) nb#children;
      self#set_visible false;
    with Exit -> ()

  method append_page label ?label_widget widget =
    let hbox = GPack.hbox ~spacing:0 () in
    let button_close = GButton.button ~relief:`NONE ~packing:hbox#pack () in
    let image = Icons.create Icons.button_close in
    button_close#event#connect#enter_notify ~callback:begin fun _ ->
      image#set_pixbuf Icons.button_close_hi;
      false
    end;
    button_close#event#connect#leave_notify ~callback:begin fun _ ->
      image#set_pixbuf Icons.button_close;
      false
    end;
    button_close#set_image image#coerce;
    button_close#connect#after#clicked ~callback:widget#destroy;
    let label = match label_widget with
      | None -> (GMisc.label ~text:label ~packing:hbox#pack ())#coerce
      | Some widget -> hbox#pack widget; widget#coerce
    in
    nb#append_page ~tab_label:hbox#coerce widget;
    begin fun finish ->
      hbox#misc#set_sensitive finish;
    end, button_close

  method present page =
    self#set_visible true;
    nb#goto_page (nb#page_num page);

  method width = nb#misc#allocation.Gtk.width

  method connect = new signals ~remove_page ~visible_changed

  initializer
    paned#add2 nb#coerce;
    nb#set_tab_pos `BOTTOM;
    ignore (nb#connect#remove ~callback:
      begin fun w ->
        memo <- List.filter (fun x -> x#misc#get_oid <> w#misc#get_oid) memo;
        if self#empty && self#visible then (self#set_visible false);
      end);
    self#set_visible false;
end

(* Messages Signals *)
and signals ~remove_page ~visible_changed =
object (self)
  inherit ml_signals [remove_page#disconnect; visible_changed#disconnect ]
  method remove_page = remove_page#connect ~after
  method visible_changed = visible_changed#connect ~after
end

and remove_page () = object (self)
  inherit [GObj.widget] signal ()
end

and visible_changed () = object (self)
  inherit [bool] signal ()
end


(** Abstract message *)
and virtual message ~id ~label =
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
object (self)
  inherit GObj.widget sw#as_widget as super
  method remove w =
    sw#remove w;
    w#destroy();
  method pack = sw#add
  method label = label
(*  method id = id*)
  method clear () = List.iter sw#remove sw#children
  method sw = sw
  initializer
    self#misc#set_name label;
end


(** Class OCaml Shell *)
and ocaml_shell ?project () =
  let hbox = GPack.hbox () in
  let toolbar = GButton.toolbar ~tooltips:true ~orientation:`VERTICAL ~style:`ICONS
    ~packing:(hbox#pack ~expand:false) () in
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
  let sh = new Shell.ocaml ~prog ~env:(Unix.environment()) ~args:[||] ~packing:sw#add () in
  let _ = Ocaml_text.shells := sh :: !Ocaml_text.shells in
object (self)
  inherit GObj.widget hbox#as_widget as super
  method textview = sh#textview
  method label = "OCaml Toplevel"
  method alive = sh#alive
  method quit () =
    Ocaml_text.shells := List.filter ((<>) sh) !Ocaml_text.shells;
    sh#quit ()
  initializer
    let path = match project with None -> assert false
      | Some p -> (p.root // Project.src) in
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


let paned = GPack.paned `VERTICAL ()

let messages = new messages ~paned ()




















