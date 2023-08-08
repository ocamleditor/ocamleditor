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
[@@@warning "-48"]


open Printf
open Miscellanea
open Resource_file
open Target
open Preferences

let cols         = new GTree.column_list
let col_name     = cols#add Gobject.Data.string
let col_pixbuf   = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf")
let col_icofile  = cols#add Gobject.Data.string

class widget ~project ~target ?packing () =
  let saved = new saved () in
  let icon_changed = new icon_changed () in
  let table = GPack.table ~row_spacings:8 ~col_spacings:8 ?packing () in
  let i = ref 0 in
  let _ = GMisc.label ~text:"Title:" ~xalign:0.0 ~packing:(table#attach ~expand:`NONE ~top:!i ~left:0) () in
  let entry_title = GEdit.entry ~packing:(table#attach ~expand:`X ~top:!i ~left:1) () in
  let _ = incr i in
  let _ = GMisc.label ~text:"Company:" ~xalign:0.0 ~packing:(table#attach ~expand:`NONE ~top:!i ~left:0) () in
  let entry_company = GEdit.entry ~packing:(table#attach ~expand:`X ~top:!i ~left:1) () in
  let _ = incr i in
  let _ = GMisc.label ~text:"Product:" ~xalign:0.0 ~packing:(table#attach ~expand:`NONE ~top:!i ~left:0) () in
  let entry_product = GEdit.entry ~packing:(table#attach ~expand:`X ~top:!i ~left:1) () in
  let _ = incr i in
  let _ = GMisc.label ~text:"Copyright:" ~xalign:0.0 ~packing:(table#attach ~expand:`NONE ~top:!i ~left:0) () in
  let entry_copyright = GEdit.entry ~packing:(table#attach ~expand:`X ~top:!i ~left:1) () in
  let _ = incr i in
  let _ = GMisc.label ~text:"File version:" ~xalign:0.0 ~packing:(table#attach ~expand:`NONE ~top:!i ~left:0) () in
  let box_file_version = GPack.hbox ~spacing:8 ~packing:(table#attach ~expand:`X ~top:!i ~left:1) () in
  let adjustment = GData.adjustment ~lower:0. ~step_incr:1. ~page_size:0. () in
  let entry_file_version_1 = GEdit.spin_button ~width:50 ~numeric:true ~digits:0 ~rate:1.0 ~adjustment ~packing:box_file_version#pack () in
  let adjustment = GData.adjustment ~lower:0. ~step_incr:1. ~page_size:0. () in
  let entry_file_version_2 = GEdit.spin_button ~width:50 ~numeric:true ~digits:0 ~rate:1.0 ~adjustment ~packing:box_file_version#pack () in
  let adjustment = GData.adjustment ~lower:0. ~step_incr:1. ~page_size:0. () in
  let entry_file_version_3 = GEdit.spin_button ~width:50 ~numeric:true ~digits:0 ~rate:1.0 ~adjustment ~packing:box_file_version#pack () in
  let adjustment = GData.adjustment ~lower:0. ~step_incr:1. ~page_size:0. () in
  let entry_file_version_4 = GEdit.spin_button ~width:50 ~numeric:true ~digits:0 ~rate:1.0 ~adjustment ~packing:box_file_version#pack () in
  (*  *)
  let _ = incr i in
  let vbox = GPack.vbox ~spacing:5 ~packing:(table#attach ~top:!i ~left:0 ~expand:`BOTH ~right:2) ~show:(not Ocaml_config.is_mingw) () in
  let _ = GMisc.label ~text:"Icons:" ~xalign:0.0 ~packing:vbox#pack () in
  let hbox = GPack.hbox ~spacing:8 ~packing:vbox#add () in
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add () in
  let model = GTree.list_store cols in
  let icon_view = GTree.icon_view ~columns:4 ~width:438 ~height:255 ~model ~selection_mode:`MULTIPLE ~packing:sw#add () in
  let _ = icon_view#set_pixbuf_column col_pixbuf in
  let _ = icon_view#set_markup_column col_name in
  let bbox = GPack.button_box `VERTICAL ~layout:`START ~spacing:8 ~packing:hbox#pack () in
  let button_add = GButton.button ~stock:`ADD ~packing:bbox#pack () in
  let button_remove = GButton.button ~stock:`REMOVE ~packing:bbox#pack () in
  let button_up = GButton.button ~stock:`GO_UP ~packing:bbox#pack () in
  let button_down = GButton.button ~stock:`GO_DOWN ~packing:bbox#pack () in
  let _ = bbox#set_child_secondary button_up#coerce true in
  let _ = bbox#set_child_secondary button_down#coerce true in

  object (self)
    inherit GObj.widget table#as_widget

    initializer
      self#set (match target.Target.resource_file with
          | Some rc -> rc
          | _ ->
              let filename = Target.create_rc_filename target in
              Resource_file.create ~filename ~title:target.name ~product:target.name ());
      (* button_add *)
      button_add#connect#clicked ~callback:begin fun () ->
        let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~modal:true ~title:"Add existing file to project"
            ~icon:(??? Icons.oe) ~position:`CENTER ~show:false () in
        dialog#add_filter (GFile.filter ~name:"Icons Files (\x2A.ico)" ~patterns:["*.ico"] ());
        dialog#add_select_button_stock `OK `OK;
        dialog#add_button_stock `CANCEL `CANCEL;
        dialog#set_select_multiple true;
        dialog#set_current_folder (project.Prj.root // Prj.default_dir_src) |> ignore;
        match dialog#run () with
        | `OK ->
            List.iter (fun fn -> self#load_icon_from_file (Filename.basename fn) fn) dialog#get_filenames;
            dialog#destroy()
        | _ -> dialog#destroy()
      end |> ignore;
      (* button_remove *)
      button_remove#connect#clicked ~callback:begin fun () ->
        let paths = icon_view#get_selected_items in
        let rr = List.map model#get_row_reference paths in
        List.iter (fun reference -> model#remove reference#iter |> ignore) rr;
      end |> ignore;
      (* button_up, button_down *)
      let move up =
        let paths = icon_view#get_selected_items in
        let rr = List.map model#get_row_reference paths in
        let move =
          match up with
          | true -> fun path reference ->
            if GTree.Path.prev path then
              model#move_before ~iter:reference#iter ~pos:(model#get_iter path) |> ignore
          | false -> fun path reference ->
            try
              GTree.Path.next path;
              model#move_after ~iter:reference#iter ~pos:(model#get_iter path) |> ignore
            with Failure _ -> ()
        in
        List.iter (fun r -> move (model#get_path r#iter) r) rr;
        icon_changed#call();
      in
      button_up#connect#clicked ~callback:(fun () -> move true) |> ignore;
      button_down#connect#clicked ~callback:(fun () -> move false) |> ignore;
      let callback () =
        let f button = button#misc#set_sensitive (icon_view#get_selected_items <> []) in
        List.iter f [button_up; button_down; button_remove]
      in
      icon_view#connect#selection_changed ~callback |> ignore;
      callback();

    method set res =
      entry_title#set_text res.rc_title;
      entry_company#set_text res.rc_company;
      entry_product#set_text res.rc_product;
      entry_copyright#set_text res.rc_copyright;
      begin
        match res.rc_file_version with a,b,c,d ->
          entry_file_version_1#set_value (float a);
          entry_file_version_2#set_value (float b);
          entry_file_version_3#set_value (float c);
          entry_file_version_4#set_value (float d);
      end;
      List.iter begin fun filename ->
        Gmisclib.Idle.add ~prio:300 begin fun () ->
          self#load_icon_from_file (Filename.basename filename) filename
        end
      end res.rc_icons(*_data*);

    method get =
      let icofiles = ref [] in
      model#foreach begin fun _ row ->
        let filename = model#get ~row ~column:col_icofile in
        icofiles := filename :: !icofiles;
        false
      end;
      Resource_file.create
        ~filename:(Target.create_rc_filename target)
        ~title:entry_title#text
        ~company:entry_company#text
        ~product:entry_product#text
        ~copyright:entry_copyright#text
        ~file_version:(entry_file_version_1#value_as_int, entry_file_version_2#value_as_int,
                       entry_file_version_3#value_as_int, entry_file_version_4#value_as_int)
        ~icons:(List.rev !icofiles) ()

    method save () = saved#call self#get

    method private load_icon_from_file iconame filename =
      if not Ocaml_config.is_mingw && Sys.file_exists filename then begin
        let size = (Unix.stat filename).Unix.st_size in
        if size > 0 then begin
          let visual_size = 64 in
          let pixbuf = GdkPixbuf.from_file filename in
          let width = GdkPixbuf.get_width pixbuf in
          let height = GdkPixbuf.get_height pixbuf in
          let icon =
            if max width height <= visual_size then pixbuf else begin
              let w, h =
                if width > height then visual_size, width / height * visual_size
                else height / width * visual_size, visual_size
              in
              let dest = GdkPixbuf.create ~width:w ~height:h ~has_alpha:(GdkPixbuf.get_has_alpha pixbuf) () in
              GdkPixbuf.scale ~dest ~width:w ~height:h pixbuf;
              dest
            end
          in
          let row = model#append () in
          model#set ~row ~column:col_name
            (sprintf "%s\n<span size='small' style='italic'>(%d × %d)</span>"
               (Filename.chop_extension iconame) width height);
          model#set ~row ~column:col_pixbuf icon;
          model#set ~row ~column:col_icofile filename;
          icon_changed#call();
        end
      end

    method get_first_icon () =
      match model#get_iter_first with
      | Some row ->
          let pixbuf = model#get ~row ~column:col_pixbuf in
          Some pixbuf
      | _ -> None

    method connect = new signals ~saved ~icon_changed
  end

and saved () = object inherit [Resource_file.t] GUtil.signal () end
and icon_changed () = object inherit [unit] GUtil.signal () end

and signals ~saved ~icon_changed =
  object
    inherit GUtil.ml_signals [saved#disconnect; icon_changed#disconnect]
    method saved = saved#connect ~after
    method icon_changed = icon_changed#connect ~after
  end

(** window *)
let window ~project ~target () =
  let window = GWindow.window ~title:"Icons and Assembly Information" ~modal:true ()
      ~icon:(??? Icons.oe) ~position:`CENTER ~show:false in
  Gmisclib.Util.esc_destroy_window window;
  let vbox = GPack.vbox ~spacing:8 ~border_width:8 ~packing:window#add () in
  let widget = new widget ~project ~target ~packing:vbox#add () in
  (*  *)
  let _ = GMisc.separator `HORIZONTAL ~packing:vbox#pack () in
  let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~spacing:8 ~packing:vbox#pack () in
  let button_ok = GButton.button ~stock:`OK ~packing:bbox#pack () in
  let button_cancel = GButton.button ~stock:`CANCEL ~packing:bbox#pack () in
  button_cancel#connect#clicked ~callback:window#destroy |> ignore;
  button_ok#connect#clicked ~callback:(fun () -> widget#save(); button_cancel#clicked()) |> ignore;
  widget#connect#icon_changed ~callback:begin fun () ->
    Gaux.may (widget#get_first_icon ()) ~f:(fun x -> window#set_icon (Some x));
  end |> ignore;
  window#present();
  window, widget
