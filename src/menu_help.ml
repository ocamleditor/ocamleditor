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


open Miscellanea
open Printf
open Preferences

(** system_properties *)
let system_properties () =
  let window = GWindow.window ~icon:(??? Icons.oe) ~title:"System Properties" ~position:`CENTER ~modal:true ~resizable:false ~show:false () in
  let text = Glib.Convert.locale_to_utf8 (System_properties.to_string ()) in
  let buffer = GText.buffer ~text () in
  let vbox = GPack.vbox ~spacing:0 ~border_width:0 ~packing:window#add () in
  let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GText.view ~buffer ~editable:false ~width:850 ~height:600 ~packing:sw#add () in
  view#misc#modify_font_by_name (Preferences.preferences#get.editor_base_font);
  view#set_left_margin 8;
  view#set_right_margin 8;
  view#set_pixels_above_lines 2;
  view#set_pixels_below_lines 2;
  view#set_cursor_visible false;
  let bbox = GPack.button_box `HORIZONTAL ~layout:`SPREAD ~border_width:8 ~packing:vbox#pack () in
  let button_close = GButton.button ~packing:bbox#pack () in
  button_close#set_image (Icons.create (??? Icons.close_16))#coerce;
  button_close#connect#clicked ~callback:window#destroy |> ignore;
  Gmisclib.Util.esc_destroy_window window;
  window#show();
  window#set_position `CENTER_ALWAYS


(** about *)
let about editor () =
  let dialog = GWindow.about_dialog
      ~type_hint:(if Sys.os_type = "Win32" then `SPLASHSCREEN else `DIALOG)
      ~modal:true
      ~width:310
      ~position:`CENTER
      ~icon:(??? Icons.oe)
      ~name:About.program_name
      ~version:About.version
      ~copyright:About.copyright
      ~logo:(??? Icons.logo)
      ~comments:(sprintf "Commit: %s" !About.git_hash)
      ~license:{|OCamlEditor is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

OCamlEditor is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.|}
      ~show:false
      ()
  in
  let vbox = dialog#vbox in
  let modify_label ?(color="#808080") label =
    label#misc#modify_fg [`NORMAL, `NAME color];
    label#misc#modify_font_by_name "Sans 7";
  in
  let hbox = GPack.hbox ~packing:vbox#pack ~show:false () in
  (* Link website *)
  let link_button = GButton.button ~relief:`NONE ~packing:hbox#add () in
  let label = GMisc.label ~text:(sprintf "Visit the %s website" About.program_name) ~packing:link_button#add () in
  modify_label ~color:"#0000ff" label;
  link_button#set_focus_on_click false;
  ignore (link_button#connect#clicked ~callback:(fun () ->
      kprintf open_url "%s?%s-%s-%s"
        About.website Sys.os_type About.version !About.build_id));

  (* Report a bug *)
  let link_button = GButton.button ~relief:`NONE ~packing:hbox#add () in
  let label = GMisc.label ~text:"Report an issue" ~packing:link_button#add () in
  modify_label ~color:"#0000ff" label;
  link_button#set_focus_on_click false;
  ignore (link_button#connect#clicked ~callback:(fun () -> open_url About.issues));

  (* Check for updates *)
  dialog#set_skip_taskbar_hint true;
  dialog#set_skip_pager_hint true;
  Gaux.may (GWindow.toplevel editor) ~f:(fun x -> dialog#set_transient_for x#as_window);
  let align = GBin.alignment ~xalign:0.5 ~packing:vbox#add () in
  let hbox = GPack.hbox ~spacing:3 ~packing:align#add () in
  let icon = GMisc.image ~xalign:1.0 ~file:(Icon.get_themed_filename "spinner_16.gif") ~packing:hbox#add ~show:false () in
  let label = GMisc.label ~text:" " ~height:22 ~xalign:0.0 ~yalign:0.5 ~packing:hbox#add () in
  icon#set_icon_size `MENU;
  modify_label label;
  let check_for_updates () =
    try
      icon#misc#hide();
      hbox#set_child_packing ~expand:true label#coerce;
      label#set_xalign 0.5;
      if try int_of_string (List.assoc "debug" App_config.application_param) >= 1 with Not_found -> false then (raise Exit);
      begin
        match Check_for_updates.check About.version () with
        | Some ver ->
            icon#set_stock `DIALOG_INFO;
            label#misc#hide();
            let text = sprintf "A new version of %s is available (%s)" About.program_name ver in
            let button = GButton.button ~packing:(hbox#pack ~expand:true ~fill:false) () in
            let label = GMisc.label ~text ~xalign:0.5 ~yalign:0.5 ~packing:button#add () in
            modify_label ~color:"#0000ff" label;
            button#set_relief `NONE;
            ignore (button#connect#clicked ~callback:begin fun () ->
                dialog#misc#hide();
                open_url About.releases;
                dialog#destroy();
              end);
        | None ->
            icon#set_stock `APPLY;
            kprintf label#set_text "%s is up to date." About.program_name
      end;
    with ex -> begin
        kprintf label#set_text "Unable to contact server for updates (%s)." (Printexc.to_string ex);
        icon#set_pixbuf (??? Icons.warning_14);
      end
  in
  (*ignore (dialog#misc#connect#show ~callback:begin fun () ->
      let count = ref 0 in
      ignore (GMain.Timeout.add ~ms:600 ~callback:begin fun () ->
          if !count = 1 then begin
            icon#misc#show();
            label#set_text "Checking for updates...";
          end else if !count = 2 then (check_for_updates());
          incr count;
          true
        end)
    end);*)
  match dialog#run() with _ -> dialog#destroy()





