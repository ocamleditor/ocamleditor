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

class widget ~project ?(spacing=3) ?border_width ~label_width ?packing ()=
  let vbox = GPack.vbox ~spacing ?border_width ?packing () in
  let top = ref 0 in
  let table = GPack.table ~row_spacings:spacing ~col_spacings:spacing ~packing:vbox#add () in

  let _ = incr top in
  let check_default = GButton.check_button ~label:"System default" ~packing:(table#attach ~top:!top ~left:0) () in

  let _ = incr top in
  let _ = GMisc.label ~xalign:0.0 ~width:label_width ~text:"Version:" ~packing:(table#attach ~top:!top ~left:0) () in
  let box = GPack.hbox ~spacing ~packing:(table#attach ~top:!top ~left:1 ~expand:`X) () in
  let entry_version = GEdit.entry ~editable:false ~packing:box#add () in
  let button_version = GButton.button ~label:"  ...  " ~packing:box#pack () in

  let _ = incr top in
  let _ = GMisc.label ~xalign:0.0 ~width:label_width ~text:"OCAMLLIB:" ~packing:(table#attach ~top:!top ~left:0) () in
  let box = GPack.hbox ~spacing ~packing:(table#attach ~top:!top ~left:1 ~expand:`X) () in
  let entry_ocamllib = GEdit.entry ~editable:true ~packing:box#add () in
  let button_ocamllib = GButton.button ~label:"  ...  " ~packing:box#pack () in

  let _ = incr top in
  let _ = GMisc.label ~xalign:0.0 ~width:label_width ~text:"Can compile native:" ~packing:(table#attach ~top:!top ~left:0) () in
  let box = GPack.hbox ~spacing ~packing:(table#attach ~top:!top ~left:1 ~expand:`X) () in
  let entry_native = GEdit.entry ~editable:false ~packing:box#add () in
  let button_native = GButton.button ~label:"Test" ~packing:box#pack () in

  let location = new GUtil.variable (Sys.getcwd()) in
  let original_ocamllib = try Sys.getenv "OCAMLLIB" with Not_found -> "" in
object (self)
  inherit GObj.widget vbox#as_widget

  method private with_ocamllib : 'a.(unit -> 'a) -> 'a = fun f ->
    Ocaml_config.putenv_ocamllib (Some entry_ocamllib#text);
    let finally () = Ocaml_config.putenv_ocamllib (Some original_ocamllib) in
    (try let result = f() in finally(); result with ex -> (finally(); raise ex))

  method get_ocaml_version ?preview path =
    try
      self#with_ocamllib begin fun () ->
        let compiler = Ocaml_config.find_tool `OCAMLC path in
        let version = Miscellanea.replace_all ["\n", " - "] (Miscellanea.trim (Ocaml_config.ocaml_version ~compiler ())) in
        Gaux.may preview ~f:(fun preview -> kprintf preview#set_label "<big><b>%s</b></big>" version);
        version
      end
    with _ -> (Gaux.may preview ~f:(fun preview -> preview#set_label "<big><b> </b></big>"); "<Not found>")

  method chooser ?(with_preview=false) path () =
    let chooser = GWindow.file_chooser_dialog ~position:`CENTER ~modal:true ~action:`SELECT_FOLDER () in
    chooser#add_button_stock `OK `OK;
    chooser#add_button_stock `CANCEL `CANCEL;
    ignore (chooser#set_current_folder path);
    let frame = GBin.viewport ~show:false () in
    let preview = GMisc.label ~markup:"" ~yalign:0.0 ~xpad:3 ~ypad:3 ~packing:frame#add () in
    preview#misc#modify_fg [`NORMAL, `BLACK];
    frame#misc#modify_bg [`NORMAL, `WHITE];
    chooser#vbox#pack frame#coerce;
    chooser#vbox#reorder_child frame#coerce ~pos:1;
    let choose () =
      if with_preview then begin
        frame#misc#show();
        Gaux.may chooser#filename ~f:begin fun path ->
          ignore (self#get_ocaml_version ~preview path);
          location#set path
        end
      end else begin
        frame#misc#hide();
        Gaux.may chooser#filename ~f:(fun path -> entry_ocamllib#set_text path)
      end
    in
    ignore (chooser#connect#current_folder_changed ~callback:choose);
    match chooser#run() with _ -> chooser#destroy()

  method ocamllib = entry_ocamllib#text
  method location = location#get

  initializer
    ignore (button_native#connect#clicked ~callback:begin fun () ->
      entry_native#set_text "Please wait...";
      Gtk_util.idle_add begin fun () ->
        let can_compile_native = self#with_ocamllib (Ocaml_config.can_compile_native ~ocaml_home:location#get) in
        entry_native#set_text (if can_compile_native then "Yes" else "No")
      end
    end);
    ignore (check_default#connect#toggled ~callback:begin fun () ->
      button_version#misc#set_sensitive (not check_default#active);
      button_ocamllib#misc#set_sensitive (not check_default#active);
      entry_ocamllib#set_editable (not check_default#active);
      entry_native#set_text "";
      if check_default#active then begin
        entry_ocamllib#set_text (match Oe_config.system_ocamllib with None -> "" | Some x -> x);
        location#set "";
      end else begin
        location#set project.Project.root;
      end;
    end);
    ignore (button_version#connect#clicked ~callback:(fun () -> self#chooser ~with_preview:true location#get ()));
    ignore (button_ocamllib#connect#clicked ~callback:begin fun () ->
      self#chooser entry_ocamllib#text ();
    end);
    let set_tooltips () =
      entry_ocamllib#misc#set_tooltip_text (sprintf "Environment Variable\nOCAMLLIB=%s" entry_ocamllib#text);
      let loc = if location#get = "" then "<System default>" else location#get in
      entry_version#misc#set_tooltip_text (sprintf "%s\nLocation: %s"
        (Miscellanea.replace_all [" - ", "\n"] entry_version#text) loc);
    in
    ignore (entry_ocamllib#connect#changed ~callback:begin fun () ->
      entry_version#set_text (self#get_ocaml_version location#get);
      set_tooltips();
      entry_native#set_text "";
    end);
    ignore (location#connect#changed ~callback:begin fun path ->
      entry_native#set_text "";
      entry_version#set_text (self#get_ocaml_version path);
      set_tooltips();
    end);
    check_default#set_active (not (project.Project.ocaml_home = ""));
    check_default#set_active (project.Project.ocaml_home = "");
    entry_ocamllib#set_text project.Project.ocamllib;
    location#set project.Project.ocaml_home;
end






