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
open Bconf
open GUtil
open Miscellanea

let mk_target_filenames project filenames =
  let filenames = Miscellanea.Xlist.filter_map project.Project.in_source_path filenames in
  if Oe_config.is_win32 then begin
    List.map begin fun filename ->
      Miscellanea.filename_unix_implicit filename
    end filenames
  end else filenames

class view ~project ?packing () =
  let changed = new changed () in
  let xalign = 0.0 in
  let mainbox = GPack.vbox ~spacing:8 ?packing () in
  let entry_name = GEdit.entry ~packing:mainbox#pack () in
  let nb = GPack.notebook ~packing:mainbox#add () in

  (** Target Tab *)
  let vbox = GPack.vbox ~border_width:5 ~spacing:13 () in
  let _ = nb#append_page ~tab_label:(GMisc.label ~text:"Target" ())#coerce vbox#coerce in
  (** Library *)
  (* Build a library with the specified toplevel modules *)
  let mbox = GPack.vbox ~spacing:0 ~packing:(vbox#pack ~expand:false) () in
  let radio_archive = GButton.radio_button ~packing:mbox#add () in
  let label_radio_archive = GMisc.label ~markup:"Build an archive" () in
  let _ = radio_archive#add label_radio_archive#coerce in

  let align_lib = GBin.alignment ~padding:(0,0,21,0) ~packing:mbox#add () in
  let lbox = GPack.vbox ~spacing:8 ~packing:align_lib#add () in

  (** Output kind *)
  let combo_kind, _ = GEdit.combo_box_text
    ~strings:["Library (-a)"; "Plugin (-shared)"; "Pack (-pack)"]
    (*~active:0*) ~packing:lbox#add () in
  (** Toplevel modules list *)
  let box = GPack.vbox ~packing:lbox#pack () in
  let _ = GMisc.label ~markup:"Toplevel modules" ~xalign:0.0 ~packing:box#pack () in

  let hbox = GPack.hbox ~spacing:3 ~packing:box#pack () in
  let entry_lib_modules = GEdit.entry ~editable:true ~packing:hbox#add () in
  let button_lib_modules = GButton.button ~label:"  ...  " ~packing:(hbox#pack ~expand:false) () in
  let _ = button_lib_modules#connect#clicked ~callback:begin fun () ->
    let chooser = GWindow.file_chooser_dialog ~title:"Choose the toplevel modules for the library..."
      ~action:`OPEN ~position:`CENTER ~show:false () in
    chooser#add_select_button_stock `OK `OK;
    chooser#add_button_stock `CANCEL `CANCEL;
    chooser#set_select_multiple true;
    chooser#set_filter (GFile.filter ~patterns:["*.ml"] ());
    chooser#add_filter (GFile.filter ~patterns:["*.ml"] ());
    match chooser#run () with
      | `OK ->
        let filenames = mk_target_filenames project chooser#get_filenames in
        entry_lib_modules#set_text (String.concat " " filenames);
        chooser#destroy()
      | _ -> chooser#destroy()
  end in
  (** Install path for library *)
  let box = GPack.vbox ~packing:lbox#pack () in
  let ocamllib = project.Project.ocamllib in
  let markup = sprintf "Installation path, relative to the standard library directory (<small><tt>%s</tt></small>) " ocamllib in
  let _ = GMisc.label ~markup ~xalign:0.0 ~packing:box#pack () in
  let hbox = GPack.hbox ~spacing:3 ~packing:box#pack () in
  let entry_lib_install = GEdit.entry ~editable:true ~packing:hbox#add () in
  let button_lib_install = GButton.button ~label:"  ...  " ~packing:hbox#pack () in
  let _ = button_lib_install#misc#set_sensitive false in
(*  let _ = button_lib_install#connect#clicked ~callback:begin fun () ->
    let chooser = GWindow.file_chooser_dialog ~title:"Install path for the library"
      ~action:`SELECT_FOLDER ~position:`CENTER ~show:false () in
    chooser#add_select_button_stock `OK `OK;
    chooser#add_button_stock `CANCEL `CANCEL;
    chooser#set_select_multiple false;
    chooser#set_current_folder Oe_config.ocamllib;
    match chooser#run () with
      | `OK ->
        entry_lib_install#set_text (try List.hd chooser#get_filenames with Failure "hd" -> "");
        chooser#destroy()
      | _ -> chooser#destroy()
  end in*)
  (** Executable *)
  let mbox = GPack.vbox ~spacing:0 ~packing:(vbox#pack ~expand:false) () in
  let radio_executable = GButton.radio_button ~group:radio_archive#group ~packing:mbox#add () in
  let label_radio_executable = GMisc.label ~markup:"Build an executable with the specified main module" () in
  let _ = radio_executable#add label_radio_executable#coerce in
  let align_exec = GBin.alignment ~padding:(0,0,21,0) ~packing:mbox#add () in
  let box = GPack.vbox ~spacing:8 ~packing:align_exec#add () in
  let hbox = GPack.hbox ~spacing:3 ~packing:(box#pack ~expand:false) () in
  let entry_main_module = GEdit.entry ~editable:false ~packing:hbox#add () in
  let button_main_module = GButton.button ~label:"  ...  " ~packing:(hbox#pack ~expand:false) () in
  let _ = button_main_module#connect#clicked ~callback:begin fun () ->
    let chooser = GWindow.file_chooser_dialog ~title:"Choose the main module..."
      ~action:`OPEN ~position:`CENTER ~show:false () in
    chooser#add_select_button_stock `OK `OK;
    chooser#add_button_stock `CANCEL `CANCEL;
    chooser#set_select_multiple false;
    chooser#set_filter (GFile.filter ~patterns:["*.ml"] ());
    match chooser#run () with
      | `OK ->
        begin
          try
            let filename = List.hd (mk_target_filenames project chooser#get_filenames) in
            entry_main_module#set_text filename;
          with Failure "hd" -> ()
        end;
        chooser#destroy()
      | _ -> chooser#destroy()
  end in
  (** Outname *)
  let box = GPack.vbox ~packing:(vbox#pack ~expand:false) () in
  let _ = GMisc.label ~markup:"Output file name <small><tt>(-o)</tt></small>" ~xalign ~packing:box#add () in
  let entry_outname = GEdit.entry ~packing:box#add () in

  (** Options Tab *)
  let vbox = GPack.vbox ~width:550 ~border_width:5 ~spacing:8 () in
  let _ = nb#append_page ~tab_label:(GMisc.label ~text:"Options" ())#coerce vbox#coerce in

  let box = GPack.vbox ~spacing:0 ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"Compilation: " ~xalign:0.0 ~packing:box#pack () in
  let combo_comp, _ = GEdit.combo_box_text
    ~strings:["Bytecode"; "Native-code"; "Bytecode and native"]
    ~active:0 ~packing:box#add () in

  let box = GPack.vbox ~packing:(vbox#pack ~expand:false) () in
  let _ = GMisc.label ~markup:"Search path <small><tt>(-I)</tt></small>" ~xalign ~packing:box#add () in
  let entry_includes = GEdit.entry ~packing:box#add () in

  let box = GPack.vbox ~packing:(vbox#pack ~expand:false) () in
  let _ = GMisc.label ~markup:"Required libraries <small><tt>(-l)</tt></small>" ~xalign ~packing:box#add () in
  let entry_libs = GEdit.entry ~packing:box#add () in

  let box = GPack.vbox ~packing:(vbox#pack ~expand:false) () in
  let _ = GMisc.label ~markup:"Other required object files <small><tt>(-m)</tt></small>" ~xalign ~packing:box#add () in
  let entry_mods = GEdit.entry ~packing:box#add () in

  let box = GPack.hbox ~packing:vbox#pack () in
  let check_thread = GButton.check_button ~label:"-thread" ~packing:box#add () in
  let check_vmthread = GButton.check_button ~label:"-vmthread" ~packing:box#add () in

  let box = GPack.vbox ~packing:vbox#pack () in
  let _ = GMisc.label ~markup:"Preprocessor <small><tt>(-pp)</tt></small>" ~xalign ~packing:box#add () in
  let entry_pp = GEdit.entry ~packing:box#add () in

  let box = GPack.vbox ~packing:(vbox#pack ~expand:false) () in
  let _ = GMisc.label ~text:"Compiler flags" ~xalign ~packing:box#add () in
  let entry_cflags = GEdit.entry ~packing:box#add () in

  let box = GPack.vbox ~packing:(vbox#pack ~expand:false) () in
  let _ = GMisc.label ~text:"Linker flags" ~xalign ~packing:box#add () in
  let entry_lflags = GEdit.entry ~packing:box#add () in

  (*  *)
  let cmd_line = GEdit.entry ~editable:false () in
object (self)
  inherit GObj.widget mainbox#as_widget
  val mutable bconf = None
  val mutable signals_enabled = true
  val mutable page_changed = false

  initializer
    ignore (entry_name#connect#changed
      ~callback:begin fun () ->
        self#update (fun bconf -> bconf.name <- entry_name#text) ();
        changed#call()
      end);
    ignore (combo_comp#connect#changed
      ~callback:begin fun () ->
        self#update (fun bconf -> bconf.byt <- (combo_comp#active = 0 || combo_comp#active = 2)) ();
        changed#call()
      end);
    ignore (combo_comp#connect#changed
      ~callback:(self#update (fun bconf -> bconf.opt <- (combo_comp#active = 1 || combo_comp#active = 2))));
    ignore (entry_libs#connect#changed
      ~callback:(self#update (fun bconf -> bconf.libs <- entry_libs#text)));
    ignore (entry_mods#connect#changed
      ~callback:(self#update (fun bconf -> bconf.other_objects <- entry_mods#text)));
    ignore (entry_main_module#connect#changed
      ~callback:(self#update begin fun bconf ->
        if signals_enabled then begin
          bconf.files <- entry_main_module#text;
          if entry_outname#text = "" then (entry_outname#set_text
            (Filename.chop_extension (Filename.basename entry_main_module#text)))
        end
      end));
    ignore (entry_lib_modules#connect#changed
      ~callback:(self#update begin fun bconf ->
        if signals_enabled then begin
          bconf.files <- entry_lib_modules#text
        end
      end));
    ignore (entry_includes#connect#changed
      ~callback:(self#update (fun bconf -> bconf.includes <- entry_includes#text)));
    ignore (check_thread#connect#toggled
      ~callback:(self#update (fun bconf -> bconf.thread <- check_thread#active)));
    ignore (check_vmthread#connect#toggled
      ~callback:(self#update (fun bconf -> bconf.vmthread <- check_vmthread#active)));
    ignore (entry_pp#connect#changed
      ~callback:(self#update (fun bconf -> bconf.pp <- entry_pp#text)));
    ignore (entry_cflags#connect#changed
      ~callback:(self#update (fun bconf -> bconf.cflags <- entry_cflags#text)));
    ignore (entry_lflags#connect#changed
      ~callback:(self#update (fun bconf -> bconf.lflags <- entry_lflags#text)));
    ignore (radio_archive#connect#after#toggled
      ~callback:(self#update begin fun bconf ->
        if signals_enabled then begin
          bconf.outkind <- begin
            if radio_archive#active then
              (match combo_kind#active with 0 -> Library | 1 -> Plugin | 2 -> Pack | _ -> assert false)
            else Executable;
          end;
          entry_lib_modules#set_text "";
          entry_main_module#set_text "";
        end
      end));
    ignore (combo_kind#connect#changed
      ~callback:(self#update (fun bconf -> bconf.outkind <-
        if radio_archive#active then
          (match combo_kind#active with 0 -> Library | 1 -> Plugin | 2 -> Pack | _ -> assert false)
        else Executable)));
    ignore (entry_outname#connect#changed
      ~callback:(self#update (fun bconf -> bconf.outname <- entry_outname#text)));
    ignore (entry_lib_install#connect#changed
      ~callback:(self#update begin fun bconf ->
        bconf.lib_install_path <- entry_lib_install#text
      end));
    let _ = check_thread#connect#toggled ~callback:begin fun () ->
      if signals_enabled then begin
        check_vmthread#set_active false;
        check_vmthread#misc#set_sensitive (not check_thread#active);
      end
    end in
    let _ = check_vmthread#connect#toggled ~callback:begin fun () ->
      if signals_enabled then begin
        check_thread#set_active false;
        check_thread#misc#set_sensitive (not check_vmthread#active);
      end
    end in
    let _ = radio_executable#connect#clicked ~callback:begin fun () ->
      align_lib#misc#set_sensitive (not radio_executable#active);
      align_exec#misc#set_sensitive radio_executable#active;
      if radio_executable#active then (entry_main_module#misc#grab_focus())
      else (button_lib_modules#misc#grab_focus());
    end in
    ignore (self#connect#changed ~callback:(fun () -> page_changed <- true))

  method changed = page_changed
  method set_changed x = page_changed <- x
  method entry_cmd_line = cmd_line
  method entry_name = entry_name

  method private update update_func () =
    Gaux.may bconf ~f:begin fun bc ->
      update_func bc;
      self#update_cmd_line bc;
      (*changed#call()*)
    end;

  method set bc =
    signals_enabled <- false;
    bconf <- Some bc;
    entry_name#set_text bc.name;
    combo_comp#set_active (match bc.byt, bc.opt with
      | true, false -> 0
      | false, true -> 1
      | true, true -> 2
      | _ -> 0);
    entry_libs#set_text bc.libs;
    entry_mods#set_text bc.other_objects;
    entry_includes#set_text bc.includes;
    if bc.thread then begin
      check_thread#set_active true;
      check_thread#misc#set_sensitive true;
      check_vmthread#set_active false;
      check_vmthread#misc#set_sensitive false;
    end else if bc.vmthread then begin
      check_thread#set_active false;
      check_thread#misc#set_sensitive false;
      check_vmthread#set_active true;
      check_vmthread#misc#set_sensitive true;
    end else begin
      check_thread#set_active false;
      check_thread#misc#set_sensitive true;
      check_vmthread#set_active false;
      check_vmthread#misc#set_sensitive true;
    end;
    entry_pp#set_text bc.pp;
    entry_cflags#set_text bc.cflags;
    entry_lflags#set_text bc.lflags;
    begin
      match bc.outkind with
        | Executable -> radio_executable#set_active true
        | _ -> radio_archive#set_active true
    end;
    combo_kind#set_active (match bc.outkind with Library -> 0 | Plugin -> 1 | Pack -> 2 | _ -> 0);
    entry_outname#set_text bc.outname;
    entry_lib_install#set_text bc.lib_install_path;
    if (List.mem bc.outkind [Library; Plugin; Pack]) then begin
      align_lib#misc#set_sensitive true;
      align_exec#misc#set_sensitive false;
      entry_lib_modules#set_text bc.files;
      entry_main_module#set_text "";
    end else begin
      align_lib#misc#set_sensitive false;
      align_exec#misc#set_sensitive true;
      entry_lib_modules#set_text "";
      let filename = Miscellanea.split " +" bc.files in
      assert (List.length filename <= 1);
      try
        let filename = List.hd filename in
        entry_main_module#set_text filename;
      with Failure "hd" -> (ignore (entry_main_module#set_text ""))
    end;
    (*  *)
    signals_enabled <- true;
    self#update_cmd_line bc;

  method private update_cmd_line bconf =
    let cmd, args = create_cmd_line bconf in
    let cmd = sprintf "%s %s" cmd (String.concat " " args) in
    cmd_line#set_text cmd

  method connect = new signals ~changed

end

and changed () = object (self) inherit [unit] signal () as super end
and signals ~changed =
object (self)
  inherit ml_signals [changed#disconnect]
  method changed = changed#connect ~after
end
