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


open Miscellanea
open Printf
open GdkKeysyms

let default_basename = "_build.ml"

class widget ~project ?packing () =
  let spacing         = 3 in
  let vbox            = GPack.vbox ~spacing:13 ?packing () in
  let text_filename   = "Specify the file to be created by the generation process" in
  let fbox            = GPack.vbox ~spacing ~packing:vbox#pack () in
  let _               = GMisc.label ~text:text_filename ~xalign:0.0 ~packing:fbox#pack () in
  let box             = GPack.hbox ~spacing:5 ~packing:fbox#pack () in
  let entry_filename  = GEdit.entry
    ~text:((!! (Project.filename project)) // default_basename)
    ~packing:box#add () in
  let button_filename = GButton.button ~label:"  ...  " ~packing:box#pack () in
  (*let abox            = GPack.vbox ~spacing ~packing:vbox#add () in
  let _               = GMisc.label ~text:"Define the command line arguments for the build script" ~xalign:0.0 ~packing:abox#pack () in
  let arguments       = new Build_script_args_widget.widget ~project ~packing:abox#add () in*)
object (self)
  inherit GObj.widget vbox#as_widget
  val mutable is_valid = new GUtil.variable true

  initializer
    ignore (button_filename#connect#clicked ~callback:self#choose_file);
    ignore (entry_filename#connect#changed
      ~callback:(fun () -> is_valid#set (Filename.check_suffix entry_filename#text ".ml")));

  method is_valid = is_valid

  method apply () =
    let filename = entry_filename#text in
    if Sys.file_exists filename then begin
      let response = Dialog.confirm ~message:(sprintf
        "Are you sure you want to overwrite file \xC2\xAB%s\xC2\xBB?" (Filename.basename filename))
        ~yes:("Overwrite", (Build_script.create ~project ~filename))
        ~no:("Do Not Overwrite", ignore)
        ~title:"Overwrite File" self
      in response <> `CANCEL
    end else (Build_script.create ~project ~filename (); true);

  method private choose_file () =
    let window = GWindow.file_chooser_dialog
      ~action:`SAVE ~icon:Icons.oe
      ~title:text_filename
      ~position:`CENTER ~modal:true ~show:false ()
    in
    window#set_select_multiple false;
    window#add_select_button_stock `OK `OK;
    window#add_button_stock `CANCEL `CANCEL;
    window#set_default_response `OK;
    let filter = GFile.filter ~name:"OCaml Source File" ~patterns:["*.ml"] () in
    window#set_filter filter;
    ignore (window#set_filename entry_filename#text);
    match window#run () with
      | `OK ->
        Gaux.may window#filename ~f:entry_filename#set_text;
        window#destroy()
      | _ -> window#destroy()

end

let window ~project () =
  let window = GWindow.window ~title:"Generate Build Script"
    ~modal:true ~border_width:8 ~width:500 ~position:`CENTER ~icon:Icons.oe ~show:false () in
  let vbox = GPack.vbox ~spacing:8 ~packing:window#add () in
  let widget = new widget ~project ~packing:vbox#add () in
  let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~spacing:8 ~packing:vbox#pack () in
  let button_ok = GButton.button ~stock:`OK ~packing:bbox#pack () in
  let button_cancel = GButton.button ~stock:`CANCEL ~packing:bbox#pack () in
  ignore (button_ok#connect#clicked ~callback:(fun () -> if widget#apply() then window#destroy()));
  ignore (button_cancel#connect#clicked ~callback:window#destroy);
  ignore (widget#is_valid#connect#changed ~callback:button_ok#misc#set_sensitive);
  (*ignore (window#event#connect#key_press ~callback:begin fun ev ->
    let key = GdkEvent.Key.keyval ev in
    if key = _Return then (button_ok#clicked(); true)
    else if key = _Escape then (window#destroy(); true)
    else false
  end);*)
  window#show();;





