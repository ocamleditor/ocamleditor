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


open Pref_page
open Printf

(** pref_editor_indent *)
class pref_editor_indent title ?packing () =
  let vbox        = GPack.vbox ~spacing ?packing () in
  let box         = GPack.hbox ~spacing:5 ~packing:vbox#pack () in
  let _           = GMisc.label ~text:"Tab key on empty lines:" ~xalign:0.0 ~packing:box#pack () in
  let combo_empty, _ = GEdit.combo_box_text ~active:0 ~strings:[
      "Indent according to formatting options"; "Indent to match preceding line";
    ] ~packing:box#add () in
  let box         = GPack.vbox ~spacing:2 ~packing:vbox#pack () in
  let _           = GMisc.label ~markup:"ocp-indent --config" ~xalign:0.0 ~packing:box#pack () in
  let box         = GPack.vbox ~spacing:5 ~packing:box#add () in
  let buffer      = GText.buffer () in
  let sw          = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN ~packing:box#add () in
  let view        = GText.view ~buffer ~height:50 ~packing:sw#add () in
  let buffer_help = GText.buffer () in
  let sw_help     = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN ~packing:box#add () in
  let view_help   = GText.view ~buffer:buffer_help ~height:380 ~packing:sw_help#add () in
  object
  inherit page title vbox

  initializer
    view#set_left_margin 2;
    view#set_right_margin 2;
    view_help#set_left_margin 2;
    view_help#set_right_margin 2;
    view_help#set_editable false;
(*    view_help#misc#modify_base *)
    view#set_wrap_mode `WORD;
    view_help#set_wrap_mode `WORD;
    let pref = Preferences.preferences#get in
    view#misc#modify_font_by_name pref.Preferences.pref_base_font;
    view_help#misc#modify_font_by_name pref.Preferences.pref_base_font;
    view_help#set_cursor_visible false;
    try
      let help = kprintf Shell.get_command_output "ocp-indent --help=plain %s" Shell.redirect_stderr in
      let help = String.concat "\n" help in
      let help = if Glib.Utf8.validate help then help else Glib.Convert.locale_to_utf8 help in
      buffer_help#set_text help;
    with Glib.Convert.Error _ -> ()

  method write pref =
    pref.Preferences.pref_editor_indent_empty_line <- (match combo_empty#active with
      | 0 -> true
      | _ -> false);
    pref.Preferences.pref_editor_indent_config <- buffer#get_text();

  method read pref =
    combo_empty#set_active (if pref.Preferences.pref_editor_indent_empty_line then 0 else 1);
    buffer#set_text pref.Preferences.pref_editor_indent_config;
end






