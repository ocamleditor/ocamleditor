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

exception Mark_deleted

let _ = Gmisclib.Util.fade_window_enabled := Oe_config.fade_window_enabled

(** window *)
let window widget ?parent ?(destroy_child=true) ?(fade=false) ~x ~y () =
  let window = GWindow.window
    ~decorated:false
    ~border_width:1
    ~deletable:true
    ~focus_on_map:true
    ~show:false ()
  in
  let ebox = GBin.event_box ~packing:window#add () in
  ebox#add widget;
  let color = Color.set_value 0.62 (`NAME !Preferences.preferences.Preferences.pref_bg_color_popup) in
  let _ = window#misc#modify_bg [`NORMAL, color] in
  let _ = ebox#misc#modify_bg [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_bg_color_popup] in
  ignore (window#event#connect#after#focus_out ~callback:begin fun _ ->
    if not destroy_child then (ebox#remove widget);
    window#destroy();
    true
  end);
  ignore (window#event#connect#key_release ~callback:begin fun ev ->
    let key = GdkEvent.Key.keyval ev in
    if key = GdkKeysyms._Escape then (window#destroy(); true) else false
  end);
  window#set_skip_pager_hint true;
  window#set_skip_taskbar_hint true;
  Gaux.may parent ~f:(fun parent -> Gaux.may (GWindow.toplevel parent) ~f:(fun x -> window#set_transient_for x#as_window));
  window#set_accept_focus true;
  if fade then (window#set_opacity 0.0);
  if Sys.os_type = "Win32" then (window#present());
  window#move ~x ~y;
  let alloc = window#misc#allocation in
  let x, y =
    (if x + alloc.Gtk.width > (Gdk.Screen.width()) then (Gdk.Screen.width() - alloc.Gtk.width) else x),
    (if y + alloc.Gtk.height > (Gdk.Screen.height()) then (Gdk.Screen.height() - alloc.Gtk.height) else y);
  in
  window#move ~x ~y;
  if fade then (Gmisclib.Util.fade_window window);
  if Sys.os_type <> "Win32" then (window#present());
  window

(** window_tooltip *)
let window_tooltip widget ?parent ?(fade=false) ~x ~y () =
  let window = GWindow.window
    ~decorated:false
    ~kind:(if Sys.os_type = "Win32" then `POPUP else `TOPLEVEL)
    ~type_hint:(if Sys.os_type = "Win32" then `MENU else `NORMAL)
    ~border_width:1
    ~show:false ()
  in
  let ebox = GBin.event_box ~packing:window#add () in
  ebox#add widget;
  let color = Color.set_value 0.62 (`NAME !Preferences.preferences.Preferences.pref_bg_color_popup) in
  let _ = window#misc#modify_bg [`NORMAL, color] in
  let _ = ebox#misc#modify_bg [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_bg_color_popup] in
  window#set_skip_pager_hint true;
  window#set_skip_taskbar_hint true;
  window#set_accept_focus false;
  window#misc#set_can_focus false;
  window#set_focus_on_map false;
  Gaux.may parent ~f:(fun parent -> Gaux.may (GWindow.toplevel parent) ~f:(fun x -> window#set_transient_for x#as_window));
  if fade then (window#set_opacity 0.0);
  window#move ~x ~y;
  if fade then (Gmisclib.Util.fade_window window);
  if Sys.os_type <> "Win32" then (window#present());
  let alloc = window#misc#allocation in
  let x, y =
    (if x + alloc.Gtk.width > (Gdk.Screen.width()) then (Gdk.Screen.width() - alloc.Gtk.width) else x),
    (if y + alloc.Gtk.height > (Gdk.Screen.height()) then (Gdk.Screen.height() - alloc.Gtk.height) else y);
  in
  window#move ~x ~y;
  window











