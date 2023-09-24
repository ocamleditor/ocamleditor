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


let _ = Gmisclib.Util.fade_window_enabled := Oe_config.fade_window_enabled

let create_mark_name =
  let count = ref 0 in fun prefix ->
    incr count;
    prefix ^ (string_of_int !count);;

(** window *)
let window widget
    ?type_hint
    ?modal
    ?(decorated=false)
    ?parent
    ?(destroy_on_focus_out=true)
    ?(destroy_child=true)
    ?(fade=false)
    ?(focus=true)
    ?(escape=true)
    ?(border_width=1)
    ?wm_class
    ?(show=true)
    ~x ~y () =
  let window = GWindow.window
      ~decorated
      ?modal
      ~border_width
      ~deletable:true
      ~focus_on_map:focus
      ?type_hint
      ?wm_class
      ~show:false ()
  in
  let ebox = GBin.event_box ~packing:window#add () in
  ebox#add widget;
  let color = Color.set_value 0.38 (`COLOR (window#misc#style#base `NORMAL)) (*(`NAME Preferences.preferences#get.Preferences.pref_bg_color_popup)*) in
  let _ = window#misc#modify_bg [`NORMAL, color] in
  (*let _ = ebox#misc#modify_bg [`NORMAL, `NAME Preferences.preferences#get.Preferences.pref_bg_color_popup] in*)
  if destroy_on_focus_out then
    window#event#connect#after#focus_out ~callback:begin fun _ ->
      if not destroy_child then (ebox#remove widget);
      window#destroy();
      true
    end |> ignore;
  if escape then Gmisclib.Util.esc_destroy_window window;
  window#set_skip_pager_hint true;
  window#set_skip_taskbar_hint true;
  window#set_urgency_hint false;
  Gaux.may parent ~f:(fun parent -> Gaux.may (GWindow.toplevel parent) ~f:(fun x -> window#set_transient_for x#as_window));
  window#set_accept_focus focus;
  if show then begin
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
  end;
  window

(** window_tooltip *)
let window_tooltip widget ?parent ?(fade=false) ~x ~y () =
  let fade = fade && !Gmisclib.Util.fade_window_enabled in
  let window = GWindow.window
      ~decorated:false
      ~kind:`POPUP (*`TOPLEVEL*)
      ~type_hint:`MENU (*`NORMAL*)
      ~border_width:1
      ~show:false ()
  in
  let ebox = GBin.event_box ~packing:window#add () in
  ebox#add widget;
  let module ColorOp = Color in
  let open Preferences in
  (*  let color = ColorOp.set_value 0.62 (`NAME ?? (Preferences.preferences#get.editor_bg_color_popup)) in
      let _ = window#misc#modify_bg [`NORMAL, color] in
      let _ = ebox#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)] in*)
  window#set_skip_pager_hint true;
  window#set_skip_taskbar_hint true;
  window#set_accept_focus false;
  window#misc#set_can_focus false;
  window#set_focus_on_map false;
  Gaux.may parent ~f:(fun parent -> Gaux.may (GWindow.toplevel parent) ~f:(fun x -> window#set_transient_for x#as_window));
  if fade then (window#set_opacity 0.0);
  window#move ~x ~y;
  if fade then (Gmisclib.Util.fade_window window) else window#show();
  if Sys.os_type <> "Win32" then (window#present());
  let alloc = window#misc#allocation in
  let x, y =
    (if x + alloc.Gtk.width > (Gdk.Screen.width()) then (Gdk.Screen.width() - alloc.Gtk.width) else x),
    (if y + alloc.Gtk.height > (Gdk.Screen.height()) then (Gdk.Screen.height() - alloc.Gtk.height) else y);
  in
  window#move ~x ~y;
  window

(** with_tag *)
let with_tag ~(buffer : GText.buffer) tag f =
  let m1 = buffer#create_mark(* ~name:(create_mark_name "Gtk_util.with_tag")*) (buffer#get_iter `INSERT) in
  f ();
  buffer#apply_tag tag ~start:(buffer#get_iter_at_mark (`MARK m1)) ~stop:(buffer#get_iter `INSERT);
  buffer#delete_mark (`MARK m1);;

(** increase_font_size *)
let increase_font_size ?weight ?(increment=3) widget =
  let fd = widget#misc#pango_context#font_description in
  if increment <> 0 then begin
    let size = Pango.Font.get_size fd + increment * Pango.scale in
    if size >= 0 then begin
      Pango.Font.modify fd ?weight ~size ();
      widget#misc#modify_font fd;
    end;
  end;
  fd;;

(** try_font *)
let try_font context family =
  try
    let fd = Printf.kprintf Pango.Font.from_string "%s 9" family in
    let _ = Pango.Context.load_font context#as_context fd in
    true
  with Gpointer.Null -> false

let label_icon ?(width=20) ?(height=16) ?(font_name="FiraCode Nerd Font Propo") ?color ?packing icon =
  let markup = Printf.sprintf "<big>%s</big>" icon in
  let label = GMisc.label ~xalign:0.5 ~yalign:0.5 ~xpad:0 ~ypad:0 ~width ~height ~markup ?packing () in
  label#misc#modify_font_by_name font_name;
  color |> Option.iter begin fun color ->
    label#misc#modify_fg [ `NORMAL, `NAME color ];
  end;
  label

class button_icon ?label ?(icon="") ?(icon_spacing=3) ?icon_width ?icon_height ?relief ?packing () =
  let button = GButton.button ?label ?relief ?packing () in
  let hbox = GPack.hbox ~spacing:icon_spacing ~packing:button#add () in
  let icon = label_icon ?width:icon_width ?height:icon_height ~packing:hbox#add icon in
  let label = GMisc.label ~packing:hbox#add () in
  object (self)
    inherit GObj.widget hbox#as_widget
    method set_icon = icon#set_label
    method set_label = label#set_label
    method button = button
  end


