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

let cursor : (Gdk.Cursor.cursor_type -> Gdk.cursor) ref = ref (fun _ -> failwith "cursor")

let init () =
  cursor :=
    let arrow = Gdk.Cursor.create `ARROW in
    let xterm = Gdk.Cursor.create `XTERM in
    let hand1 = Gdk.Cursor.create `HAND1 in
    function
      | `ARROW -> arrow
      | `XTERM -> xterm
      | `HAND1 -> hand1
      | _ -> assert false

(** fade_window *)
let fade_window =
  if Oe_config.fade_window_enabled then begin
    fun ?(incr=0.159) ?(stop=0.96) window ->
      window#set_opacity 0.0;
      window#show();
      let callback =
        let opa = ref 0.0 in fun () ->
        window#set_opacity !opa;
        opa := !opa +. incr;
        !opa <= stop;
      in
      ignore (callback());
      ignore (GMain.Timeout.add ~ms:20 ~callback)
  end else (fun ?incr ?stop window -> window#show())

(** idle_add_gen *)
let idle_add_gen ?prio f = GMain.Idle.add ?prio begin fun () ->
  try f ()
  with ex -> (eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())); false
end

(** idle_add *)
let idle_add ?prio (f : unit -> unit) = ignore (GMain.Idle.add ?prio begin fun () ->
  try f (); false
  with ex -> (eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())); false
end)

(** get_iter_at_mark_safe *)
let get_iter_at_mark_safe buffer mark =
  (*try*)
    if GtkText.Mark.get_deleted mark then (raise Mark_deleted)
    else (GtkText.Buffer.get_iter_at_mark buffer mark)
  (*with ex ->
    Printf.eprintf "File \"gtk_util.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    raise ex*)

(** set_tag_paragraph_background *)
let set_tag_paragraph_background (tag : GText.tag) =
  Gobject.Property.set tag#as_tag {Gobject.name="paragraph-background"; conv=Gobject.Data.string}

(** set_ebox_invisible *)
let set_ebox_invisible (ebox : GBin.event_box) =
  Gobject.Property.set ebox#as_widget {Gobject.name="visible-window"; conv=Gobject.Data.boolean} false

(** treeview_is_path_onscreen *)
let treeview_is_path_onscreen (view : GTree.view) path =
  let rect = view#get_cell_area ~path () in
  let y = float (Gdk.Rectangle.y rect) in
  0. <= y && y <= view#vadjustment#page_size;;

(** window *)
let window widget ~parent ?(destroy_child=true) ~x ~y () =
  let window = GWindow.window
    ~decorated:false
    ~border_width:1
    ~deletable:true
    ~focus_on_map:true
    ~show:false ()
  in
  let ebox = GBin.event_box ~packing:window#add () in
  ebox#add widget;
  let color = Color.set_value 0.80 (`NAME !Preferences.preferences.Preferences.pref_bg_color_popup) in
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
  Gaux.may (GWindow.toplevel parent) ~f:(fun x -> window#set_transient_for x#as_window);
  window#set_accept_focus true;
  window#set_opacity 0.0;
  window#present();
  window#move ~x ~y;
  let alloc = window#misc#allocation in
  let x, y =
    (if x + alloc.Gtk.width > (Gdk.Screen.width()) then (Gdk.Screen.width() - alloc.Gtk.width) else x),
    (if y + alloc.Gtk.height > (Gdk.Screen.height()) then (Gdk.Screen.height() - alloc.Gtk.height) else y);
  in
  window#move ~x ~y;
  fade_window window;
  window













