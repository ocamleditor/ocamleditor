(*

  OCamlEditor
  Copyright (C) 2010-2013 Francesco Tovagliari

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


(** GeometryMemo *)
module GeometryMemo = struct
  type t = {
    mutable enabled : bool;
    mutable delayed : bool;
    filename        : string;
    table           : (string, (int * int * int * int)) Hashtbl.t;
  }

  let create ?(enabled=true) ?(delayed=false) ~filename () =
    let ichan = open_in_gen [Open_binary; Open_creat] 0o777 filename in
    let table =
    try
      begin
        try
          let (table : ((string, (int * int * int * int)) Hashtbl.t)) = input_value ichan in
          table
        with End_of_file -> Hashtbl.create 7
      end;
    with ex ->
      close_in ichan;
      raise ex
    in
    { enabled; delayed; filename; table; }

  let set_enabled memo x = memo.enabled <- x

  let set_delayed memo x = memo.delayed <- x

  let add ~(window : GWindow.window) ~key memo =
    if memo.enabled then begin
      window#set_gravity `STATIC;
      let write memo =
        let dump = memo.table in
        let ochan = open_out_bin memo.filename in
        try
          output_value ochan dump;
          close_out ochan;
        with ex ->
          close_out ochan;
          raise ex
      in
      ignore (window#misc#connect#show ~callback:begin fun () ->
          try
            let x, y, width, height = Hashtbl.find memo.table key in
            let move_resize () =
              window#move ~x ~y;
              window#resize ~width ~height;
            in
            if memo.delayed then begin
              window#resize ~width:1 ~height:1;
              Gmisclib_util.idle_add move_resize
            end else (move_resize());
          with Not_found -> ();
        end);
      ignore (window#misc#connect#hide ~callback:begin fun () ->
          try
            let rect = window#misc#allocation in
            let x, y = Gdk.Window.get_position window#misc#window in
            Hashtbl.replace memo.table key (x, y, rect.Gtk.width, rect.Gtk.height);
            write memo;
          with Gpointer.Null -> ()
        end)
    end;;

end

(** popup *)
class popup ?(position=(`SOUTH:[`NORTH | `SOUTH | `POINTER])) ?border_width ?(decorated=false) ~widget () =
  let popup = GWindow.window(* ~kind:`POPUP*) ~type_hint:`UTILITY ~decorated ~focus_on_map:true ~modal:false ~deletable:true ~border_width:1 ~title:"" () in
  let ebox = GBin.event_box ~packing:popup#add () in
  let vbox = GPack.vbox ?border_width ~packing:ebox#add () in
  (*let _ = popup#misc#modify_bg [`NORMAL, `COLOR (ebox#misc#style#bg `ACTIVE)] in *)
  let _ = popup#misc#modify_bg [`NORMAL, `COLOR (ebox#misc#style#base `ACTIVE)] in
object (self)
  val mutable on_popdown = fun () -> ()
  initializer
    popup#set_destroy_with_parent true;
    ignore (popup#event#connect#after#focus_out ~callback:begin fun ev ->
      self#popdown();
      false
    end);

  method set_on_popdown f = on_popdown <- f

  method popdown () =
    if popup#misc#get_flag `VISIBLE then begin
      on_popdown();
      popup#misc#hide();
    end;

  method present () =
    popup#present();
    let x0, y0 = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
    let x, y = match position with
      | `POINTER -> x0, y0
      | _ ->
        let x, y = widget#misc#toplevel#misc#pointer in
        let alloc = widget#misc#allocation in
        let alloc_popup = popup#misc#allocation in
        let x = x0 - x + alloc.Gtk.x in
        let y =
          if position = `SOUTH
          then y0 - y + alloc.Gtk.y + alloc.Gtk.height
          else y0 - y + alloc.Gtk.y - alloc_popup.Gtk.height
        in
        let x, y =
          (if x + alloc_popup.Gtk.width > (Gdk.Screen.width()) then (Gdk.Screen.width() - alloc_popup.Gtk.width - 3) else x),
          (if y + alloc_popup.Gtk.height > (Gdk.Screen.height()) then (Gdk.Screen.height() - alloc_popup.Gtk.height - 3) else y);
        in x, y
    in
    popup#move ~x ~y;


  method add = vbox#add
  method set_transient_for = popup#set_transient_for
  method set_modal = popup#set_modal
  method event = popup#event
  method misc = popup#misc
  method move = popup#move
  method resize = popup#resize
end
