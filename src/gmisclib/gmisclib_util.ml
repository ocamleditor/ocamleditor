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


open Printf

exception Mark_deleted

let fade_window_enabled = ref false

(** fade_window *)
let fade_window =
  if !fade_window_enabled then begin
    (*fun ?(incr=0.159) ?(stop=0.96) window ->*)
    fun ?(incr=0.25) ?(stop=1.0) window ->
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
  end else (fun ?incr ?stop window -> window#set_opacity 1.0; window#show())

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

let get_iter_at_mark_opt buffer mark =
  (*try*)
    if GtkText.Mark.get_deleted mark then None
    else Some (GtkText.Buffer.get_iter_at_mark buffer mark)
  (*with ex ->
    Printf.eprintf "File \"gtk_util.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    raise ex*)

(** set_tag_paragraph_background *)
let set_tag_paragraph_background (tag : GText.tag) =
  Gobject.Property.set tag#as_tag {Gobject.name="paragraph-background"; conv=Gobject.Data.string}

(** treeview_is_path_onscreen *)
let treeview_is_path_onscreen (view : GTree.view) path =
  let rect = view#get_cell_area ~path () in
  let y = float (Gdk.Rectangle.y rect) in
  0. <= y && y <= view#vadjustment#page_size;;













