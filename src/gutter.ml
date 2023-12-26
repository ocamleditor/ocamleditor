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


(*

 S | chars (varying) | S | B | B | Folding | B | B |

*)

open Printf

type t = {
  mutable size            : int;
  mutable chars           : int;
  mutable start_selection : GText.iter option;
  spacing                 : int;
  mutable fold_size       : int;
  mutable fold_x          : int;
  mutable bg_color        : GDraw.color;
  mutable fg_color        : GDraw.color;
  mutable border_color    : GDraw.color;
  mutable marker_color    : GDraw.color;
  mutable marker_bg_color : GDraw.color;
  mutable markers         : marker list;
}
and marker = {
  kind                    : [`None | `Bookmark of int | `Error of string | `Warning of string];
  mark                    : Gtk.text_mark;
  icon_pixbuf             : GdkPixbuf.pixbuf option;
  mutable icon_obj        : GObj.widget option;
  callback                : (Gtk.text_mark -> bool) option;
}

let icon_size = 15

(** create *)
let create () = {
  size            = 0;
  chars           = 0;
  start_selection = None;
  spacing         = 2;
  fold_size       = 0;
  fold_x          = (-1);
  bg_color        = `WHITE;
  fg_color        = `WHITE;
  border_color    = `WHITE;
  marker_color    = `WHITE;
  marker_bg_color = `WHITE;
  markers         = [];
}

(** create_marker *)
let create_marker ?(kind=`None) ~mark ?pixbuf ?callback () =
  {kind=kind; mark=mark; icon_pixbuf=pixbuf; callback=callback; icon_obj=None}

(** destroy_markers *)
let destroy_markers gutter markers =
  gutter.markers <- List.filter (fun x -> not (List.memq x markers)) gutter.markers;
  List.iter begin fun marker ->
    Gaux.may marker.icon_obj ~f:(fun i -> i#destroy());
    match GtkText.Mark.get_buffer marker.mark with
    | None -> ()
    | Some buffer ->
        GtkText.Buffer.delete_mark buffer marker.mark;
  end markers




