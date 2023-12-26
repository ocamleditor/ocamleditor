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

open Printf

type t = {
  mutable locked : (int * GMisc.label) list;
  mutable free : GMisc.label list;
  mutable max_width : int;
}

(** create *)
let create () = {
  free = [];
  locked = [];
  max_width = 0;
}

(** print *)
let print ~view ~x ~y ~num ~width_chars lnl =
  let text = string_of_int num in
  let label = match lnl.free with
    | label :: tl ->
      lnl.free <- tl;
      label#set_text text;
      label
    | [] ->
      let label = GMisc.label ~xalign:1.0 ~yalign:0.5 ~text ~show:false () in
      label#misc#modify_fg [`NORMAL, view#gutter.Gutter.fg_color];
      label#misc#modify_font_by_name view#options#line_numbers_font;
      view#add_child_in_window ~child:label#coerce ~which_window:`LEFT ~x ~y;
      label
  in
  (match List_opt.assoc y lnl.locked with Some x -> x#misc#hide() | _ -> ());
  lnl.locked <- (y, label) :: lnl.locked;
  label#set_width_chars width_chars;
  label#misc#show();
  let width = max label#misc#allocation.Gtk.width lnl.max_width in
  view#move_child ~child:label#coerce ~x:(x - width) ~y;
  if width > lnl.max_width then (lnl.max_width <- width)

(** iter *)
let iter f lnl =
  List.iter f lnl.free;
  List.iter (*f*) (fun (_, labl) -> f labl) lnl.locked

(** reset *)
let reset lnl =
  let lck = snd (List.split lnl.locked) in
  lnl.free <- List.rev_append lck lnl.free;
  List.iter (fun l -> l#misc#hide()) lnl.free;
  lnl.locked <- []

(** hide *)
let hide y lnl =
  match List_opt.assoc y lnl.locked with Some lnl -> lnl#misc#hide() | _ -> ()
