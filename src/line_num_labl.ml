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
