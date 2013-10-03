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

module type ENTRY = sig
  type key
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
end

module Make (Entry : ENTRY) = struct

  type t = (Entry.key, entry) Hashtbl.t

  and entry = {
    key                  : Entry.key;
    node                 : Entry.t;
    mutable dependants   : entry list;
    mutable dependencies : entry list;
  }

  let length = Hashtbl.length

  let set_dependants (dag : t) =
    Hashtbl.iter begin fun _ entry ->
      List.iter begin fun node ->
        node.dependants <- entry :: node.dependants
      end entry.dependencies
    end dag

  let get_leaves : t -> entry list =
    fun dag ->
      Hashtbl.fold begin fun _ entry acc ->
        if entry.dependencies = [] then entry :: acc else acc
      end dag [];;

  let remove_leaf : t -> entry -> unit =
    fun dag leaf ->
      if leaf.dependencies <> [] then failwith "Not a leaf"
      else begin
        Hashtbl.iter begin fun _ entry ->
          entry.dependencies <- List.filter (fun d -> d.key <> leaf.key) entry.dependencies;
        end dag;
        Hashtbl.remove dag leaf.key;
      end;;
end

