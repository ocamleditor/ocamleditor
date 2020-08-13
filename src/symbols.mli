(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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

val get_parent_path : Oe.symbol -> string list
val get_name : Oe.symbol -> string
val get_module_name : Oe.symbol -> string
val is_method : Oe.symbol -> bool
val concat_value_path : Oe.symbol -> string
val split_value_path : string -> string list
val string_of_id : string list -> string

module Modules :
  sig
    type t = {
      mo_name     : string;
      mo_filename : string;
      mo_descr    : string;
    }
    val read :
      ?filter:string -> path:string list -> unit -> (string * string) list
    val get_descr : project:Prj.t -> t list
  end

module Cache :
  sig
    val reset : project:Prj.t -> unit
    val update :
      cache:Oe.symbol_cache -> value_path:string list -> unit -> bool
    val save : project:Prj.t -> unit
    val load : project:Prj.t -> unit
  end

val find_by_modulepath :
  ?kind:Oe.symbol_kind list ->
  Oe.symbol_cache -> string list -> Oe.symbol option
val filter_methods : Oe.symbol_cache -> string list -> Oe.symbol list
val find_local_defs :
  regexp:Str.regexp ->
  project:Prj.t ->
  filename:string -> offset:int -> Oe.symbol list
val filter_by_name :
  ?use_longidents:bool ->
  ?include_locals:Prj.t * string * int ->
  ?include_methods:bool ->
  ?include_modules:string list ->
  regexp:Str.regexp -> Oe.symbol list -> Oe.symbol list
val filter_by_type : regexp:Str.regexp -> Oe.symbol list -> Oe.symbol list
val filter_by_modulepath :
  ?update_cache:bool -> Oe.symbol_cache -> string list -> Oe.symbol list
val find_parent :
  Oe.symbol_cache -> ?update_cache:bool -> Oe.symbol -> Oe.symbol option
