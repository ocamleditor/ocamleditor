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


exception Not_started

type t

val channels : t -> in_channel * out_channel * in_channel

val close : t -> unit

val kill : t -> unit

val start : t -> unit

val getpid : t -> int

val cmd_line : t -> string

val create :
  ?at_exit:(unit -> unit) ->
  ?env:string array -> prog:string -> ?args:string list -> unit -> t
