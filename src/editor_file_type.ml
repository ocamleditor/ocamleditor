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

type remote_login = {
  host             : string;
  user             : string;
  pwd              : string;
  sslkey           : string;
  sshpublickeyfile : string;
  sslkeypasswd     : string;
}

type stats = {
  perm  : string;
  size  : int;
  mtime : float;
}

class type abstract_file  =
  object
    method filename : string
    method set_filename : string -> unit
    method dirname : string
    method basename : string
    method changed : bool
    method last_modified : unit -> float
    method exists : bool
    method is_readonly : bool
    method is_writeable : bool
    method read : string
    method write : string -> unit
    method list : unit -> string list
    method rename : string -> unit
    method remove : unit
    method remote : remote_login option
    method backup : ?move_to:string -> unit -> string
    method cleanup : unit -> unit
    method stat : unit -> stats option
  end
