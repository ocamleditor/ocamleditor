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

type remote_login = {
  host : string;
  user : string;
  pwd  : string;
}

class type abstract_file  =
object
  method filename : string
  method dirname : string
  method basename : string
  method last_modified : unit -> float
  method is_readonly : bool
  method is_writeable : bool
  method changed : bool
  method read : string
  method write : string -> unit
  method backup : ?move_to:string -> unit -> string
  method rename : string -> unit
  method remove : unit
  method exists : bool
  method remote : remote_login option
end
