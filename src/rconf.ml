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


type t = {
  mutable id          : int;
  mutable id_target   : int;
  mutable name        : string;
  mutable default     : bool;
  mutable build_task  : Target.rbt;
  mutable env         : (bool * string) list;
  mutable env_replace : bool;
  mutable args        : (bool * string) list;
}

let create ~id ~name ~id_target = {
   id          = id;
   id_target   = id_target;
   name        = name;
   default     = false;
   build_task  = `NONE;
   env         = [];
   env_replace = false;
   args        = []
}





