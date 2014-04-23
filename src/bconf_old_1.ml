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

(** Deprecated *)

let filename = "oebuild.conf"

type t = {
  filename : string;
  mutable env : string;
  mutable opt : bool;
  mutable libs : string;
  mutable mods : string;
  mutable includes : string;
  mutable thread : bool;
  mutable vmthread : bool;
  mutable cflags : string;
  mutable lflags : string;
  mutable libname : string option;
  mutable run : string;
}
