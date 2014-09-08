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
open Miscellanea

type t = {
  mutable rc_filename     : string;
  mutable rc_title        : string;
  mutable rc_company      : string;
  mutable rc_product      : string;
  mutable rc_copyright    : string;
  mutable rc_file_version : (int * int * int * int);
  mutable rc_icons        : string list;
  (*mutable rc_icons_data   : (string * Buffer.t) list*)
}

(** create *)
let create ?(filename="") ?(title="") ?(company="") ?(product="")
    ?(copyright=(sprintf "© %d" ((Unix.localtime (Unix.gettimeofday())).Unix.tm_year + 1900)))
    ?(file_version=(1,0,0,0)) ?(icons=[]) (*?(icons_data=[])*) () =
  {
    rc_filename     = filename;
    rc_title        = title;
    rc_company      = company;
    rc_product      = product;
    rc_copyright    = copyright;
    rc_file_version = file_version;
    rc_icons        = icons;
    (*rc_icons_data   = icons_data;*)
  }

