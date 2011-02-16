(*

  OCamlEditor
  Copyright (C) 2010, 2011 Francesco Tovagliari

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

type level = Warning of int | Error

type message = {
  er_filename : string;
  er_level : level;
  er_line : int;
  er_characters : int * int;
  er_location : string;
  er_message : string;
  er_inconsistent_assumptions : string option;
}

type t = {
  er_warnings : message list;
  er_errors : message list;
}

let re_inconsistent_assumptions = Str.regexp
  ".*make[ \t\r\n]+inconsistent[ \t\r\n]+assumptions[ \t\r\n]+over[ \t\r\n]+\\(interface\\|implementation\\)[ \t\r\n]+\\([^ \t\r\n]+\\)[ \t\r\n]*"

















