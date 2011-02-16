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

open Printf
open Oe_config

let glib_is_utf8, glib_charset = Glib.Convert.get_charset()

let locale_is_utf8, locale_charset =
  if true || Sys.os_type = "Win32" then glib_is_utf8, glib_charset else begin
    let charset = Miscellanea.expand ~first_line:true "locale -c LC_CTYPE | head -6 | tail -1" in
    (charset = "UTF-8"), charset
  end

let is_utf8, default_charset = locale_is_utf8, locale_charset

let to_utf8 = Glib.Convert.convert ~from_codeset:default_charset ~to_codeset:"utf8"
let from_utf8 = Glib.Convert.convert ~from_codeset:"utf8" ~to_codeset:default_charset

let offset_from_pos text ~pos =
  if Glib.Utf8.validate text then (Glib.Utf8.length (String.sub text 0 pos)) else pos

let offset_to_pos = Glib.Utf8.offset_to_pos









