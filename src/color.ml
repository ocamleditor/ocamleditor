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

let set_value value =
  let value = if value < 0. then 0.
    else if value > 1.0 then 1.
    else value
  in fun color ->
    let gdk = GDraw.color color in
    `RGB (
      (int_of_float (float (Gdk.Color.red gdk) *. value)),
      (int_of_float (float (Gdk.Color.green gdk) *. value)),
      (int_of_float (float (Gdk.Color.blue gdk) *. value))
    )

let name = function `RGB (red, green, blue) -> Printf.sprintf "#%X%X%X" red green blue

let name_of_gdk color =
  let r = Gdk.Color.red color in
  let g = Gdk.Color.green color in
  let b = Gdk.Color.blue color in
  name (`RGB (r, g, b))


