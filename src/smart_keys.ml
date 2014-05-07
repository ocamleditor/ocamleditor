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


open Text_util

let smart_home ~view state =
  let buffer : GText.buffer = view#buffer in
  let iter = buffer#get_iter `INSERT in
  if iter#chars_in_line > 1 then begin
    if view#options#smart_home then begin
      let prev = iter#copy in
      let where = iter#set_line_offset 0 in
      let where = if Glib.Unichar.isspace where#char then begin
        let first_non_blank = where#forward_find_char not_blank in
        if first_non_blank#line <> iter#line then begin
          if iter#line_index > 0 then where else iter#forward_to_line_end
        end else first_non_blank
      end else where in
      buffer#move_mark `INSERT ~where;
      if state <> [`SHIFT] then buffer#move_mark `SEL_BOUND ~where;
      prev#compare where <> 0
    end else if iter#starts_line && (not view#options#smart_home) then begin
      let where = iter#forward_find_char not_blank in
      buffer#move_mark `INSERT ~where;
      if state <> [`SHIFT] then buffer#move_mark `SEL_BOUND ~where;
      true
    end else false;
  end else false;;

let smart_end ~view state =
  let buffer : GText.buffer = view#buffer in
  let backward_non_blank iter =
    let rec f it =
      let stop = it#backward_char in
      if List.for_all ((<>) (it#get_text ~stop)) [" "; "\t"; "\r"] then it
      else f stop
    in
    f iter
  in
  let iter = buffer#get_iter `INSERT in
  if iter#ends_line then begin
    let where = backward_non_blank iter in
    buffer#move_mark `INSERT ~where;
    if state <> [`SHIFT] then buffer#move_mark `SEL_BOUND ~where;
    true
  end else if not iter#ends_line && view#options#smart_end then begin
    let prev = iter#copy in
    let where = backward_non_blank iter#forward_to_line_end in
    buffer#move_mark `INSERT ~where;
    if state <> [`SHIFT] then buffer#move_mark `SEL_BOUND ~where;
    prev#compare where <> 0
  end else false;;
