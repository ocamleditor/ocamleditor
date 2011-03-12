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

let re_let = Str.regexp_case_fold "^[\t ]*\\(\\(let\\)\\|\\(mutable\\)\\)?[\t ]*\\([a-z_0-9']+\\)[\t ]*\\([:=]+\\).*";;

(** iter *)
let iter ~(start : GText.iter) ~stop f =
  let iter = ref start in
  while !iter#compare stop < 0 do
    let line = !iter#get_text ~stop:!iter#forward_to_line_end in
    if Str.string_match re_let line 0 then begin
      let w1 = Str.matched_group 4 line in
      let w2 = Str.matched_group 5 line in
      match !iter#forward_search w1 with
        | Some (a, b) ->
          begin
            match b#forward_search w2 with
              | Some (c, d) -> f a b c d;
              | _ -> ()
          end
        | _ -> ()
    end;
    iter := !iter#forward_line;
  done

(** collapse *)
let collapse ~(view : GText.view) ~start ~stop =
  let ranges = ref [] in
  iter start stop begin fun _ b c _ ->
    ranges := (b#line, b#line_index, c#line, c#line_index - 1) :: !ranges;
  end;
  List.iter begin fun (a, b, c, d) ->
    view#buffer#delete
      ~start:(view#buffer#get_iter (`LINECHAR (a, b)))
      ~stop:(view#buffer#get_iter (`LINECHAR (c, d)))
  end !ranges

(** align_selection *)
let align_selection ~(view : GText.view) =
  let buffer = view#buffer in
  if buffer#has_selection then begin
    let top, bottom = buffer#selection_bounds in
    let top = top#set_line_index 0 in
    let bottom = bottom#set_line_index 0 in
    let max_width = ref 0 in
    iter top bottom begin fun a b c d ->
      let width = c#line_index - a#line_index in
      if width > !max_width then (max_width := width)
    end;
    let inserts = ref [] in
    iter top bottom begin fun a b c d ->
      let width = c#line_index - a#line_index in
      if !max_width > width then
        (inserts := (c#line, c#line_index, (String.make (!max_width - width) ' ')) :: !inserts)
    end;
    if !inserts = [] then (collapse view top bottom) else begin
      List.iter begin fun (line, index, spaces) ->
        buffer#insert ~iter:(buffer#get_iter (`LINECHAR (line, index))) spaces
      end !inserts;
    end
  end






