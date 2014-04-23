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

let scan_indent view (start : GText.iter) (stop : GText.iter) =
  let data = ref [] in
  let iter = ref (start#set_line_index 0) in
  let stop = stop#set_line_index 0 in
  while !iter#compare stop < 0 do
    if List.mem !iter#char blanks then (iter := !iter#forward_find_char not_blank);
    let indent = !iter#line_index / view#tbuffer#tab_width in
    data := (!iter#line, indent) :: !data;
    iter := !iter#forward_line;
  done;
  !data;;

(** draw_indent_lines *)
let draw_indent_lines view (drawable : GDraw.drawable) start stop y0 =
  let left_margin = view#left_margin + 1 in (* +1 per evitare di coprire la linea del cursore *)
  let buffer = view#tbuffer in
  let indents = scan_indent view start stop in
  let count = ref buffer#line_count in
  let prev = ref 0 in
  let lines = ref [] in
  let add_segment ll x y1 y2 =
    match List_opt.assoc x !ll with
      | Some segs ->
        begin
          match !segs with
            | (y3, y4) :: tl when y2 = y3 ->
              segs := (y1, y4) :: tl;
            | _ -> segs := (y1, y2) :: !segs;
        end
      | _ -> (ll := (x, ref [y1, y2]) :: !ll)
  in
  let hline = ref 0 in
  let hadjust = match view#hadjustment with Some adj -> int_of_float adj#value - left_margin | _ -> 0 in
  let draw line indent =
    let iter = buffer#get_iter (`LINE line) in
    let y1, h1 = view#get_line_yrange iter in
    let y1 = y1 - y0 in
    let y2 = y1 + h1 in
    hline := max h1 !hline;
    for i = 1 to indent - 1 do
      let x = view#approx_char_width * (i * buffer#tab_width) - hadjust in
      add_segment lines x y1 y2;
    done;
  in
  List.iter begin fun (line, indent) ->
    if !count > line then begin
      while !count > line do
        draw !count !prev;
        decr count;
      done
    end;
    draw line indent;
    prev := indent;
    decr count;
  end indents;
  (*hline := !hline * 2;*)
  (* Draw lines *)
  let lines1, lines2 = List.partition (fun (x, _) -> (x - left_margin) mod (2 * buffer#tab_width * view#approx_char_width) = 0) !lines in
  drawable#set_foreground view#options#indent_lines_color_solid;
  drawable#set_line_attributes ~width:1 ~style:`SOLID ();
  List.iter begin fun (x, xlines) ->
    List.iter begin fun (y1, y2) ->
      if y2 - y1 > !hline then (drawable#line ~x ~y:y1 ~x ~y:y2)
    end !xlines
  end lines2;
  Gdk.GC.set_dashes drawable#gc ~offset:1 Oe_config.on_off_dashes;
  let color_dashed = view#options#indent_lines_color_dashed in
  List.iter begin fun (x, xlines) ->
    List.iter begin fun (y1, y2) ->
      if y2 - y1 > !hline then begin
        drawable#set_foreground view#options#base_color;
        drawable#set_line_attributes ~width:1 ~style:`SOLID ();
        drawable#line ~x ~y:y1 ~x ~y:y2;
        drawable#set_foreground color_dashed;
        drawable#set_line_attributes ~width:1 ~style:`ON_OFF_DASH ();
        drawable#line ~x ~y:y1 ~x ~y:y2
      end
    end !xlines
  end lines1;
