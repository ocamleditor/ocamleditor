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

open Printf

(** iter *)
(*let re_align = Str.regexp_case_fold "^[\t ]*\\(\\(let\\)\\|\\(mutable\\)\\)?[\t ]*\\([a-z_0-9']+\\)[\t ]*\\([:=]+\\).*";;
let iter ~(start : GText.iter) ~stop f =
  let iter = ref start in
  while !iter#compare stop < 0 do
    let line = !iter#get_text ~stop:!iter#forward_to_line_end in
    if Str.string_match re_align line 0 then begin
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
  done*)

let re_align = Str.regexp_case_fold "^[\t ]*\\(\\(let[\t ]+\\)\\|\\(mutable[\t ]+\\)\\)?\\([a-z_0-9']+\\)[\t ]*\\([:=]+\\).*";;
let iter ~(start : GText.iter) ~stop f =
  let iter = ref start in
  while !iter#compare stop < 0 do
    let line = !iter#get_text ~stop:!iter#forward_to_line_end in
    if Str.string_match re_align line 0 then begin
      let prefix = try String.length (Str.matched_group 1 line) with Not_found -> 0 in
      let w1 = Str.matched_group 4 line in
      let w2 = Str.matched_group 5 line in
      match !iter#forward_search w1 with
        | Some (a, b) ->
          begin
            match b#forward_search w2 with
              | Some (c, d) -> f prefix a b c d;
              | _ -> ()
          end
        | _ -> ()
    end;
    iter := !iter#forward_line;
  done

(** collapse *)
let collapse ~(buffer : GText.buffer) ~start ~stop =
  let ranges = ref [] in
  iter start stop begin fun _ _ b c _ ->
    ranges := (b#line, b#line_index, c#line, c#line_index - 1) :: !ranges;
  end;
  List.iter begin fun (a, b, c, d) ->
    buffer#delete
      ~start:(buffer#get_iter (`LINECHAR (a, b)))
      ~stop:(buffer#get_iter (`LINECHAR (c, d)))
  end !ranges

(** align *)
let align ~(buffer : GText.buffer) ~start ~stop =
  let top = start#set_line_index 0 in
  let bottom = stop#set_line_index 0 in
  let max_width = ref 0 in
  iter top bottom begin fun prefix a b c d ->
    let width = c#line_index - a#line_index + prefix in
    if width > !max_width then (max_width := width)
  end;
  let inserts = ref [] in
  iter top bottom begin fun prefix a b c d ->
    let width = c#line_index - a#line_index + prefix in
    if !max_width > width then
      (inserts := (c#line, c#line_index, (String.make (!max_width - width) ' ')) :: !inserts)
  end;
  if !inserts = [] then (collapse buffer top bottom) else begin
    List.iter begin fun (line, index, spaces) ->
      buffer#insert ~iter:(buffer#get_iter (`LINECHAR (line, index))) spaces
    end !inserts;
  end;;

(** indent_matching_previous *)
let indent_matching_previous ~(buffer : GText.buffer) =
  (* Advance to the column that lines up with the start of the next word in
     the previous line. *)
  let ins = buffer#get_iter `INSERT in
  let bol = ins#set_line_index 0 in
  if (bol#forward_find_char (fun x -> x <> 32 && x <> 9))#equal ins then begin
    false
  end else if ins#char = 32 || ins#char = 9 then begin
    buffer#place_cursor ~where:(ins#forward_find_char (fun x -> x <> 32 && x <> 9));
    true
  end else begin
    let prev = ins#backward_line in
    if prev#chars_in_line > ins#line_index then begin
      let prev = prev#set_line_index ins#line_index in
      if prev#line_index >= ins#line_index && not prev#ends_line then begin
        let prev = if prev#line_index = 0 || prev#char = 32 || prev#char = 9 then prev else prev#forward_find_char (fun x -> x = 32 || x = 9) in
        let prev = if prev#line_index = 0 && prev#char <> 32 && prev#char <> 9 then prev else prev#forward_find_char (fun x -> x <> 32 && x <> 9) in
        let length = prev#line_index - ins#line_index in
        if length > 0 then begin
          let spaces = String.make length ' ' in
          buffer#insert spaces;
          true
        end else false
      end else false
    end else false
  end;;

(** indent *)
let indent ~buffer ?(decrease=false) () =
  let mk_spaces length = String.make length (if buffer#tab_spaces then ' ' else '\t') in
  let i1, i2 = buffer#selection_bounds in
  buffer#undo#begin_block ~name:"indent";
  buffer#block_signal_handlers();
  let tbuffer : GText.buffer = buffer#as_gtext_buffer in
  if buffer#has_selection || decrease then begin
    let start, stop = i1#line, i2#line in
    begin
      match decrease with
        | false ->
          let current = ref start in
          let spaces = mk_spaces buffer#tab_width in
          while !current < stop do
            tbuffer#insert ~iter:(tbuffer#get_iter_at_char 0 ~line:!current) spaces;
            incr current;
          done;
          let m = if (tbuffer#get_iter `INSERT)#compare (tbuffer#get_iter `SEL_BOUND) < 0
            then `INSERT else `SEL_BOUND in
          tbuffer#move_mark m ~where:(tbuffer#get_iter_at_char 0 ~line:start);
        | true ->
          let stop = if buffer#has_selection && i2#line_offset = 0 then stop - 1 else stop in
          for line = start to stop do
            let start = tbuffer#get_iter_at_char 0 ~line in
            for j = 1 to buffer#tab_width do
              let c = start#char in
              if c = 32 || c = 9 then tbuffer#delete ~start ~stop:start#forward_char;
            done;
          done;
    end;
  end else begin
    (*let ins = buffer#get_iter `INSERT in*)
    let increase () =
     (* Advance to the next indentation level *)
     let spaces = mk_spaces buffer#tab_width in
     tbuffer#insert spaces;
    in
    (*if ins#ends_line && ins#line_index > 0 then (increase())
    else*) if not (indent_matching_previous ~buffer:tbuffer)
    then increase();
    GtkSignal.stop_emit();
  end;
  buffer#unblock_signal_handlers();
  buffer#undo#end_block();;



















