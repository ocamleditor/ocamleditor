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


let letters = [
  "'a\\([0-9]*\\)", "\xCE\xB1\\1";
  "'b\\([0-9]*\\)", "\xCE\xB2\\1";
  "'c\\([0-9]*\\)", "\xCE\xB3\\1";
  "'d\\([0-9]*\\)", "\xCE\xB4\\1";
  "'e\\([0-9]*\\)", "\xCE\xB5\\1";
  "'f\\([0-9]*\\)", "\xCE\xB6\\1";
  "'g\\([0-9]*\\)", "\xCE\xB7\\1";
  "'h\\([0-9]*\\)", "\xCE\xB8\\1";
  "'i\\([0-9]*\\)", "\xCE\xB9\\1";
  "'j\\([0-9]*\\)", "\xCE\xBA\\1";
  "'k\\([0-9]*\\)", "\xCE\xBB\\1";
  "'l\\([0-9]*\\)", "\xCE\xBC\\1";
  "'m\\([0-9]*\\)", "\xCE\xBD\\1";
  "'n\\([0-9]*\\)", "\xCE\xBE\\1";
  "'o\\([0-9]*\\)", "\xCE\xBF\\1";
  "'p\\([0-9]*\\)", "\xCE\xC0\\1";
  "'q\\([0-9]*\\)", "\xCE\xC1\\1";
  "'r\\([0-9]*\\)", "\xCE\xC2\\1";
  "'s\\([0-9]*\\)", "\xCE\xC3\\1";
  "'t\\([0-9]*\\)", "\xCE\xC4\\1";
  "'u\\([0-9]*\\)", "\xCE\xC5\\1";
  "'v\\([0-9]*\\)", "\xCE\xC6\\1";
  "'w\\([0-9]*\\)", "\xCE\xC7\\1";
  "'x\\([0-9]*\\)", "\xCE\xC8\\1";
  "'y\\([0-9]*\\)", "\xCE\xC9\\1";
];;


let greek = Miscellanea.replace_all ~regexp:true letters

let replace (buffer : GText.buffer) =
  (*let tag = buffer#create_tag [`SCALE `X_LARGE] in*)
  let replace x y =
    let iter = ref buffer#start_iter in
    while not !iter#is_end do
      match !iter#forward_search x with
        | Some (start, stop) ->
          let mstart = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Greek.replace1")*) start in
          buffer#delete ~start ~stop;
          iter := buffer#get_iter_at_mark (`MARK mstart);
          buffer#insert ~iter:!iter y;
          (*buffer#apply_tag tag
            ~start:(buffer#get_iter_at_mark (`MARK mstart))
            ~stop:((buffer#get_iter_at_mark (`MARK mstart))#forward_chars (Glib.Utf8.length y));*)
          iter := !iter#forward_char;
          buffer#delete_mark (`MARK mstart);
        | None -> iter := buffer#end_iter
    done;
  in
  let iter = ref buffer#start_iter in
  while not !iter#is_end do
    match !iter#forward_search "'" with
      | Some (start, stop) ->
        let mstart = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Greek.replace2")*) start in
        let mstop = ref mstart in
        iter := !iter#forward_char;
        begin
          match String.get (buffer#get_text ~start:!iter ~stop:!iter#forward_char ()) 0 with
            | 'a'..'z' ->
              begin
                iter := !iter#forward_char;
                let stop = !iter#forward_char in
                let text = buffer#get_text ~start:!iter ~stop () in
                if String.length text > 0 then begin
                  match String.get text 0 with
                    | '0'..'9' -> mstop := buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Greek.replace3")*) stop
                    | _ -> mstop := buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Greek.replace4")*) !iter
                end else mstop := buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Greek.replace5")*) stop
              end;
              let start = buffer#get_iter_at_mark (`MARK mstart) in
              let stop = buffer#get_iter_at_mark (`MARK !mstop) in
              let letter = buffer#get_text ~start ~stop () in
              if letter <> "" then begin
                let repl = greek letter in
                buffer#delete ~start ~stop;
                buffer#insert ~iter:(buffer#get_iter_at_mark (`MARK mstart)) repl;
                iter := buffer#get_iter_at_mark (`MARK !mstop);
              end
            | x ->
              if not (GtkText.Mark.get_deleted mstart) then (buffer#delete_mark (`MARK mstart));
        end;
        if not (GtkText.Mark.get_deleted mstart) then (buffer#delete_mark (`MARK mstart));
        if not (GtkText.Mark.get_deleted !mstop) then (buffer#delete_mark (`MARK !mstop));
      | None -> iter := buffer#end_iter
  done;
  replace "->" "\xE2\x86\x92";
  replace " * " " \xE2\xA8\xAF ";
;;
