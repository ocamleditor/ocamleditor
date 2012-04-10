(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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

type location = Mark of Gtk.text_mark | Offset of int

type t = {
  num : int;
  filename : string;
  mutable loc : location;
  mutable marker : Gutter.marker option;
}

let bookmarks_filename = Filename.concat Oe_config.ocamleditor_user_home "bookmarks-1.5.0"

let bookmarks = (*new GUtil.variable*) ref []

let limit = 1000

let icon = function
  | 0 -> Some Icons.b0
  | 1 -> Some Icons.b1
  | 2 -> Some Icons.b2
  | 3 -> Some Icons.b3
  | 4 -> Some Icons.b4
  | 5 -> Some Icons.b5
  | 6 -> Some Icons.b6
  | 7 -> Some Icons.b7
  | 8 -> Some Icons.b8
  | 9 -> Some Icons.b9
  | _ -> None;;

(** Read bookmarks at startup *)
let _ = begin
  if Sys.file_exists bookmarks_filename then begin
    let inchan = open_in_gen [Open_creat] 0o666 bookmarks_filename in
    set_binary_mode_in inchan false;
    try
      begin try
        while true do
          match Miscellanea.split "," (input_line inchan) with
            | [filename; offset; num] ->
              let num = int_of_string num in
              let bm = {
                filename = filename;
                loc = Offset (int_of_string offset);
                num = num;
                marker = None;
              } in
              bookmarks := bm :: !bookmarks;
            | _ -> assert false
        done;
      with End_of_file -> ()
      end;
      close_in inchan;
    with ex -> begin
      close_in inchan;
      Printf.printf "%s\n" (Printexc.to_string ex)
    end
  end
end

(** remove *)
let rec remove ~num =
  try
    let bm = List.find (fun x -> x.num = num) !bookmarks in
    begin
      match bm.loc with
        | Mark mark ->
          begin
            match GtkText.Mark.get_buffer mark with
              | None -> ()
              | Some buffer ->
                GtkText.Buffer.delete_mark buffer mark;
          end
        | _ -> ()
    end;
    bm.loc <- (Offset 0);
    (*bookmarks#set [];*)
    bookmarks := (List.filter (fun x -> x.num <> num) !bookmarks);
    write()
  with Not_found -> ()

(** apply *)
and apply bm f =
  match bm.loc with
    | Mark mark ->
      begin
        match GtkText.Mark.get_buffer mark with
          | None ->
            remove ~num:bm.num;
            invalid_arg "bookmark"
          | Some buffer ->
            let iter = Gmisclib.Util.get_iter_at_mark_safe buffer mark in
            f (`ITER iter)
      end
    | Offset offset -> f (`OFFSET offset)

(** write *)
and write () =
  let outchan = open_out bookmarks_filename in
  try
    set_binary_mode_out outchan false;
    List.iter begin fun bm ->
      let print offset = fprintf outchan "%s,%d,%d\n" bm.filename offset bm.num in
      try
        apply bm begin function
          | `ITER iter -> print (GtkText.Iter.get_offset iter)
          | `OFFSET offset -> print offset
        end
      with Invalid_argument "bookmark" -> ()
    end !bookmarks;
    close_out outchan
  with ex -> begin
    close_out outchan;
    raise ex
  end

(** mark_to_offset *)
let mark_to_offset bm =
  apply bm begin function
    | `ITER iter ->
      let loc = Offset (GtkText.Iter.get_offset iter) in
      begin
        match bm.loc with
          | Mark m ->
            begin
              match GtkText.Mark.get_buffer m with
                | None -> assert false
                | Some buffer -> GtkText.Buffer.delete_mark buffer m;
            end
          | _ -> assert false
      end;
      bm.loc <- loc;
    | `OFFSET _ -> ()
  end

(** offset_to_mark *)
let offset_to_mark (buffer : GText.buffer) bm =
  match bm.loc with
    | Mark mark -> mark
    | Offset offset ->
      let offset = min offset buffer#char_count in
      let mark = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Bookmark.offset_to_mark")*) (buffer#get_iter (`OFFSET offset)) in
      bm.loc <- (Mark mark);
      mark

(** find *)
let find filename buffer iter =
  try
    Some (List.find begin fun bm ->
      if bm.filename = filename then begin
        let mark = offset_to_mark buffer bm in
        iter#line = (buffer#get_iter (`MARK mark))#line
      end else false
    end !bookmarks)
  with Not_found -> None
;;

(** actual_maximum *)
let actual_maximum () =
  List.fold_left begin fun acc bm ->
    max acc bm.num
  end 0 !bookmarks;;

(** create *)
let create ~num ~filename ~mark ~marker () =
  remove ~num;
  let bm = {
    filename = filename;
    loc = Mark mark;
    num = num;
    marker = Some marker;
  } in
  bookmarks := (bm :: !bookmarks);
  write()
