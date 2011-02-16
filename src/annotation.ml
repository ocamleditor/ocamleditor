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
open Annot_types
open Miscellanea

let table = Hashtbl.create 17

(** get_ref *)
let rec get_ref = function
  | [] -> None
  | x :: y -> (match x with Annot_types.Int_ref _ | Ext_ref _ -> Some x | _ -> get_ref y)

(** get_int_ref *)
let rec get_int_ref = function
  | [] -> None
  | x :: y -> (match x with Annot_types.Int_ref a -> Some a | _ -> get_int_ref y)

(** get_ext_ref *)
let rec get_ext_ref = function
  | [] -> None
  | x :: y -> (match x with Annot_types.Ext_ref a -> Some a | _ -> get_ext_ref y)

(** get_type *)
let rec get_type = function
  | [] -> None
  | x :: y ->
    begin
      match x with
        | Annot_types.Type descr -> Some descr
        | _ -> get_type y
    end

(** get_def *)
let rec get_def = function
  | [] -> None
  | x :: y ->
    begin
      match x with
        | Annot_types.Def a -> Some a
        | _ -> get_def y
    end

(** parse_file *)
let parse_file filename ts =
  try
    if Sys.file_exists filename then begin
      let ichan = open_in filename in
      let lexbuf = Lexing.from_channel ichan in
      try
       let blocks = Annot_parser.file Annot_lexer.token lexbuf in
       let annot = { blocks = blocks; mtime = ts } in
       Hashtbl.remove table filename;
       Hashtbl.add table filename annot; (* basename.annot *)
       close_in ichan;
      with ex -> begin
       close_in ichan
      end
    end
  with Sys_error _ -> ()

let (!!) filename = (Filename.chop_extension filename) ^ ".annot"

(** find *)
let rec find ~filename =
  if Filename.check_suffix filename ".ml" then begin
    let fileannot = !! filename in
    try
      if Sys.file_exists filename then begin
        if Sys.file_exists fileannot then begin
          let amtime = (Unix.stat fileannot).Unix.st_mtime in
          let smtime = (Unix.stat filename).Unix.st_mtime in
          if smtime > amtime then (raise Not_found);
          let annot =
            try
              let ca = Hashtbl.find table fileannot in
              if ca.mtime = amtime then ca
              else if ca.mtime < amtime then (raise Not_found)
              else (raise Not_found)
            with Not_found -> begin
              parse_file fileannot amtime;
              Hashtbl.find table fileannot;
            end
          in
          Some annot
        end else (raise Not_found)
      end else (raise Not_found)
    with Not_found -> begin
      Hashtbl.remove table fileannot;
      if Sys.file_exists fileannot then (Sys.remove fileannot);
      None
    end
  end else None

(** find_block_at_offset' *)
let find_block_at_offset' annot offset =
  try
    Some (List.find begin fun block ->
      block.start.pos_cnum <= offset && offset < block.stop.pos_cnum
    end annot.blocks);
  with Not_found -> None

(** find_block_at_offset *)
let find_block_at_offset ~filename ~offset =
  match find ~filename with
    | None -> None
    | Some annot -> find_block_at_offset' annot offset







