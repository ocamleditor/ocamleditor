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
open Location
open Lexing
open Miscellanea

let string_of_loc loc =
  let filename, a, b = Location.get_pos_info loc.loc_start in
  let _, c, d = Location.get_pos_info loc.loc_end in
  sprintf "%s, %d:%d(%d) -- %d:%d(%d)" filename a b (loc.loc_start.pos_cnum) c d (loc.loc_end.pos_cnum);;

let linechar_of_loc loc =
  let _, a, b = Location.get_pos_info loc.loc_start in
  let _, c, d = Location.get_pos_info loc.loc_end in
  ((a - 1), b), ((c - 1), d)

let string_of_type_expr te = Odoc_info.string_of_type_expr te;;

let (<==) loc offset = loc.loc_start.pos_cnum <= offset && offset <= loc.loc_end.pos_cnum

(** read *)
let read ~page =
  let project = page#project in
  let filename = page#get_filename in
  let ext = if filename ^^ ".ml" then Some ".cmt" else if filename ^^ ".mli" then Some ".cmti" else None in
  match ext with
    | Some ext ->
      begin
        match Project.tmp_of_abs project filename with
          | Some (tmp, relname) ->
            let source_tmp = tmp // relname in
            if not (Sys.file_exists source_tmp) then (page#compile_buffer ?join:(Some true) ());
            let cmt = (Filename.chop_extension source_tmp) ^ ext in
            let mtime_src = (Unix.stat source_tmp).Unix.st_mtime in
            let mtime_cmt = (Unix.stat cmt).Unix.st_mtime in
            if mtime_cmt >= mtime_src then Some (Cmt_format.read cmt) else None
          | _ -> None
      end;
    | _ -> kprintf invalid_arg "Binannot.read \"%s\"" filename



