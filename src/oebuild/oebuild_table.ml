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


open Printf

let dummy_crc = String.make 32 '0'

(* <filename.[opt|byt], last-compiled> *)
type t = (string, float) Hashtbl.t

let oebuild_times_filename = ".oebuild"

let (^^) filename opt = filename ^ (if opt then ".opt" else ".byt")

let find (table : t) filename opt = Hashtbl.find table (filename ^^ opt)
let add (table : t) filename opt = Hashtbl.add table (filename ^^ opt)
let remove (table : t) filename opt = Hashtbl.remove table (filename ^^ opt)

(** read *)
let read () =
  if not (Sys.file_exists oebuild_times_filename) then begin
    let ochan = open_out_bin oebuild_times_filename in
    Marshal.to_channel ochan (Hashtbl.create 7) [];
    close_out ochan
  end;
  let ichan = open_in_bin oebuild_times_filename in
  let times = Marshal.from_channel ichan in
  close_in ichan;
  (times : t)

(** write *)
let write (times : t) =
  if Hashtbl.length times > 0 then begin
    let ochan = open_out_bin oebuild_times_filename in
    Marshal.to_channel ochan times [];
    close_out ochan
  end

(** update *)
let update =
  let get_last_compiled_time ~opt cache filename =
    try
      let time = find cache filename opt in
      let ext = if opt then "cmx" else "cmo" in
      let cm = sprintf "%s.%s" (Filename.chop_extension filename) ext in
      if Sys.file_exists cm then time
      else begin
        remove cache filename opt;
        raise Not_found
      end
    with Not_found -> 0.0
  in
  fun ~opt (cache : t) filename ->
    let ctime = get_last_compiled_time ~opt cache filename in
    if ctime > 0.0 && ((Unix.stat filename).Unix.st_mtime) >= ctime then begin
      remove cache filename opt;
      true
    end else ctime = 0.0
;;
