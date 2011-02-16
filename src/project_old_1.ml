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

(** Deprecated *)

open Miscellanea
open Printf

type paths = {
  mutable home : string;
  mutable source : string;
  mutable backup : string;
  mutable doc : string
}

type t = {
  mutable name : string;
  mutable saved : bool;
  mutable modified : bool;
  mutable author : string;
  mutable description : string;
  mutable version : string;
  mutable paths : paths;
  mutable files : File.file list;
  mutable current_file : string;
  mutable current_file_position : int;
  mutable loadfiles : string list;
  mutable file_history : string list
}

let create ~filename () =
  let home = Filename.dirname filename in
  let paths = {
    home = home;
    source = "";
    backup = "";
    doc = "";
  } in
  let project = {
    name = Filename.chop_extension (Filename.basename filename);
    saved = false;
    modified = true;
    author = "";
    paths = paths;
    files = [];
    current_file = "";
    current_file_position = 0;
    loadfiles = [];
    file_history = [];
    description = "";
    version = "1.0.0";
  } in project

let project_name_extension = ".mlp"

let filename proj = Filename.concat proj.paths.home (proj.name^project_name_extension)

let load filename =
  let proj = create ~filename () in
  let home = Filename.dirname filename in
  let concat_home filename =
    match Miscellanea.filename_relative home filename with
      | None -> Filename.concat home filename
      | Some _ -> filename
  in
  let chan = open_in filename in
  set_binary_mode_in chan false;
  let properties = Hashtbl.create 7 in
  let _ = try
    while true do
      let line = (input_line chan) in
      let line = if line.[String.length line - 1] = '\r'
        then String.sub line 0 (String.length line - 1) else line in
      let pos = String.index line '=' + 1 in
      Hashtbl.add properties
        (String.sub line 0 (pos - 1)) (String.sub line pos (String.length line - pos));
    done
  with End_of_file -> () in
  let find = Hashtbl.find properties in
  proj.name <- find "name";
  proj.author <- find "author";
  proj.description <- find "description";
  proj.version <- find "version";
  proj.paths.home <- home;
  proj.paths.source <- concat_home (find "paths.source");
  proj.paths.backup <- concat_home (find "paths.backup");
  proj.paths.doc <- concat_home (find "paths.doc");
  proj.current_file <- (try concat_home (find "current_file") with Not_found -> "");
  proj.current_file_position <- (try int_of_string (find "current_file_position") with Not_found -> 0);
  proj.loadfiles <- List.map concat_home (Hashtbl.find_all properties "loadfiles");
  let recent = try Miscellanea.split ";" (Hashtbl.find properties "file_history") with _ -> [] in
  proj.file_history <- recent;
  close_in chan;
  proj.saved <- true;
  proj.modified <- false;
  proj

