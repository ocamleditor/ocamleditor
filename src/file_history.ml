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

type t = {
  mutable content : string list;
  filename : string;
  max_length : int;
}

(** create *)
let create ~filename ~max_length = {
  content = [];
  filename = filename;
  max_length = max_length;
}

(** write *)
let write h =
  let ochan = open_out h.filename in
  try
    List.iter (fprintf ochan "%s\n") h.content;
    close_out ochan;
  with _ -> (close_out ochan)

(** read *)
let read h =
  if Sys.file_exists h.filename then begin
    let ichan = open_in h.filename in
    try
      begin
        try
          while true do
            let elem = Pervasives.input_line ichan in
            if Sys.file_exists elem then (h.content <- elem :: h.content)
          done;
        with End_of_file -> ()
      end;
      close_in ichan;
      h.content <- List.rev h.content;
    with _ -> (close_in ichan)
  end

(** add *)
let add h elem =
  if List.length h.content > h.max_length then begin
    h.content <- List.rev (List.tl (List.rev h.content))
  end;
  h.content <- elem :: (List.filter ((<>) elem) h.content)

(** clear *)
let clear h =
  h.content <- [];
  write h


