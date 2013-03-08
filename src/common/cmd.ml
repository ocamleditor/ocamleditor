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

let expand =
  let trimfunc =
     let replace = Str.global_replace (Str.regexp "\\(^[ \t\r\n]+\\)\\|\\([ \t\r\n]+$\\)") in
     fun str -> replace "" str
  in fun ?(trim=true) ?(first_line=false) ?filter command ->
    let ichan = Unix.open_process_in command in
    let finally () = ignore (Unix.close_process_in ichan) in
    let data = Buffer.create 100 in
    begin
      try
        let get_line ichan = if trim then trimfunc (input_line ichan) else input_line ichan in
        while true do
          let line = get_line ichan in
          if first_line && String.length line = 0 then begin
          end else if first_line then begin
            Buffer.add_string data line;
            raise End_of_file
          end else begin
            match filter with
              | None ->
                Buffer.add_string data line;
                Buffer.add_char data '\n'
              | Some f when (f line) ->
                Buffer.add_string data line;
                Buffer.add_char data '\n'
              | _ -> ()
          end
        done
      with
        | End_of_file -> ()
        | ex -> (finally(); raise ex)
    end;
    finally();
    if Buffer.length data = 0 then (kprintf failwith "Cmd.expand: %s" command);
    Buffer.contents data;;
