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

(** iter_chan *)
let iter_chan (f : in_channel -> unit) chan = try while true do f chan done with End_of_file -> ()

(** exec *)
let exec =
  let re_spaces = Str.regexp " +" in
  fun ?(env=Unix.environment()) ?(verbose=true) ?(sync=true) ?at_exit
    ?(process_in=(iter_chan (fun chan -> print_endline (input_line chan))))
    ?(process_err=(iter_chan (fun chan -> prerr_endline (input_line chan))))
    command_line ->
    let at_exit = match at_exit with None when not sync -> Some ignore | _ -> at_exit in
    let command_line = Str.global_replace re_spaces " " command_line in
    if verbose then printf "%s\n%!" command_line;
    let exit_code = ref (-9998) in
    let (inchan, _, errchan) as channels = Unix.open_process_full command_line env in
    set_binary_mode_in inchan true;
    set_binary_mode_in errchan true;
    let close () =
      match Unix.close_process_full channels with
       	| Unix.WEXITED code -> code
       	| _ -> (-9997)
    in
    let thi =
      Thread.create begin fun () ->
        process_in inchan;
      end ()
    in
    let the =
      Thread.create begin fun () ->
        process_err errchan;
        Thread.join thi;
        begin
          match at_exit with
            | None -> ()
            | Some f ->
              exit_code := close();
              f !exit_code
        end;
      end ()
    in
    if sync then begin
      Thread.join thi;
      Thread.join the;
    end;
    if at_exit = None then Some (close()) else None;;

let sync = exec ~sync:true
let async = exec ~sync:false

