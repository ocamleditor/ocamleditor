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

(** parfold_entry *)
type parfold_entry = {
  pf_cmd         : string;
  pf_out         : Buffer.t;
  pf_err         : Buffer.t;
  pf_process_in  : (in_channel -> unit);
  pf_process_err : (in_channel -> unit);
}

(** parfold_command *)
let parfold_command ~command ~args ?verbose () =
  let finished = Condition.create() in
  let mx_nargs = Mutex.create () in
  let mx_finished = Mutex.create () in
  let nargs = ref (List.length args) in
  let write buf chan =
    Buffer.add_string buf (input_line chan);
    Buffer.add_char buf '\n';
  in
  let entries = List.map begin fun arg ->
    let out = Buffer.create 10 in
    let err = Buffer.create 10 in {
      pf_cmd         = sprintf "%s %s" command arg;
      pf_out         = out;
      pf_err         = err;
      pf_process_in  = write out;
      pf_process_err = write err;
    } end args in
  let at_exit exit_code =
    Mutex.lock mx_nargs;
    decr nargs;
    Mutex.unlock mx_nargs;
    Mutex.lock mx_finished;
    if !nargs = 0 then Condition.signal finished;
    Mutex.unlock mx_finished;
  in
  List.iter begin fun entry ->
    async ?env:None ?verbose ~at_exit
        ~process_in:(iter_chan entry.pf_process_in)
        ~process_err:(iter_chan entry.pf_process_err)
        entry.pf_cmd |> ignore
  end entries;
  Mutex.lock mx_finished;
  while !nargs > 0 do Condition.wait finished mx_finished done;
  Mutex.unlock mx_finished;
  entries
;;
