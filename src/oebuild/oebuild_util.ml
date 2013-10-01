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

let (!$) = Filename.chop_extension
let (//) = Filename.concat
let (^^) = Filename.check_suffix
let (<@) = List.mem
let is_win32, win32 = (Sys.os_type = "Win32"), (fun a b -> match Sys.os_type with "Win32" -> a | _ -> b)
let may opt f = match opt with Some x -> f x | _ -> ()
let re_spaces = Str.regexp " +"

(** crono *)
let crono ?(label="Time") f x =
  let finally time =
    Printf.fprintf stdout "%s: %f sec." label (Unix.gettimeofday() -. time);
    print_newline();
  in
  let time = Unix.gettimeofday() in
  let result = try f x with e -> begin
    finally time;
    raise e
  end in
  finally time;
  result

(** remove_dupl *)
let remove_dupl l =
  List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)

(** remove_file *)
let remove_file ?(verbose=false) filename =
  try
    if Sys.file_exists filename then (Sys.remove filename; if verbose then print_endline filename)
  with Sys_error ex -> eprintf "%s\n%!" ex

(** command *)
let command ?(echo=true) cmd =
  let cmd = Str.global_replace re_spaces " " cmd in
  if echo then (printf "%s\n%!" cmd);
  let exit_code = Sys.command cmd in
  Pervasives.flush stderr;
  Pervasives.flush stdout;
  exit_code

(** iter_chan *)
let iter_chan chan f = try while true do f chan done with End_of_file -> ()

(** exec *)
let exec ?(env=Unix.environment()) ?(verbose=true) ?(join=true) ?at_exit ?(process_err=(fun ~stderr -> prerr_endline (input_line stderr))) cmd =
  let cmd = Str.global_replace re_spaces " " cmd in
  if verbose then printf "%s\n%!" cmd;
  let (inchan, _, errchan) as channels = Unix.open_process_full cmd env in
  let close () =
  match Unix.close_process_full channels with
	| Unix.WEXITED code -> code
	| _ -> (-1)
  in
  let thi =
    Thread.create begin fun () ->
      iter_chan inchan (fun chan -> print_endline (input_line chan));
    end ()
  in
  let the =
    Thread.create begin fun () ->
      iter_chan errchan (fun chan -> process_err ~stderr:chan);
      Thread.join thi;
      (match at_exit with None -> () | Some f -> ignore (close()); f());
    end ()
  in
  if join then begin
    Thread.join thi;
    Thread.join the;
  end;
  if at_exit = None then (close()) else 0

(** Remove files with wildcards *)
let rm = win32 "DEL /F /Q" "rm -f"

(** Copy file *)
let copy_file ic oc =
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let cp ?(echo=true) src dst =
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  if echo then (printf "%s -> %s\n%!" src dst);
  let finally () = close_out oc; close_in ic in
  try copy_file ic oc; finally() with ex -> (finally(); raise ex)

(** mkdir *)
let rec mkdir_p ?(echo=true) d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    printf "mkdir -p %s\n%!" d;
    (Unix.mkdir d 0o755)
  end

