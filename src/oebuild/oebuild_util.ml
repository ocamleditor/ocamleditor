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

let (!$) = Filename.chop_extension
let (//) = Filename.concat
let (^^) = Filename.check_suffix
let (<@) = List.mem
let is_win32, win32 = (Sys.os_type = "Win32"), (fun a b -> match Sys.os_type with "Win32" -> a | _ -> b)
let may opt f = match opt with Some x -> f x | _ -> ()
let re_spaces = Str.regexp " +"
let redirect_stderr_to_null = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null"

(** crono *)
let crono ?(label="Time") f x =
  let finally time =
    Printf.fprintf stdout "[CRONO] %s: %f sec." label (Unix.gettimeofday() -. time);
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
let exec ?(env=Unix.environment()) ?(verbose=true) ?(join=true) ?at_exit
    ?(process_in=(fun chan -> print_endline (input_line chan)))
    ?(process_err=(fun chan -> prerr_endline (input_line chan)))
    cmd =
  let cmd = Str.global_replace re_spaces " " cmd in
  if verbose then printf "%s\n%!" cmd;
  let exit_code = ref (-9998) in
  let (inchan, _, errchan) as channels = Unix.open_process_full cmd env in
  let close () =
    match Unix.close_process_full channels with
     	| Unix.WEXITED code -> code
     	| _ -> (-9997)
  in
  let thi =
    Thread.create begin fun () ->
      iter_chan inchan process_in;
    end ()
  in
  let the =
    Thread.create begin fun () ->
      iter_chan errchan process_err;
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
  if join then begin
    Thread.join thi;
    Thread.join the;
  end;
  if at_exit = None then Some (close()) else None;;

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

(*(** replace_extension *)
let replace_extension x =
  sprintf "%s.%s" (Filename.chop_extension x)
    (if x ^^ "cmi" then "mli" else if x ^^ "cmx" then "ml" else assert false);;*)

(** replace_extension_to_ml *)
let replace_extension_to_ml filename =
  if Filename.check_suffix filename ".cmx" then (Filename.chop_extension filename) ^ ".ml"
  else if Filename.check_suffix filename ".cmi" then (Filename.chop_extension filename) ^ ".mli"
  else filename
;;

(** get_effective_command *)
let get_effective_command =
  let re_verbose = Str.regexp " -verbose" in
  fun ?(linkpkg=false) ocamlfind ->
    try
      let cmd = sprintf "%s%s -verbose %s" ocamlfind (if linkpkg then " -linkpkg" else "") redirect_stderr_to_null in
      let lines = Cmd.exec_lines cmd in
      let effective_compiler = List.find (fun line -> String.sub line 0 2 = "+ ") lines in
      let effective_compiler = Str.string_after effective_compiler 2  in
      let effective_compiler = Str.replace_first re_verbose "" effective_compiler in
      effective_compiler
    with Not_found -> ocamlfind
;;

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
    exec ?env:None ?verbose ~join:false ~at_exit
        ~process_in:entry.pf_process_in
        ~process_err:entry.pf_process_err
        entry.pf_cmd |> ignore
  end entries;
  Mutex.lock mx_finished;
  while !nargs > 0 do Condition.wait finished mx_finished done;
  Mutex.unlock mx_finished;
  entries
;;
