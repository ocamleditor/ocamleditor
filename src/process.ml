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

exception Not_started

type t = {
  prog : string;
  args : string array;
  env : string array;
  mutable cmd_line : string;
  mutable at_exit : (unit -> unit);
  mutable pid : int option;
  mutable channels : (in_channel * out_channel * in_channel) option;
  mutex : Mutex.t;
  cond : Condition.t;
  mutable started : bool;
}

let re = Str.regexp " +"
let (//) = Filename.concat

(** channels *)
let channels p =
  if not p.started then (raise Not_started);
  Mutex.lock p.mutex;
  while p.channels = None do Condition.wait p.cond p.mutex done;
  let cc = match p.channels with None -> assert false | Some cc -> cc in
  Mutex.unlock p.mutex;
  cc

let open_proc_full cmd env input output error toclose =
  match Unix.fork() with
     0 -> Unix.dup2 input Unix.stdin; Unix.close input;
          Unix.dup2 output Unix.stdout; Unix.close output;
          Unix.dup2 error Unix.stderr; Unix.close error;
          List.iter Unix.close toclose;
          begin try Unix.execve "/bin/sh" [| "/bin/sh"; "-c"; cmd |] env
          with _ -> exit 127
          end;
  | id -> id

(** Modified version of open_process_full from the OCaml Unix module
    in order to return the pid of the child process. *)
let open_process_full cmd env =
  let (in_read, in_write) = Unix.pipe() in
  let (out_read, out_write) = Unix.pipe() in
  let (err_read, err_write) = Unix.pipe() in
  let inchan = Unix.in_channel_of_descr in_read in
  let outchan = Unix.out_channel_of_descr out_write in
  let errchan = Unix.in_channel_of_descr err_read in
  let pid = open_proc_full cmd env out_read in_write err_write [in_read; out_write; err_read] in
  Unix.close out_read;
  Unix.close in_write;
  Unix.close err_write;
  pid, (inchan, outchan, errchan)

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

(** close *)
let close proc =
  try 
    let (inchan, outchan, errchan) as chans = channels proc in
    match Sys.os_type with
    | "Win32" -> ignore (Unix.close_process_full chans)
    | _ ->
      close_in inchan;
      begin try close_out outchan with Sys_error _ -> () end;
      close_in errchan;
      (match proc.pid with Some pid -> ignore (waitpid_non_intr pid) | _ -> ())
  with Not_started -> ()


(** kill *)
let kill p =
  ignore (channels p);
  match p.pid with
  | None -> ()
  | Some pid ->
    begin
      match Sys.os_type with
      | "Win32" ->
        let taskkill = sprintf "TASKKILL /F /T /PID %d" pid in
        let exit_code = Sys.command taskkill in
        printf "%s (%d)\n%!" taskkill exit_code;
      | _ ->
        printf "kill %d\n%!" pid;
        Unix.kill pid Sys.sigkill;
        (*(try ignore (Unix.wait()); with Unix.Unix_error (Unix.ECHILD as err, _, _) -> ());*)
        ignore (Unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] pid);
    end;
    p.pid <- None

(** start *)
let start p =
  if p.channels = None then begin
    p.started <- true;
    match Sys.os_type with
      | "Win32" ->
        Mutex.lock p.mutex;
        let oeproc = Oe_config.oeproc_command in
        let cmd = (sprintf "\"\"%s\" %s\"" oeproc p.cmd_line) in
        let (inchan, outchan, errchan) as channels = Unix.open_process_full cmd p.env in
        begin
          try
            set_binary_mode_in inchan false;
            let pid = input_line inchan in
            p.pid <- Some (int_of_string pid);
            set_binary_mode_in inchan true;
            p.channels <- Some channels;
            Condition.broadcast p.cond;
          with
            | End_of_file ->
              if p.pid = None then (eprintf "Process: End_of_file, PID = None\n%!");
              Condition.broadcast p.cond;
            | ex -> begin
                eprintf "Process: %s%s\n%!" (if p.pid = None then "PID = None\n" else "") (Printexc.to_string ex);
                ignore (Unix.close_process_full channels);
                Condition.broadcast p.cond;
              end;
        end;
        Mutex.unlock p.mutex;
        printf "(%d) %s\n%!" (match p.pid with Some pid -> pid | _ -> 0) p.cmd_line;
        if p.pid = None then (p.started <- false)
      | _ ->
        Mutex.lock p.mutex;
        let pid, (inchan, outchan, errchan) (*as channels*) =
          open_process_full p.cmd_line p.env
        in
        p.channels <- Some (inchan, outchan, errchan);
        p.pid <- Some pid;
        Condition.broadcast p.cond;
        Mutex.unlock p.mutex;
        printf "(%d) %s\n%!" (match p.pid with Some pid -> pid | _ -> assert false) p.cmd_line;
  end

(** getpid *)
let getpid p = 
  ignore (channels p);
  match p.pid with Some pid -> pid | _ -> failwith "Process.getpid"

(** cmd_line *)
let cmd_line proc = proc.cmd_line

(** create *)
let create ?(at_exit=ignore) ?(env=Unix.environment()) ~prog ?(args=[||]) () =
  let arg_string = String.concat " " (Array.to_list args) in
  let cmd_line = prog ^ " " ^ arg_string in
  { 
    env = env;
    prog = prog;
    args = args;
    cmd_line = cmd_line;
    pid = None;
    channels = None;
    mutex = Mutex.create();
    cond = Condition.create();
    at_exit = at_exit;
    started = false;
  }

















