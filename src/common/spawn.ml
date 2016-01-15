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

type exit_status = [ `STATUS of Unix.process_status | `ERROR of int * string ]

module Parallel_process = struct

  exception Not_started

  type t = {
    prog             : string;
    args             : string list;
    env              : string array;
    mutable cmd_line : string;
    mutable at_exit  : (unit -> unit);
    mutable pid      : int option;
    mutable channels : (in_channel * out_channel * in_channel) option;
    mutex            : Mutex.t;
    cond             : Condition.t;
    mutable started  : bool;
    mutable verbose  : bool;
  }

  let oeproc_command = ref (App_config.get_oeproc_command ())

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
        | "Win32" -> `STATUS (Unix.close_process_full chans)
        | _ ->
          close_in inchan;
          (try close_out outchan with Sys_error _ -> ());
          close_in errchan;
          begin
            match proc.pid with
              | Some pid ->
                let _, status = waitpid_non_intr pid in
                `STATUS status
              | _ -> `ERROR (-3, "No souch process (close)")
          end
    with Not_started -> `ERROR (-2, "Not started")

  (** kill *)
  let kill p =
    channels p |> ignore;
    match p.pid with
      | Some pid ->
        let status =
          match Sys.os_type with
            | "Win32" ->
              let redirect = if p.verbose then " 2>NUL 1>NUL" else "" in
              let taskkill = sprintf "TASKKILL /F /T /PID %d%s" pid redirect in
              let exit_code = Sys.command taskkill in
              if p.verbose then printf "%s (%d)\n%!" taskkill exit_code;
              Unix.WSIGNALED exit_code
            | _ ->
              if p.verbose then printf "kill %d\n%!" pid;
              Unix.kill pid Sys.sigkill;
              (*(try ignore (Unix.wait()); with Unix.Unix_error (Unix.ECHILD as err, _, _) -> ());*)
              let _, status = Unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] pid in
              status
        in
        p.pid <- None;
        `STATUS status
      | _ -> `ERROR (-4, "No souch process (kill)")

  (** start *)
  let start p =
    if p.channels = None then begin
      p.started <- true;
      match Sys.os_type with
        | "Win32" ->
          Mutex.lock p.mutex;
          let cmd = (sprintf "\"\"%s\" %s\"" !oeproc_command p.cmd_line) in
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
          if p.verbose then
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
          if p.verbose then
            printf "(%d) %s\n%!" (match p.pid with Some pid -> pid | _ -> assert false) p.cmd_line;
    end

  (** getpid *)
  let getpid p =
    ignore (channels p);
    match p.pid with Some pid -> pid | _ -> failwith "Process.getpid"

  (** cmd_line *)
  let cmd_line proc = proc.cmd_line

  (** create *)
  let create ?(verbose=true) ?(at_exit=ignore) ?(env=Unix.environment()) ~prog ?(args=[]) () =
    let args = List.map Shell.quote_arg args in
    let arg_string = String.concat " " args in
    let cmd_line = (Shell.quote_path prog) ^ " " ^ arg_string in
    {
      env      = env;
      prog     = prog;
      args     = args;
      cmd_line = cmd_line;
      pid      = None;
      channels = None;
      mutex    = Mutex.create();
      cond     = Condition.create();
      at_exit  = at_exit;
      started  = false;
      verbose;
    }

end

type exit = Status of Unix.process_status | Async of int * (unit -> exit_status) | Error of int * string

(** iter_chan *)
let iter_chan (f : in_channel -> unit) chan = try while true do f chan done with End_of_file -> ()

(** exec *)
let exec
    ?(mode=(`SYNC : [`SYNC | `ASYNC | `ASYNC_KILL]))
    ?env
    ?(at_exit=(ignore : (exit_status -> unit)))
    ?process_in
    ?process_out
    ?process_err
    ?(verbose=false)
    command_line =
  if verbose then printf "%s\n%!" command_line;
  match mode with
    | `SYNC when process_out = None && process_in = None && process_err = None && (env = None || env = Some (Unix.environment())) ->
      let n = Sys.command command_line in
      Some (Status (Unix.WEXITED n))
    | `SYNC | `ASYNC when process_out = None && process_err = None && (env = None || env = Some (Unix.environment())) ->
      let inchan = Unix.open_process_in command_line in
      set_binary_mode_in inchan true;
      let close () = `STATUS (Unix.close_process_in inchan) in
      let thi =
        Thread.create begin fun () ->
          (match process_in with Some f -> f inchan | _ -> ());
          begin
            match mode with
              | `ASYNC ->
                let exit_code = close() in
                at_exit exit_code
              | `SYNC | `ASYNC_KILL -> ()
          end;
        end ()
      in
      begin
        match mode with
          | `SYNC ->
            Thread.join thi;
            let exit_code = close() in
            at_exit exit_code;
            begin
              match exit_code with
                | `STATUS x -> Some (Status x)
                | `ERROR (a, b) -> Some (Error (a, b))
            end;
          | `ASYNC | `ASYNC_KILL -> None
      end
    | _ ->
      let killable_proc, (inchan, outchan, errchan as channels) =
        match mode with
          | `ASYNC_KILL ->
            let proc = Parallel_process.create ?env ~prog:"" ~verbose () in
            proc.Parallel_process.cmd_line <- command_line;
            Parallel_process.start proc;
            (Some proc), (Parallel_process.channels proc)
          | `ASYNC | `SYNC ->
            None, Unix.open_process_full command_line (match env with Some e -> e | _ -> Unix.environment())
      in
      set_binary_mode_in inchan true;
      set_binary_mode_in errchan true;
      let close () =
        match killable_proc with
          | Some proc -> Parallel_process.close proc;
          | _ -> `STATUS (Unix.close_process_full channels)
      in
      let process_in = match process_in with Some f -> f | _ -> iter_chan (fun chan -> print_endline (input_line chan)) in
      let process_err = match process_err with Some f -> f | _ -> iter_chan (fun chan -> prerr_endline (input_line chan)) in
      let tho = match process_out with Some f -> Some (Thread.create f outchan) | _ -> None in
      let thi = Thread.create process_in inchan in
      let the =
        Thread.create begin fun () ->
          process_err errchan;
          Thread.join thi;
          (match tho with Some t -> Thread.join t | _ -> ());
          begin
            match mode with
              | `ASYNC | `ASYNC_KILL ->
                let exit_code = close() in
                at_exit exit_code
              | `SYNC -> ()
          end;
        end ()
      in
      begin
        match mode with
          | `SYNC ->
            Thread.join the;
            Thread.join thi;
            (match tho with Some t -> Thread.join t | _ -> ());
            let exit_code = close() in
            at_exit exit_code;
            begin
              match exit_code with
                | `STATUS x -> Some (Status x)
                | `ERROR (a, b) -> Some (Error (a, b))
            end;
          | `ASYNC -> None
          | `ASYNC_KILL ->
            begin
              match killable_proc with
                | Some proc -> Some (Async (Parallel_process.getpid proc, (fun () -> Parallel_process.kill proc)))
                | _ -> Some (Error (-90, ""))
            end;
      end;;

(** sync *)
let sync ?env ?at_exit ?process_in ?process_out ?process_err ?verbose command_line =
  match exec ~mode:`SYNC ?env ?at_exit ?process_in ?process_out ?process_err ?verbose command_line with
    | Some (Error (code, msg)) -> `ERROR (code, msg)
    | Some (Status x) -> ((`STATUS x) : exit_status)
    | Some (Async _) | None -> failwith "Spawn.sync"

(** async *)
let async ?env ?at_exit ?process_in ?process_out ?process_err ?verbose command_line =
  match exec ~mode:`ASYNC ?env ?at_exit ?process_in ?process_out ?process_err ?verbose command_line with
    | None -> ()
    | Some (Status _) | Some (Error _) | Some (Async _) -> failwith "Spawn.async"

(** async_k *)
let async_k ?env ?at_exit ?process_in ?process_out ?process_err ?verbose command_line =
  match exec ~mode:`ASYNC_KILL ?env ?at_exit ?process_in ?process_out ?process_err ?verbose command_line with
    | Some (Error (code, msg)) -> kprintf failwith "Spawn.async_k (%d, %s)" code msg
    | Some (Async (pid, kill_f)) -> pid, kill_f
    | Some (Status _) | None -> failwith "Spawn.async_k"


module Parfold = struct
  (** parfold_entry *)
  type entry = {
    pf_cmd         : string;
    pf_out         : Buffer.t;
    pf_err         : Buffer.t;
    pf_process_in  : (in_channel -> unit);
    pf_process_err : (in_channel -> unit);
  }

  (** command *)
  let command ~command ~args ?verbose () =
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
    let at_exit _ =
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
        entry.pf_cmd
    end entries;
    Mutex.lock mx_finished;
    while !nargs > 0 do Condition.wait finished mx_finished done;
    Mutex.unlock mx_finished;
    entries;;

end


#directory "common";;
#directory "+threads";;
#load "unix.cma";;
#load "str.cma";;
#load "threads.cma";;
#load "common.cma";;
open Printf
open Spawn

let process_out outchan =
  output_string outchan "[\"tell\",\"file\",\"C:/OCPWin32/develop/ocamleditor-mingw/src/common/shell.ml\"]";
  output_string outchan "[\"type\",\"enclosing\", \"at\",{\"line\":43,\"col\":54}]";
  output_string outchan "[\"case\",\"analysis\", \"from\",{\"line\":59,\"col\":13}, \"to\",{\"line\":59,\"col\":21}]";
  flush outchan;
  Thread.delay 1.;
  output_string outchan "[\"dump\",\"env\"]";
  flush outchan;;

let pid, kill = Spawn.async_k ~process_out "C:\\OCPWin32\\bin\\ocamlmerlin.exe";;

kill ();;



