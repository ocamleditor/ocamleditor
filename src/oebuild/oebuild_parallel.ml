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

module Dep_dag = Oebuild_dep_dag

type process_output = {
  command           : string;
  filename          : string;
  mutable exit_code : int;
  mutable err       : Buffer.t;
  mutable out       : Buffer.t;
}

module NODE = struct
  type key = string
  type t = {
    nd_create_command     : (string -> string option);
    nd_at_exit            : (process_output -> unit);
    nd_filename           : string;
    mutable nd_processing : bool;
  }
  let equal a b = a.nd_filename = b.nd_filename
  let hash x = Hashtbl.hash x.nd_filename
  let to_string x = x.nd_filename
end

module Dag = Oebuild_dag.Make(NODE)

type t = (NODE.key, Dag.entry) Hashtbl.t

type dag = {
  graph     : t;
  ocamldeps : (string, bool * string list) Hashtbl.t;
  mutex     : Mutex.t;
}

(** print_results *)
let print_results err_outputs ok_outputs =
  flush_all();
  (*let sep = "----------------------------------------------------------------------\n" in*)
  let sep = "\n" in
  List.iter begin fun process_output ->
    let has_out = Buffer.length process_output.out > 0 in
    let has_err = Buffer.length process_output.err > 0 in
    if has_out then printf "%s\n%s\n%s%!" process_output.command (Buffer.contents process_output.out) sep;
    if has_err then begin
      (*printf "-----\n%s\n-----\n%!" process_output.command;*)
      eprintf "%s\n%s%!" (Buffer.contents process_output.err) sep;
    end
  end ok_outputs;
  flush_all();
  List.iter begin fun process_output ->
    let has_out = Buffer.length process_output.out > 0 in
    let has_err = Buffer.length process_output.err > 0 in
    let cmd = sprintf "%s\n(exit code = %d)" process_output.command process_output.exit_code in
    if has_out then printf "%s\n%s\n%s%!" cmd (Buffer.contents process_output.out) sep;
    if has_err then begin
      (*printf "-----\n%s\n-----\n%!" cmd;*)
      eprintf "%s\n%s%!" (Buffer.contents process_output.err) sep;
    end;
  end err_outputs;
  flush_all()
;;

(** create_dag *)
let create_dag ?times ~cb_create_command ~cb_at_exit ~toplevel_modules ~verbose () =
  let open Dag in
  match Dep_dag.create_dag ?times ~toplevel_modules ~verbose () with
    | Dep_dag.Cycle cycle -> kprintf failwith "Cycle: %s" (String.concat "->" cycle)
    | Dep_dag.Dag (dag', ocamldeps) ->
      let dag = Hashtbl.create 17 in
      Hashtbl.iter begin fun filename deps ->
        let node = {
          NODE.nd_create_command = cb_create_command;
          nd_at_exit        = cb_at_exit;
          nd_filename       = filename;
          nd_processing     = false
        } in
        Hashtbl.add dag filename {
          key          = filename;
          node         = node;
          dependencies = [];
          dependants   = []
        }
      end dag';
      Hashtbl.iter begin fun node deps ->
        try
          let node = Hashtbl.find dag node in
          List.iter begin fun dep ->
            try
              let e = Hashtbl.find dag dep in
              node.dependencies <- e :: node.dependencies;
            with Not_found -> ()
          end deps;
        with Not_found -> assert false
      end dag';
      set_dependants dag;
      { graph = dag; ocamldeps; mutex = Mutex.create() }
;;

let job_counter = ref 1
let job_mutex = Mutex.create ()

(** create_process *)
let create_process ?(jobs=0) ~verbose cb_create_command cb_at_exit dag leaf errors messages =
  leaf.Dag.node.NODE.nd_processing <- true;
  let filename = Oebuild_util.replace_extension_to_ml leaf.Dag.node.NODE.nd_filename in
  let command = cb_create_command filename in
  match command with
    | Some command when jobs = 0 || !job_counter <= jobs ->
      if verbose >= 4 then
        Printf.printf "Oebuild_parallel.create_process [%d/%d]: %s\n%!" !job_counter jobs (*filename*) (*print_endline*) command
      else if verbose >= 2 then print_endline command;
      let output = {
        command;
        filename;
        exit_code  = 0;
        err        = Buffer.create 10;
        out        = Buffer.create 10
      } in
      let at_exit exit_code =
        output.exit_code <- exit_code;
        if exit_code <> 0 then (errors := output :: !errors)
        else messages := output :: !messages;
        Mutex.lock dag.mutex;
        Dag.remove_leaf dag.graph leaf;
        Mutex.unlock dag.mutex;
        (*if jobs > 0 then begin*)
          Mutex.lock job_mutex;
          decr job_counter;
          Mutex.unlock job_mutex;
        (*end;*)
        cb_at_exit output
      in
      let process_in stdin =
        Buffer.add_string output.out (input_line stdin);
        Buffer.add_char output.out '\n';
      in
      let process_err stderr =
        Buffer.add_string output.err (input_line stderr);
        Buffer.add_char output.err '\n';
      in
      (*if jobs > 0 then begin*)
        Mutex.lock job_mutex;
        incr job_counter;
        Mutex.unlock job_mutex;
      (*end;*)
      Oebuild_util.exec ~verbose:false ~join:false ~at_exit ~process_in ~process_err command
    | None ->
      if verbose >= 4 then
        Printf.printf "Oebuild_parallel.create_process: %30s (No command)\n%!" filename;
      Mutex.lock dag.mutex;
      Dag.remove_leaf dag.graph leaf;
      Mutex.unlock dag.mutex;
      None
    | _ ->
      leaf.Dag.node.NODE.nd_processing <- false;
      None
;;

(** process_parallel *)
let process_parallel ?jobs ~verbose dag =
  let open NODE in
  let errors = ref [] in
  let messages = ref [] in
  (*let process_time = Unix.gettimeofday () in*)
  let leaves = ref [] in
  begin
    try
      while
        leaves := Dag.get_leaves dag.graph;
        !leaves <> []
      do
        List.iter begin fun leaf ->
          if not leaf.Dag.node.nd_processing
          then (create_process ?jobs ~verbose
                  leaf.Dag.node.nd_create_command
                  leaf.Dag.node.nd_at_exit
                  dag leaf errors messages |> ignore)
        end !leaves;
        (* Errors occurred at the same level (leaves) are independent so
           checking for the presence of errors after looping over the leaves
           can collect more than one of these independent errors (although
           there is no guarantee they are all). *)
        if !errors <> [] then raise Exit;
        Thread.delay 0.005;
      done;
    with Exit -> ()
  end;
  let errors = List.rev !errors in
  let messages = List.rev !messages in
  (*let process_time = Unix.gettimeofday () -. process_time in*)
  print_results errors messages;
  (*process_time*)
;;




