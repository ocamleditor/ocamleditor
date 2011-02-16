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

type kind = [ `CLEAN | `CLEANALL | `ANNOT | `COMPILE | `RUN | `OTHER]
type phase = Before_clean | Clean | After_clean | Before_compile | Compile | After_compile

type t = {
  mutable name : string;
  mutable env : string list;
  mutable env_replace : bool; (* After system environment *)
  mutable dir : string;       (* Working directory: relative to the project source directory (actually: Sys.getcwd()) *)
  mutable cmd : string;
  mutable args : string list;
  mutable phase : phase option;
  mutable always_run : bool;
}

let string_of_phase = function
  | Before_clean -> "Before_clean"
  | Clean -> "Clean"
  | After_clean -> "After_clean"
  | Before_compile -> "Before_compile"
  | Compile -> "Compile"
  | After_compile -> "After_compile"

let descr_of_phase = function
  | Before_clean -> "Pre-clean"
  | Clean -> "Clean"
  | After_clean -> "Post-clean"
  | Before_compile -> "Pre-build"
  | Compile -> "Build"
  | After_compile -> "Post-build"

let phase_of_string = function
  | "Before_clean" -> Before_clean
  | "Clean" -> Clean
  | "After_clean" -> After_clean
  | "Before_compile" -> Before_compile
  | "Compile" -> Compile
  | "After_compile" -> After_compile
  | _ -> failwith "phase_of_string"

let create ~name ~env ?(env_replace=false) ~dir ~cmd ~args ?phase () = {
    name = name;
    env = env;
    env_replace = env_replace;
    dir = dir;
    cmd = cmd;
    args = args;
    phase = phase;
    always_run = false;
  }

(** prepare *)
let prepare task =
  let tenv = Array.of_list task.env in
  let env =
    if task.env_replace then Array.concat [(*Unix.environment();*) tenv]
    else (Array.concat [tenv (* takes precedence *); Unix.environment()])
  in
  let prog = Quote.path task.cmd in
  let args = List.map Quote.arg task.args in
  let args = Array.of_list args in
  let proc = Process.create ~env ~prog ~args () in (* 2>&1 *)
  let task_working_dir = if task.dir <> "" then task.dir else (Sys.getcwd ()) in
  proc, fun () ->
    let cwd = Sys.getcwd () in
    Sys.chdir task_working_dir;
    Process.start proc;
    Sys.chdir cwd








