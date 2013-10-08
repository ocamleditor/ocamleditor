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
open Oebuild_util

exception Loop_found of string

let re1 = Str.regexp " ?:\\( \\|$\\)"
let re3 = Str.regexp " "
let split_nl = Str.split (Str.regexp "\n")

(** ocamldep_command *)
let ocamldep_command ?pp ?(slash=true) ?(search_path="") () =
  sprintf "%s%s %s -native -one-line %s "
    (Ocaml_config.ocamldep())
    (match pp with Some pp when pp <> "" -> " -pp " ^ pp | _ -> "" )
    search_path
    (if slash then "-slash" else "");;

(** ocamldep *)
let ocamldep ?times ?pp ?(with_errors=true) ?(verbose=true) ?slash ?search_path filenames =
  let redirect_stderr = if with_errors then "" else Oebuild_util.redirect_stderr in
  let command = ocamldep_command ?pp ?slash ?search_path () in
  let command = sprintf "%s %s %s" command filenames redirect_stderr in
  if verbose then (printf "%s\n%!" command);
  let text = Cmd.expand command in
  let entries = split_nl text in
  let table = Hashtbl.create 7 in
  let replace =
    match times with
      | Some (times, opt) ->
        fun table key data ->
          let fn = Oebuild_util.replace_extension key in
          let changed = Oebuild_table.update ~opt times fn in
          if changed then Hashtbl.replace table key data
      | _ -> Hashtbl.replace
  in
  List.iter begin fun entry ->
    match Str.split re1 entry with
      | key :: _ when key ^^ ".cmo" -> ()
      | key :: [] -> replace table key []
      | [key; deps] ->
        let deps = Str.split re3 deps in
        replace table key deps
      | _ -> eprintf "%s\n%s\n%!" command entry; assert false
  end entries;
  table;;

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
    Oebuild_util.exec ?env:None ?verbose ~join:false ~at_exit
        ~process_in:entry.pf_process_in
        ~process_err:entry.pf_process_err
        entry.pf_cmd |> ignore
  end entries;
  Mutex.lock mx_finished;
  while !nargs > 0 do Condition.wait finished mx_finished done;
  Mutex.unlock mx_finished;
  entries
;;

(** ocamldep_parfold *)
let ocamldep_parfold ?times ?pp ?(with_errors=true) ?(verbose=true) ?slash ?search_path ~toplevel_modules () =
  let redirect_stderr = if with_errors then "" else Oebuild_util.redirect_stderr in
  let command = ocamldep_command ?pp ?slash ?search_path () in


()


(** find_dep *)
let find_dep ?pp ?(with_errors=true) ?(echo=true) target =
  let dir = Filename.dirname target in
  let filenames =
    (match dir with "." -> "*.mli" | _ -> dir ^ "/" ^ "*.mli *.mli") ^ " " ^
      (match dir with "." -> "*.ml" | _ -> dir ^ "/" ^ "*.ml *.ml")
  in
  let search_path = Ocaml_config.expand_includes dir in
  let table = ocamldep ?pp ~with_errors ~verbose:echo ~search_path filenames in
  let target = (Filename.chop_extension target) ^ ".cmx" in
  let anti_loop = ref [] in
  let result = ref [] in
  let rec find_chain target =
    if (List.mem target !anti_loop) && (not (List.mem target !result))
    then (raise (Loop_found (String.concat " " (List.map replace_extension (target :: !anti_loop)))));
    anti_loop := target :: (List.filter ((<>) target) !anti_loop);
    try
      if not (List.mem target !result) then begin
        match Hashtbl.find table target with
          | [] -> result := target :: !result;
          | deps ->
            List.iter find_chain deps;
            result := target :: !result;
      end
    with Not_found ->
      (* This exception can be caused by syntax errors in the source files. *)
      (kprintf failwith "Dep: %s" target)
  in
  find_chain target;
  List.rev (List.map replace_extension !result)
;;

(** find_dependants *)
let find_dependants =
  let re = Str.regexp "\\(.+\\.mli?\\) ?: ?\\(.*\\)" in
  let re1 = Str.regexp "\r?\n" in
  let re2 = Str.regexp " " in
  fun ~dirname ~modname ->
    (*let dir = Filename.dirname target in*)
    let dir = if dirname = Filename.current_dir_name then "" else (dirname ^ "/") in
    let cmd = sprintf "%s -modules -native %s*.ml %s*.mli%s"
        (Ocaml_config.ocamldep()) dir dir redirect_stderr in
    printf "%s (%s)\n%!" cmd modname;
    let ocamldep = Cmd.expand cmd in
    let entries = Str.split re1 ocamldep in
    let entries =
      List.map begin fun entry ->
        if Str.string_match re entry 0 then begin
          let filename = Str.matched_group 1 entry in
          let modules = Str.matched_group 2 entry in
          (filename, (Str.split re2 modules))
        end else (assert false)
      end entries
    in
    let dependants = ref [] in
    let rec loop modname =
      List.iter begin fun (filename, modules) ->
        if List.mem modname modules then begin
          if not (List.mem filename !dependants) then begin
            dependants := filename :: !dependants;
            let prefix = Filename.chop_extension filename in
            let prefix_mli = prefix ^ ".mli" in
            if List.mem_assoc prefix_mli entries then (dependants := prefix_mli :: !dependants;);
            let mdep = String.capitalize prefix in
            ignore (loop mdep);
          end
        end
      end entries;
      !dependants
    in
    loop modname

let find_dependants ~path ~modname =
  let dependants = List.map (fun dirname -> find_dependants ~dirname ~modname) path in
  List.flatten dependants
;;

(** find *)
let find ?pp ?with_errors ?(echo=true) targets =
  let deps = List.map (find_dep ?pp ?with_errors ~echo) targets in
  let deps = List.flatten deps in
  List.rev (List.fold_left begin fun acc x ->
      if not (List.mem x acc) then x :: acc else acc
    end [] deps)
;;

(** ocamldep *)
(*let ocamldep ?pp ?(with_errors=true) ?(verbose=true) target =
  let dir = Filename.dirname target in
  let redirect_stderr = if with_errors then "" else (if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null") in
  let command = sprintf "%s%s %s -native -slash -one-line %s %s %s"
      (Ocaml_config.ocamldep())
      (match pp with Some pp when pp <> "" -> " -pp " ^ pp | _ -> "" )
      (Ocaml_config.expand_includes dir)
      (match dir with "." -> "*.mli" | _ -> dir ^ "/" ^ "*.mli *.mli")
      (match dir with "." -> "*.ml" | _ -> dir ^ "/" ^ "*.ml *.ml")
      redirect_stderr
  in
  if verbose then (printf "%s\n%!" command);
  let ocamldep = Cmd.expand command in
  let table = Hashtbl.create 7 in
  let entries = split_nl ocamldep in
  List.iter begin fun entry ->
    match Str.split re1 entry with
      | key :: _ when key ^^ ".cmo" -> ()
      | key :: [] -> Hashtbl.replace table key []
      | [key; deps] ->
        let deps = Str.split re3 deps in
        Hashtbl.replace table key deps
      | _ -> eprintf "%s\n%s\n%!" command entry; assert false
  end entries;
  (table : dag);;*)

