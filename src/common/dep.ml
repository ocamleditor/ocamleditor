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

type dag = (string, string list) Hashtbl.t

exception Loop_found of string

let (^^) = Filename.check_suffix
let (//) = Filename.concat

let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null"

let re1 = Str.regexp " ?:\\( \\|$\\)"
let re3 = Str.regexp " "
let split_nl = Str.split (Str.regexp "\n")

(** replace_extension *)
let replace_extension x =
  sprintf "%s.%s" (Filename.chop_extension x)
    (if x ^^ "cmi" then "mli" else if x ^^ "cmx" then "ml" else assert false);;

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

  let ocamldep ?pp ?(with_errors=true) ?(verbose=true) ?(slash=true) ?(search_path="") filenames =
    let redirect_stderr = if with_errors then "" else (if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null") in
    let command = sprintf "%s%s %s -native -one-line %s %s %s"
        (Ocaml_config.ocamldep())
        (match pp with Some pp when pp <> "" -> " -pp " ^ pp | _ -> "" )
        search_path
        (if slash then "-slash" else "")
        filenames
        redirect_stderr
    in
    if verbose then (printf "%s\n%!" command);
    let text = Cmd.expand command in
    let entries = split_nl text in
    let table = Hashtbl.create 7 in
    List.iter begin fun entry ->
      match Str.split re1 entry with
        | key :: _ when key ^^ ".cmo" -> ()
        | key :: [] -> Hashtbl.replace table key []
        | [key; deps] ->
          let deps = Str.split re3 deps in
          Hashtbl.replace table key deps
        | _ -> eprintf "%s\n%s\n%!" command entry; assert false
    end entries;
    (table : dag);;

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
    anti_loop := target :: (List.filter (fun x -> x <> target) !anti_loop);
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

