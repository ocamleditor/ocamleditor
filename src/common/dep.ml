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
open Miscellanea

exception Loop_found of string

let trim = let re = Str.regexp "[ \n\r\n\t]+$" in Str.replace_first re ""
let (^^) = Filename.check_suffix
let (!$) = Filename.chop_extension

let re1 = Str.regexp ":\\( \\|$\\)"
let re2 = Str.regexp " \\\\[\r\n]+"
let re3 = Str.regexp " "
let re_ss = Str.regexp "\\\\ "
let re_00 = Str.regexp "\x00\x00"

(** replace_extension *)
let replace_extension x =
  sprintf "%s.%s" (Filename.chop_extension x)
    (if x ^^ "cmi" then "mli" else if x ^^ "cmx" then "ml" else assert false);;

(** find_dep *)
let find_dep ?pp ?includes ?(with_errors=true) target =
  let dir = Filename.dirname target in
  let anti_loop = ref [] in
  let table = Hashtbl.create 7 in
  let redirect_stderr = if with_errors then "" else (if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null") in
  let command = sprintf "%s%s %s -native -slash %s %s %s"
    (Ocaml_config.ocamldep())
    (match pp with Some pp when pp <> "" -> " -pp " ^ pp | _ -> "" )
    (Ocaml_config.expand_includes dir)
    (match dir with "." -> "*.mli" | _ -> dir // "*.mli *.mli")
    (match dir with "." -> "*.ml" | _ -> dir // "*.ml *.ml")
    redirect_stderr
  in
  printf "%s\n%!" command;
  let ocamldep = Miscellanea.expand command in
  let ocamldep = Str.global_replace re2 " " ocamldep in
  let entries = Miscellanea.split "\n" ocamldep in
  List.iter begin fun entry ->
    match Str.split re1 entry with
      | key :: [] -> Hashtbl.add table key None
      | [key; deps] ->
        let deps = Str.global_replace re_ss "\x00\x00" deps in
        let deps = Str.split re3 deps in
        let deps = List.map (Str.global_replace re_00 "\\ ") deps in
        Hashtbl.add table key (Some deps)
      | _ -> eprintf "%s\n%s\n%!" command entry; assert false
  end entries;
  let target = (Filename.chop_extension target) ^ ".cmx" in
  let result = ref [] in
  let rec find_chain target =
    if (List.mem target !anti_loop) && (not (List.mem target !result))
      then (raise (Loop_found (String.concat " " (List.map replace_extension (target :: !anti_loop)))));
    anti_loop := target :: (List.filter (fun x -> x <> target) !anti_loop);
    try
      if not (List.mem target !result) then begin
        match Hashtbl.find table target with
          | None ->
            result := target :: !result
          | Some deps ->
            List.iter find_chain deps;
            result := target :: !result;
      end
    with Not_found -> (kprintf failwith "Dep: %s" target)
  in
  find_chain target;
  List.rev (List.map replace_extension !result)

(** find *)
let find ?pp ?includes ?(with_errors=true) targets =
  let deps = List.map (find_dep ?pp ?includes) targets in
  let deps = List.flatten deps in
  List.rev (List.fold_left begin fun acc x ->
    if not (List.mem x acc) then x :: acc else acc
  end [] deps)

(** find_dependants *)
let find_dependants =
  let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null" in
  let re = Str.regexp "\\(.+\\.ml\\): ?\\(.*\\)" in
  let re1 = Str.regexp "\r?\n" in
  let re2 = Str.regexp " " in
  fun ~modname ->
    let ocamldep = kprintf Miscellanea.expand "%s -modules *.ml%s" (Ocaml_config.ocamldep()) redirect_stderr in
    let entries = Str.split re1 ocamldep in
    let entries = List.map begin fun entry ->
      if Str.string_match re entry 0 then begin
        let filename = Str.matched_group 1 entry in
        let modules = Str.matched_group 2 entry in
        (filename, (Str.split re2 modules))
      end else (assert false)
    end entries in
    let dependants = ref [] in
    let rec loop modname =
      List.iter begin fun (filename, modules) ->
        if List.mem modname modules then begin
          if not (List.mem filename !dependants) then begin
            dependants := filename :: !dependants;
            let mdep = String.capitalize (Filename.chop_extension filename) in
            ignore (loop mdep);
          end
        end
      end entries;
      !dependants
    in
    loop modname













