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

let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null"

let re1 = Str.regexp " ?:\\( \\|$\\)"
let re3 = Str.regexp " "
let split_nl = Str.split (Str.regexp "\n")

(** replace_extension *)
let replace_extension x =
  sprintf "%s.%s" (Filename.chop_extension x)
    (if x ^^ "cmi" then "mli" else if x ^^ "cmx" then "ml" else assert false);;

(** ocamldep *)
let ocamldep ?pp ?(with_errors=true) ?(verbose=true) target =
  let dir = Filename.dirname target in
  let redirect_stderr = if with_errors then "" else (if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null") in
  let command = sprintf "%s%s %s -native -slash -one-line %s %s %s"
      (Ocaml_config.ocamldep())
      (match pp with Some pp when pp <> "" -> " -pp " ^ pp | _ -> "" )
      (Ocaml_config.expand_includes dir)
      (match dir with "." -> "*.mli" | _ -> dir ^ "/" ^ "*.mli")
      (match dir with "." -> "*.ml" | _ -> dir ^ "/" ^ "*.ml")
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
  (table : dag);;

(** find_dep *)
let find_dep ?pp ?(with_errors=true) ?(echo=true) target =
  let table = ocamldep ?pp ~with_errors ~verbose:echo target in
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

(** array_exists *)
let array_exists from p a =
  try for i = from to Array.length a - 1 do
    if p a.(i) then raise Exit
  done; false with Exit -> true

(** reduce *)
let reduce : dag -> dag = function table ->
  let rec (<-?-) x y =
    let deps = try Hashtbl.find table y with Not_found -> [] in
    (List.mem x deps) || (List.exists ((<-?-) x) deps)
  in
  let is_descendant = (*Miscellanea.Memo.create2*) (<-?-) in
  let reduce ll =
    let stop = ref "" in
    let rec reduce' ll =
      let len = Array.length ll in
      if len <= 1 then ll
      else
        let fst = ll.(0) in
        if fst = !stop then ll
        else begin
          let len = len - 1 in
          if array_exists 1 (is_descendant fst) ll
          then begin
            let tail = Array.make len "" in
            Array.blit ll 1 tail 0 len;
            reduce' tail
          end else begin
            if !stop = "" then (stop := fst);
            Array.blit ll 1 ll 0 len;
            ll.(len) <- fst;
            reduce' ll
          end
        end
    in
    Array.to_list (reduce' (Array.of_list ll))
  in
  Hashtbl.iter (fun key deps -> Hashtbl.replace table key (reduce deps)) table;
  table;;

(** dot_of_dag *)
let dot_of_dag (dag : dag) =
  let buf = Buffer.create 1000 in
  Buffer.add_string buf "digraph {\n";
  Hashtbl.iter begin fun key ->
    List.iter (kprintf (Buffer.add_string buf) "%S -> %S;\n" key)
  end dag;
  Buffer.add_string buf "}\n";
  Buffer.contents buf;;

(*

#load "C:\\ocaml\\lib\\str.cma";;
#load "C:\\ocaml\\lib\\unix.cma";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\app_config.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\cmd.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\ocaml_config.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\miscellanea.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\file_util.cmo";;
#directory "C:\\ocaml\\devel\\ocamleditor\\src\\common"

File_util.write "test.dot" (dot_of_dag (reduce (run_ocamldep "ocamleditor.ml")));;

*)

(** find *)
let find ?pp ?with_errors ?(echo=true) targets =
  let deps = List.map (find_dep ?pp ?with_errors ~echo) targets in
  let deps = List.flatten deps in
  List.rev (List.fold_left begin fun acc x ->
      if not (List.mem x acc) then x :: acc else acc
    end [] deps)
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
