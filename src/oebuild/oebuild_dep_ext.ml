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


open Oebuild_dep_dag
open Oebuild_util

(** find_top_modules *)
let find_top_modules dir =
  let files = Array.to_list (Sys.readdir dir) in
  let files = List.map (fun x -> dir // x) files in
  let files = List.filter (fun x -> x ^^^ ".ml" && not (Sys.is_directory x)) files in
  let search_path = Ocaml_config.expand_includes dir in
  let table = Oebuild_dep.ocamldep ~slash:false ~verbose:false ~search_path (dir // "*.ml") in
  let files = ref files in
  Hashtbl.iter begin fun filename (changed, deps) ->
    let mldeps = List.filter (fun x -> not (x ^^^ ".cmi")) deps in
    let mldeps = List.map (fun x -> (Filename.chop_extension x) ^ ".ml") mldeps in
    let mldeps = List.filter ((<>) filename) mldeps in
    let mldeps = List.map (fun x -> if Filename.is_implicit x then dir // x else x) mldeps in
    files := List.filter (fun x -> not (List.mem x mldeps)) !files;
  end table;
  !files
;;

(*(** analyze *)
let analyze dag =
  let rdag =
    let t = Hashtbl.create 17 in
    Hashtbl.iter (fun key deps -> List.iter (fun d -> Hashtbl.add t d key) deps) dag;
    t
  in
  let rec chain x =
    try
      begin
        match Hashtbl.find_all rdag x with
          | parent :: [] ->
            let n_childs = try List.length (Hashtbl.find dag parent) with Not_found -> 0 in
            if n_childs = 1 then x :: (chain parent) else [x]
          | _ -> [x]
      end;
    with Not_found -> []
  in
  let leaves =
    Hashtbl.fold begin fun key deps acc ->
      match deps with
        | [] -> (chain key) :: acc
        | _ -> acc
    end dag []
  in
  let fleaves = List.flatten leaves in
  List.iter (Hashtbl.remove dag) fleaves;
  Hashtbl.iter begin fun key deps ->
    Hashtbl.replace dag key (List.filter (fun d -> not (List.mem d fleaves)) deps)
  end dag;
  leaves
;;*)

