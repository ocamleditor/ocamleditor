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

open Dep
open Printf
open Miscellanea

(** array_exists *)
let array_exists from p a =
  try for i = from to Array.length a - 1 do
    if p a.(i) then raise Exit
  done; false with Exit -> true

(** reduce *)
let reduce : dag -> unit = function table ->
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
  Hashtbl.iter (fun key deps -> Hashtbl.replace table key (reduce deps)) table
;;

(** dot_of_dag *)
let dot_of_dag (dag : dag) =
  let buf = Buffer.create 1000 in
  Buffer.add_string buf "digraph {\n";
  Hashtbl.iter begin fun key ->
    List.iter (kprintf (Buffer.add_string buf) "%S -> %S;\n" key)
  end dag;
  Buffer.add_string buf "}\n";
  Buffer.contents buf;;

(** find' *)
let find' target =
  let target = (Filename.chop_extension target) ^ ".cmx" in
  let dir = Filename.dirname target in
  let filenames =
    (match dir with "." -> "*.mli" | _ -> dir ^ "/" ^ "*.mli *.mli") ^ " " ^
      (match dir with "." -> "*.ml" | _ -> dir ^ "/" ^ "*.ml *.ml")
  in
  let search_path = Ocaml_config.expand_includes dir in
  let deps = ocamldep ~search_path filenames in
  let out = Hashtbl.create 17 in
  let rec add root =
    let deps = try Hashtbl.find deps root with Not_found -> [] in
    Hashtbl.replace out root deps;
    List.iter add deps
  in
  add target;
  reduce out;
  (out : dag);;

(** analyze *)
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
;;


(*
#load "C:\\ocaml\\lib\\str.cma";;
#load "C:\\ocaml\\lib\\unix.cma";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\cmd.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\app_config.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\ocaml_config.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\miscellanea.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\file_util.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\dep.cmo";;
#directory "C:\\ocaml\\devel\\ocamleditor\\src\\common"



let dag = find' "editor_page.ml" in File_util.write "D://temp/test.dot" (dot_of_dag dag);;

let dag = find' "editor_page.ml" in for i = 1 to 5 do analyze dag |> ignore done; File_util.write "D://temp/test.dot" (dot_of_dag dag);;

let dag = find' "prj.ml" in analyze dag;;

let dag = find' "editor_page.ml" in for i = 1 to 3 do
  let leaves = analyze dag in
   List.iter begin fun g ->
    Printf.printf "%s\n%!" (String.concat ", " g);
    Printf.printf "-----------------------\n%!" ;
  end leaves;
  Printf.printf "************************\n%!" ;
done;;

*)

(** find_top_modules *)
let find_top_modules dir =
  let files = Array.to_list (Sys.readdir dir) in
  let files = List.map (fun x -> dir // x) files in
  let files = List.filter (fun x -> x ^^ ".ml" && not (Sys.is_directory x)) files in
  let search_path = Ocaml_config.expand_includes dir in
  let table = ocamldep ~slash:false ~verbose:false ~search_path (dir // "*.ml") in
  let files = ref files in
  Hashtbl.iter begin fun filename deps ->
    let mldeps = List.filter (fun x -> not (x ^^ ".cmi")) deps in
    let mldeps = List.map (fun x -> (Filename.chop_extension x) ^ ".ml") mldeps in
    let mldeps = List.filter ((<>) filename) mldeps in
    let mldeps = List.map (fun x -> if Filename.is_implicit x then dir // x else x) mldeps in
    files := List.filter (fun x -> not (List.mem x mldeps)) !files;
  end table;
  !files
;;

(*let _ = find_top_modules "C:\\ocaml\\devel\\cpdf\\src";;
let _ = Miscellanea.crono find_top_modules "C:\\ocaml\\devel\\ocamleditor\\src";;*)

