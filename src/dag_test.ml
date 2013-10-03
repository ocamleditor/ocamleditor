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

(*

#load "C:\\ocaml\\lib\\str.cma";;
#load "C:\\ocaml\\lib\\unix.cma";;
#directory "C:\\ocaml\\lib\\threads";;
#load "C:\\ocaml\\lib\\threads\\threads.cma";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\cmd.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\app_config.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\ocaml_config.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\miscellanea.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\file_util.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\dep.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\dep_ext.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\dag.cmo";;
#directory "C:\\ocaml\\devel\\ocamleditor\\src\\common";;
#directory "C:\\ocaml\\devel\\ocamleditor\\src\\oebuild";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\oebuildlib.cma";;

*)

open Printf

module COMP = struct
  type key = string
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
  let to_string x = x
end

module Dag = Dag.Make(COMP)

let create filename =
  let open Dag in
  let dag' = Dep_ext.find' filename in
  let dag = Hashtbl.create 17 in
  Dep_ext.iter begin fun node deps ->
    Hashtbl.add dag node { key = node; node; dependencies = []; dependants = [] }
  end dag';
  Dep_ext.iter begin fun node deps ->
    try
      let node = Hashtbl.find dag node in
      List.iter begin fun dep ->
        let e = Hashtbl.find dag dep in
        node.dependencies <- e :: node.dependencies;
      end deps;
    with Not_found -> assert false
  end dag';
  set_dependants dag;
  dag;;


let test () =
  let dag = create "ocamleditor.ml" in
  let leaves = ref (Dag.get_leaves dag) in
  Printf.printf "==> %d/%d\n%!" (List.length !leaves) (Dag.length dag);
  while !leaves <> [] do
    let text = String.concat "," (List.map (fun e -> e.Dag.node) !leaves) in
    Printf.printf "%s\n%!" text;
    (*List.iter begin fun leaf ->
      let filename = (Filename.chop_extension leaf.Dag.node) ^ ".ml" in
      let filename = Filename.concat "C:\\ocaml\\devel\\ocamleditor\\src" filename in
      kprintf (Oebuild_util.exec ~verbose:true ~join:false) "ocamlc %s" filename |> ignore
    end !leaves;*)
    List.iter (Dag.remove_leaf dag) !leaves;
    leaves := Dag.get_leaves dag;
    Printf.printf "==> %d/%d\n%!" (List.length !leaves) (Dag.length dag);
  done;;


Miscellanea.crono test ()



