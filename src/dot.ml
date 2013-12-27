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
open Miscellanea

let round f =
  int_of_float (if f < 0. then f -. 0.5 else f +. 0.5)

let ocamldoc = Ocaml_config.ocamldoc ()

(** mk_ocamldoc_cmd *)
let mk_ocamldoc_cmd ~project ?(dot_include_all=false) ?(dot_reduce=true) ?(dot_types=false) ~outfile sourcefiles =
  let search_path = Project.get_search_path_i_format project in
  sprintf "%s -dot -I +threads %s%s%s%s -o %s %s %s"
    ocamldoc
    search_path
    (if dot_include_all then " -dot-include-all" else "")
    (if dot_reduce then " -dot-reduce" else "")
    (if dot_types then " -dot-types" else "")
    outfile
    (String.concat " " sourcefiles)
    Cmd.redirect_stderr;;

(** mk_dot_cmd *)
let mk_dot_cmd ~outlang ~outfile ?(label="") ?(rotate=0.) filename =
  sprintf "dot -T%s -o %s -Glabel=\"%s\" -Grotate=%.2f %s %s"
    outlang outfile label rotate Oe_config.dot_attributes filename;;

(** draw *)
let draw ~project ~filename ?dot_include_all ?dot_types ?packing ?on_ready_cb () =
  let module Device =
    (val match Oe_config.dot_viewer with
      | `DEFAULT -> !Dot_viewer_plugin.device
      | `PDF -> (module Dot_viewer_pdf.PDF))
  in
  let outlang       = Device.lang in
  let basename      = Filename.basename filename in
  let label         = sprintf "Dependency graph for \xC2\xAB%s\xC2\xBB" basename in
  let prefix = Filename.chop_extension basename in
  let dependencies =
    Oebuild_dep.ocamldep_recursive [filename] |> Oebuild_dep.sort_dependencies |> List.map Oebuild_util.replace_extension_to_ml
  in
  let dependants =
    let path = [Filename.dirname filename] in
    let modname = Miscellanea.modname_of_path filename in
    Oebuild_dep.find_dependants ~path ~modname
  in
  let sourcefiles   = dependencies @ dependants in
  let dotfile       = Filename.temp_file prefix ".dot" in
  let outfile       = dotfile ^ "." ^ outlang in
  (*  *)
  let ocamldoc_cmd  = mk_ocamldoc_cmd ?dot_include_all ?dot_types ~project ~outfile:dotfile sourcefiles in
  let dot_cmd       = mk_dot_cmd ~outlang ~outfile ~label dotfile in
  let viewer        = Device.create ?packing () in
  let activity_name = "Generating module dependency graph, please wait..." in
  Activity.add Activity.Other activity_name;
  ignore (Oebuild_util.exec ~verbose:App_config.application_debug ~join:false ~at_exit:begin fun _ ->
    let modname = Miscellanea.modname_of_path filename in
    let re = kprintf Str.regexp "\"%s\" \\[.+" modname in
    let re1 = Str.regexp "\\(\".*\"\\) \\[style=filled, color=darkturquoise\\];$" in
    map_file_lines dotfile begin fun ~lnum ~line ->
      if Str.string_match re line 0 then (sprintf "\"%s\" [style=filled, color=black, fontcolor=white];\n" modname)
      else if Str.string_match re1 line 0 then (sprintf "%s;\n" (Str.matched_group 1 line))
      else line
    end;
    ignore (Oebuild_util.exec ~join:false ~verbose:App_config.application_debug ~at_exit:begin fun _ ->
      if Sys.file_exists dotfile then (Sys.remove dotfile);
      Device.draw ~filename:outfile viewer;
      Activity.remove activity_name;
      Gaux.may on_ready_cb ~f:(fun cb -> cb viewer);
    end dot_cmd);
  end ocamldoc_cmd);
  viewer;;
