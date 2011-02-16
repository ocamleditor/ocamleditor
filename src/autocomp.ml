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
open Err_types

let (!!) = Filename.quote
let re_tmp = Str.regexp "File \"..[\\/]tmp[\\/]"

(** replace_output_file *)
let replace_output_file project tmp rel_filename ext =
  let rel_file_ext = rel_filename ^ ext in
  let tmp_ext = tmp // rel_file_ext in
  if Sys.file_exists tmp_ext then begin
    let src_ext = (Project.path_src project) // rel_file_ext in
    try
      if Sys.file_exists src_ext then (Sys.remove src_ext);
      Sys.rename tmp_ext src_ext
    with Sys_error _ as ex ->
      Printf.eprintf "File \"autocomp.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  end

(** compile_buffer *)
let rec compile_buffer ~project ~filename ~text ?(commit=false) ~error_indication () =
  try
    (*Prf.crono Prf.prf_compile_buffer begin fun () ->*)
    let tmp = Project.path_tmp project in
    match project.Project.in_source_path filename with
      | None -> ()
      | Some rel_name ->
        let tmp_filename = (tmp // rel_name) in
        Miscellanea.mkdir_p (Filename.dirname tmp_filename);
        let chan = open_out_bin tmp_filename in
        lazy (output_string chan text) /*finally*/ lazy (close_out chan);
        (* Compile *)
        let includes = Project.get_includes project in
        let command = sprintf "%s %s -I ../tmp %s ../tmp/%s"
          project.Project.autocomp_compiler
          project.Project.autocomp_cflags
          (let includes = String.concat " -I " includes in if includes = "" then "" else "-I " ^ includes)
          rel_name
        in
        let compiler_output = Buffer.create 101 in
        let process_err ~stderr =
          let line = input_line stderr in
          let line = Str.replace_first re_tmp "File \"" line in
          Buffer.add_string compiler_output (Miscellanea.rtrim line);
          Buffer.add_char compiler_output '\n';
        in
        let at_exit () =
          (* Replace output files and update the table of compilation times of oebuild *)
          let rel_filename = match project.Project.in_source_path filename with
            | Some x -> Filename.chop_extension x | _ -> assert false
          in
          replace_output_file project tmp rel_filename ".annot";
          replace_output_file project tmp rel_filename ".cmi";
    (*      if false && commit && exit_code = 0 then begin
            let current_time = (Unix.time () +. 1.) in
            replace_output_file project basename ".cmo";
            let oebuild_times = Oebuild.read_cache () in
            Hashtbl.remove oebuild_times basename;
            Hashtbl.add oebuild_times basename (current_time(*, false*));
            Oebuild.write_cache oebuild_times;
          end;*)
          let errors = Error.parse_string (Buffer.contents compiler_output) in
          GtkThread2.async error_indication#apply_tag errors
        in
        let exit_code = Oebuild_util.exec ~echo:true ~join:false ~at_exit ~process_err command in ()
    (*end ()*)
  with ex -> begin
    eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ());
    (*Error.empty*) ()
  end















