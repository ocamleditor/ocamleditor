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


open Utils
open Printf

let filename = "oebuild_script.ml"

module Util = struct
  let header = ref ""

  let replace_header buf =
    let header_re = Str.regexp_string !header in
    Str.replace_first header_re "" (Buffer.contents buf)

  let test () =
    let cmd = sprintf "ocaml %s" filename in
    Printf.printf "Testing %s...\n  %s\n%!" filename cmd;
    let exit_code = Sys.command cmd in
    if exit_code > 0 then failwith "Test failed" else Printf.printf "OK\n%!" ;
end

let create_script () =
  let ochan = open_out_bin filename in
  let finally () = close_out_noerr ochan in
  try
    output_string ochan ("(\x2A\x2A\n\n  This file is automatically generated by OCamlEditor. Do not edit.\n\n\x2A)\n\n");
    output_string ochan "#directory \"+threads\"\n";
    output_string ochan "#load \"str.cma\"\n";
    output_string ochan "#load \"unix.cma\"\n";
    output_string ochan "#load \"threads.cma\"\n";
    output_string ochan "let split re = Str.split (Str.regexp re)\n";
    let modules = [
      "../common/argc";
      "../common/log";
      "../common/shell";
      "../common/ocaml_config";
      "../common/app_config";
      "../common/spawn";
      "../task";
      "../build_script_command";
      "oebuild_util";
      "oebuild_table";
      "oebuild_dag";
      "oebuild_dep";
      "oebuild_dep_dag";
      "oebuild_parallel";
      "oebuild";
      "../build_script_util";
    ] in
    List.iter begin fun name ->
      let buf = Util.replace_header (File_util.read (name ^ ".ml")) in
      let name = Filename.basename name in
      fprintf ochan "module %s = struct " (String.capitalize_ascii name);
      output_string ochan buf;
      output_string ochan "end\n";
    end modules;
    output_string ochan "\nopen Oebuild\nopen Build_script_util\n";
    finally();
    Util.test();
  with ex -> (finally(); raise ex);;

let code_of_script () =
  let buf = File_util.read filename in
  let ochan = open_out_bin filename in
  let finally () = close_out_noerr ochan in
  try
    output_string ochan !Util.header;
    kprintf (output_string ochan) "let code = %S" (Buffer.contents buf);
    finally();
  with ex -> (finally());;

let _ =
  pushd "oebuild";
  Util.header := Buffer.contents (File_util.read (".."//".."//"header"));
  create_script ();
  code_of_script();
  let new_filename = ".."//filename in
  if Sys.file_exists new_filename then (Sys.remove new_filename);
  Sys.rename filename new_filename;
  popd ()










