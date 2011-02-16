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

let create ~proj =
  let bconfigs = proj.Project.build in
  ()

let can_compile_native () =
  let ccopt =
    match Sys.os_type with
      | "Win32" -> ""
        (*"-ccopt \"-LC:\\Programmi\\MIC977~1\\Lib -LC:\\Programmi\\MID05A~1\\VC\\lib\""*)
      | _ -> ""
  in
  let result = ref false in
  let filename = "test_native.ml" in
  let ochan = open_out filename in
  begin
    try
      output_string ochan ("0");
      close_out ochan
    with _ -> (close_out ochan)
  end;
  let outname = Filename.chop_extension filename in
  let cmd = sprintf "ocamlopt -o %s %s %s" outname ccopt filename in
  result := (Sys.command cmd) = 0;
  Sys.remove filename;
  if Sys.file_exists outname then (Sys.remove outname);
  let cmi = outname ^ ".cmi" in
  if Sys.file_exists cmi then (Sys.remove cmi);
  let cmx = outname ^ ".cmx" in
  if Sys.file_exists cmx then (Sys.remove cmx);
  let obj = outname ^ ".o" in
  if Sys.file_exists obj then (Sys.remove obj);
  let obj = outname ^ ".obj" in
  if Sys.file_exists obj then (Sys.remove obj);
  !result



