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

open Miscellanea
open Printf

let find_best_compiler compilers =
  try
    let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null" in
    List.find begin fun comp ->
      try ignore (kprintf expand "%s -version%s" comp redirect_stderr); true with _ -> false
    end compilers
  with Not_found ->
    kprintf failwith "Cannot find compilers: %s" (String.concat ", " compilers)

let find_tool which path =
  let commands =
    match which with
      | `BEST_OCAMLC -> ["ocamlc.opt"; "ocamlc"]
      | `BEST_OCAMLOPT -> ["ocamlopt.opt"; "ocamlopt"]
      | `BEST_OCAMLDEP -> ["ocamldep.opt"; "ocamldep"]
      | `OCAMLC -> ["ocamlc"]
      | `OCAML -> ["ocaml"]
  in
  let quote    = if path <> "" && Sys.os_type = "Win32" then Filename.quote else (fun x -> x) in
  let path     = if path <> "" then path // "bin" else "" in
  find_best_compiler (List.map quote (List.map ((//) path) commands))

let get_home () = try Sys.getenv "OCAML_HOME" with Not_found -> ""

let expand_includes compact =
  if String.length compact > 0 then
    ("-I " ^ (String.concat " -I " (Miscellanea.split " +" compact))) else ""

(** OCaml Tools path *)

let ocamlc () = find_tool `BEST_OCAMLC (get_home ())
let ocamlopt () = find_tool `BEST_OCAMLOPT (get_home ())
let ocamldep () = find_tool `BEST_OCAMLDEP (get_home ())
let ocaml () = find_tool `OCAML (get_home ())
let ocamllib () = Miscellanea.expand ~first_line:true ((ocamlc()) ^ " -where")

(** OCaml Version *)

let ocaml_version ?(compiler=ocamlc()) () =
  let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null" in
  Miscellanea.expand (compiler ^ " -v " ^ redirect_stderr)

(** can_compile_native *)
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









