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


open Printf

let (!$) = Filename.chop_extension
let (//) = Filename.concat
let (^^^) = Filename.check_suffix
let (<@) = List.mem
let win32 = (fun a b -> match Sys.os_type with "Win32" -> a | _ -> b)
let may opt f = match opt with Some x -> f x | _ -> ()
let re_spaces = Str.regexp " +"
let redirect_stderr_to_null = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null"

(** format_int *)
let format_int n =
  let n = string_of_int n in
  let i = ref (String.length n - 3) in
  let res = ref "" in
  while !i >= 0 do
    res := (if !i > 0 then "," else "") ^ (String.sub n !i 3) ^ !res;
    i := !i - 3;
  done;
  (String.sub n 0 (!i + 3)) ^ !res;;

let unquote =
  let re = Str.regexp "^\"\\(.*\\)\"$" in
  fun x -> if Str.string_match re x 0 then Str.matched_group 1 x else x

let split_space = Str.split re_spaces

(** lpad *)
let lpad txt c width =
  let result = (String.make width c) ^ txt in
  String.sub result (String.length result - width) width;;

(** rpad *)
let rpad txt c width =
  let result = txt ^ (String.make width c) in
  String.sub result 0 width;;

(** dot_leaders *)
let dot_leaders ?(prefix="") ?(postfix="") ?(right_align=false) properties =
  let prefix_length = String.length prefix in
  let maxl =
    List.fold_left begin fun maxl (x, _) ->
      let len = prefix_length + String.length x in
      max maxl len
    end 0 properties
  in
  let lpad x =
    if right_align then
      let len =
        List.fold_left begin fun maxr (_, y) ->
          let len = String.length y in
          max maxr len
        end 0 properties
      in lpad x ' ' len
    else x
  in
  List.map (fun (n, v) -> Printf.sprintf "%s : %s%s" (rpad (prefix ^ n ^ " ") '.' maxl) (lpad v) postfix) properties
;;

(** crono *)
let crono ?(label="Time") f x =
  let finally time =
    Printf.fprintf stdout "[CRONO] %s: %f sec." label (Unix.gettimeofday() -. time);
    print_newline();
  in
  let time = Unix.gettimeofday() in
  let result = try f x with e -> begin
    finally time;
    raise e
  end in
  finally time;
  result

(** remove_dupl *)
let remove_dupl l =
  List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)

(** remove_file *)
let remove_file ?(verbose=false) filename =
  try
    if Sys.file_exists filename then (Sys.remove filename; if verbose then print_endline filename)
  with Sys_error ex -> eprintf "%s\n%!" ex

(** command *)
let command ?(echo=true) cmd =
  if echo then (printf "%s\n%!" cmd);
  let exit_code = Sys.command cmd in
  Stdlib.flush stderr;
  Stdlib.flush stdout;
  exit_code

(** Remove files with wildcards *)
let rm = win32 "DEL /F /Q" "rm -f"

(** Copy file *)
let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let cp ?(echo=true) src dst =
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  if echo then (printf "%s -> %s\n%!" src dst);
  let finally () = close_out oc; close_in ic in
  try copy_file ic oc; finally() with ex -> (finally(); raise ex)

(** mkdir *)
let rec mkdir_p d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    printf "mkdir -p %s\n%!" d;
    (Unix.mkdir d 0o755)
  end

(*(** replace_extension *)
let replace_extension x =
  sprintf "%s.%s" (Filename.chop_extension x)
    (if x ^^ "cmi" then "mli" else if x ^^ "cmx" then "ml" else assert false);;*)

(** replace_extension_to_ml *)
let replace_extension_to_ml filename =
  if Filename.check_suffix filename ".cmx" then (Filename.chop_extension filename) ^ ".ml"
  else if Filename.check_suffix filename ".cmi" then (Filename.chop_extension filename) ^ ".mli"
  else filename
;;

(** split_prog_args *)
let split_prog_args x =
  match split_space x with
    | h :: t -> h, Array.of_list t
    | _ -> assert false

(** get_effective_command *)
let get_effective_command =
  let re_verbose = Str.regexp " -verbose" in
  fun ?(linkpkg=false) ocamlfind ->
    try
      let cmd = sprintf "%s%s -verbose %s" ocamlfind (if linkpkg then " -linkpkg" else "") redirect_stderr_to_null in
      let lines = Shell.get_command_output cmd in
      let effective_compiler = List.find (fun line -> String.sub line 0 2 = "+ ") lines in
      let effective_compiler = Str.string_after effective_compiler 2  in
      let effective_compiler = Str.replace_first re_verbose "" effective_compiler in
      let a, b = split_prog_args effective_compiler in
      (if Sys.win32 then a ^ ".exe" else a), b
    with Not_found -> split_prog_args ocamlfind
;;

