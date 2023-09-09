(*

  This file is automatically generated by OCamlEditor 1.14.3-ocaml414, do not edit.

*)

#load "unix.cma"
#load "str.cma"
open Printf

let unquote =
  let re = Str.regexp "^\"\\(.*\\)\"$" in
  fun x -> if Str.string_match re x 0 then Str.matched_group 1 x else x

let get_command_output command =
  let ch = Unix.open_process_in command in
  set_binary_mode_in ch false;
  let output = ref [] in
  try
    while true do output := (input_line ch) :: !output done;
    assert false
  with End_of_file -> begin
    ignore (Unix.close_process_in ch);
    List.rev !output
  end | e -> begin
    ignore (Unix.close_process_in ch);
    raise e
  end

let is_mingw = List.exists ((=) "system: mingw") (get_command_output "ocamlc -config")

let _ = if not Sys.win32 || is_mingw then exit 0

let _ = Printf.kprintf Sys.command "editbin %S /subsystem:windows 2>&1 1>NUL" Sys.argv.(1)
