(*

  This file is automatically generated by OCamlEditor 1.15.2-ocaml414, do not edit.

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

open Printf

let packages = [
  (** gmisclib *)
  "gmisclib","\
version         = \"1.16.0-ocaml414\"
description     = \"Miscellaneous widgets based on LablGtk2.\"
requires        = \"lablgtk2\"
archive(byte)   = \"gmisclib.cma\"
archive(native) = \"gmisclib.cmxa\"",
  [
    "gmisclib.cma";
    "gmisclib.cmxa"
  ], [
    "gmisclib/gmisclib.cmi"
  ], [
    
  ];
]

let is_mingw = List.exists ((=) "system: mingw") (get_command_output "ocamlc -config")

let _ = 
  if Array.length Sys.argv < 2 then failwith "Invalid parameters";
  let cwd = Sys.getcwd() in
  Sys.chdir "src";
  let lib_ext = if Sys.win32 && not is_mingw then ".lib" else ".a" in
  List.iter begin fun (name, defs, archives, (cmis : string list), (mlis : string list)) ->
    let cmxas = List.filter (fun x -> Filename.check_suffix x ".cmxa") archives in
    let libs = String.concat " " (List.map (fun x -> (Filename.chop_extension x) ^ lib_ext) cmxas) in
    let arcs = String.concat " " archives in
    let cmis = String.concat " " cmis in
    let mlis = String.concat " " mlis in
    let _META = "META" in
    if Sys.file_exists _META then failwith "Cannot write META file: file exists";
    let chan = open_out_bin _META in
    try
      let sudo = if Sys.win32 || List.mem Sys.argv.(1) ["print"] then "" else "sudo -E " in
      let cmd = ref [] in
      if not (List.mem Sys.argv.(1) ["install"; "uninstall"; "reinstall"; "print"]) then failwith "Invalid parameters";
      output_string chan defs;
      flush chan;
      if List.mem Sys.argv.(1) ["uninstall"; "reinstall"] then cmd := (sprintf "%socamlfind remove %s" sudo name) :: !cmd;
      if List.mem Sys.argv.(1) ["install"; "reinstall"; "print"] then 
        cmd := (sprintf "%socamlfind install %s %s %s %s %s %s" sudo name _META arcs libs cmis mlis) :: !cmd;
      assert (!cmd <> []);
      let cmd = String.concat " && " (List.rev !cmd) in
      if List.mem Sys.argv.(1) ["print"] then printf "\n##### %s.%s #####\n\n%s\n\n# %s\n%!" _META name defs cmd;
      let exit_code = if Sys.argv.(1) = "print" then 0 else Sys.command cmd in
      if exit_code <> 0 then eprintf "Error: command %s exited with code %d\n%!" cmd exit_code;
      if Sys.file_exists _META then (close_out_noerr chan; Sys.remove _META)
    with ex -> begin
      if Sys.file_exists _META then (close_out_noerr chan; Sys.remove _META)
    end
  end packages;
  Sys.chdir cwd;
