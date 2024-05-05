#load "unix.cma";;
#cd "src";;
#use "../tools/scripting.ml";;
#cd "common";;
#mod_use "./shell.ml";;
#use "./ocaml_config.ml";;

open Printf

let is_mingw = List.exists ((=) "system: mingw") (get_command_output "ocamlc -config");;
let is_native_supported = can_compile_native() <> None;;

let compile () =
  let filename = if Sys.win32 then "terminate_process" else "terminate_process_unix" in
  sys_command ["ocamlc"; filename ^ ".c" ];
  let ext = if Sys.win32 && not is_mingw then "obj" else "o" in
  if is_native_supported then
    sys_command ["ocamlmklib"; (sprintf "%s.%s" filename ext); "process_termination.ml"; "-o process_termination"]
  else begin
    sys_command ["ocamlc"; "-c"; "process_termination.ml"];
    sys_command ["ocamlmklib"; (sprintf "%s.%s" filename ext); "process_termination.cmo"; "-o process_termination"; "-v"]
  end
;;

let _ = main ~default_target:compile ~options:[] ()

