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
open Arg
open Oebuild
open Miscellanea

(** main *)
let main () = begin
  let target = ref [] in
  let is_library = ref false in
  let is_clean = ref false in
  let is_clean_all = ref false in
  let compilation = ref [] in
  let prof = ref false in
  let includes = ref "" in
  let cflags = ref "" in
  let lflags = ref "" in
  let libs = ref "" in
  let mods = ref "" in
  let install = ref None in
  let compile_only = ref false in
  let annot = ref false in
  let pp = ref "" in
  let output_name = ref "" in
  let run_code = ref None in
  let run_args = ref [] in
  let ms_paths = ref false in
  let print_output_name = ref false in
  let thread = ref false in
  let vmthread = ref false in
  let no_build = ref false in
  let add_target x = target := x :: !target in
  let add_compilation x () = compilation := x :: !compilation in
  let set_run_args x = run_args := x :: !run_args in
  let set_run_code x () = run_code := Some x in
  let set_install x = install := Some x in
  let dep () =
    try
      let deps = Dep.find ~pp:!pp (*~includes:!includes*) ~with_errors:true !target in
      printf "%s\n%!" (String.concat " " deps);
      exit 0;
    with Dep.Loop_found msg -> begin
      printf "%s\n%!" msg;
      exit 0;
    end
  in
  let speclist = Arg.align [
    ("-byt",         Unit (add_compilation Bytecode), " Bytecode compilation (default).");
    ("-opt",         Unit (add_compilation Native),   " Native-code compilation.");
    ("-c",           Set compile_only,                " Compile only.");
    ("-a",           Set is_library,                  " Build a library. ");
    ("-I",           Set_string includes,             "\"<includes>\" Includes, separated by spaces.");
    ("-l",           Set_string libs,                 "\"<libs>\" Libraries, separated by spaces (e.g. -l \"unix str\"). When the library name ends with \".o\" is interpreted as compiled object file, not as library.");
    ("-m",           Set_string mods,                 "\"<objects>\" Other required object files.");
    ("-cflags",      Set_string cflags,               "\"<flags>\" Flags passed to the compiler.");
    ("-lflags",      Set_string lflags,               "\"<flags>\" Flags passed to the linker.");
    ("-thread",      Set thread,                      " Add -thread option to both cflags and lflags.");
    ("-vmthread",    Set vmthread,                    " Add -vmthread option to both cflags and lflags.");
    ("-annot",       Set annot,                       " Add -annot option to cflags.");
    ("-pp",          Set_string pp,                   " Add -pp option to the compiler.");
    ("-o",           Set_string output_name,          "\"<filename>\" Output file name. Extension {.cm[x]a | [.opt][.exe]} is automatically added to the resulting filename according to the -a and -opt options and the type of O.S.");
    ("-run",         Unit (set_run_code Unspecified), " Run the resulting executable (native-code takes precedence) giving each argument after \"--\" on the command line (\"-run --\" for no arguments).");
    ("-run-byt",     Unit (set_run_code Bytecode),    " Run the resulting bytecode executable.");
    ("-run-opt",     Unit (set_run_code Native),      " Run the resulting native-code executable.");
    ("--",           Rest set_run_args,               "<run-args> Command line arguments for the -run, -run-byt or -run-opt options.");
    ("-clean",       Set is_clean,                    " Remove output files appropriate to the target selected and exit.");
    ("-clean-all",   Set is_clean_all,                " Remove all build output and exit.");
    ("-dep",         Unit dep,                        " Print dependencies and exit.");
    ("-output-name", Set print_output_name,           " (undocumented)");
    ("-msvc",        Set ms_paths,                    " (undocumented)");
    ("-no-build",    Set no_build,                    " (undocumented)");
    ("-install",     String set_install,              " (undocumented)");
    ("-prof",        Set prof,                        " (undocumented)");
(*    ("-cs", Set compile_separately, " Compile separately without recompiling unmodified modules.");*)
(*    ("-install", Set_string install, "\"<path>\" Copy the output to the specified directory. When building libraries the path is relative to " ^
      (win32 "%OCAMLLIB%" "$OCAMLLIB") ^ ". Create all non-existing directories.");*)
  ] in
  let command_name = Filename.basename Sys.argv.(0) in
  let help_message = sprintf "\nUsage:\n  %s target.ml... [options]\n\nOptions:" command_name in
  Arg.parse speclist add_target help_message;
  target := List.rev !target;
  if List.length !target = 0 then (invalid_arg "No target was specified");
  (** Compilation mode *)
  let compilation = if !compilation = [] then [Bytecode] else !compilation in
  let compilation = List.sort Pervasives.compare compilation in
  (** print_output_name *)
  if !print_output_name then begin
    List.iter begin fun compilation ->
      let outname = get_output_name ~compilation ~is_library:!is_library
        ~outname:!output_name ~targets:!target in
      printf "%s\n%!" outname;
    end compilation;
    exit 0;
  end;
  (** Clean *)
  if !is_clean || !is_clean_all then begin
    let deps = Dep.find ~pp:!pp (*~includes:!includes*) ~with_errors:true !target in
    List.iter begin fun compilation ->
      clean ~all:!is_clean_all ~compilation ~is_library:!is_library ~outname:!output_name ~targets:!target ~deps ();
    end compilation;
    if !is_clean_all then (clean_all());
    exit 0;
  end;
  (** Build, install and run *)
  let last_outname = ref None in
  let outnames =
    List.map begin fun compilation ->
      let outname = get_output_name ~compilation ~is_library:!is_library ~outname:!output_name ~targets:!target in
      match
        if !no_build then Built_successfully, [] else begin
          let deps = Dep.find ~pp:!pp (* ~includes:!includes*) ~with_errors:true !target in
          (build
            ~compilation
            ~includes:!includes
            ~libs:!libs
            ~other_mods:!mods
            ~is_library:!is_library
            ~compile_only:!compile_only
            ~thread:!thread
            ~vmthread:!vmthread
            ~annot:!annot
            ~pp:!pp
            ~cflags:!cflags
            ~lflags:!lflags
            ~outname
            ~deps
            ~prof:!prof
            ~ms_paths ()), deps
        end
      with
        | Build_failed code, _ -> exit code (*(compilation, None)*)
        | Built_successfully, deps ->
          begin
            match !install with
              | Some path -> install_output ~compilation ~outname ~is_library:!is_library ~deps ~path;
              | _ -> ()
          end;
          last_outname := Some outname;
          (compilation, Some outname)
    end compilation;
  in
  if not !is_library then begin
    try
      begin
        match !run_code with
          | Some Native ->
            (match List.assoc Native outnames with None -> ()
              | Some outname -> run_output ~outname ~args:!run_args)
          | Some Bytecode ->
            (match List.assoc Bytecode outnames with None -> ()
              | Some outname -> run_output ~outname ~args:!run_args)
          | Some Unspecified ->
            (match !last_outname with None -> ()
              | Some outname -> run_output ~outname ~args:!run_args)
          | None -> ()
      end
    with Not_found -> (invalid_arg "-run-opt or -run-byt")
  end;
end

let _ = crono ~label:"Build time" main ()
