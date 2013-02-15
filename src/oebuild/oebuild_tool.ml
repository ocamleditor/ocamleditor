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
open Arg
open Oebuild

(** main *)
let main () = begin
  let enabled = ref true in
  let nodep = ref false in
  let target = ref [] in
  let outkind = ref Executable in
  let is_clean = ref false in
  let is_distclean = ref false in
  let compilation = ref [] in
  let prof = ref false in
  let package = ref "" in
  let includes = ref "" in
  let cflags = ref "" in
  let lflags = ref "" in
  let libs = ref "" in
  let mods = ref "" in
  let install_flag = ref None in
  let compile_only = ref false in
  let annot = ref false in
  let bin_annot = ref false in
  let pp = ref "" in
  let inline : int option ref = ref None in
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
  let set_install x = install_flag := Some x in
  let dep () =
    try
      let deps = Dep.find ~pp:!pp ~with_errors:true !target in
      printf "%s\n%!" (String.concat " " deps);
      exit 0;
    with Dep.Loop_found msg -> begin
      printf "%s\n%!" msg;
      exit 0;
    end
  in
  let check_restrictions restr =
    let restr = Str.split (Str.regexp "[ ,]+") restr in
    enabled := check_restrictions restr
  in
  let speclist = Arg.align [
    ("-byt",         Unit (add_compilation Bytecode), " Bytecode compilation (default).");
    ("-opt",         Unit (add_compilation Native),   " Native-code compilation.");
    ("-c",           Set compile_only,                " Compile only.");
    ("-a",           Unit (fun _ -> outkind := Library), " Build a library. ");
    ("-shared",      Unit (fun _ -> outkind := Plugin), " Build a plugin. ");
    ("-pack",        Unit (fun _ -> outkind := Pack), " Pack object files.");
    ("-package",     Set_string package,              "\"<package-name-list>\" Option passed to ocamlfind.");
    ("-I",           Set_string includes,             "\"<paths>\" Search paths, separated by spaces.");
    ("-l",           Set_string libs,                 "\"<libs>\" Libraries, separated by spaces (e.g. -l \"unix str\").
                                 When the library name ends with \".o\" is interpreted
                                 as compiled object file, not as library.");
    ("-m",           Set_string mods,                 "\"<objects>\" Other required object files.");
    ("-cflags",      Set_string cflags,               "\"<flags>\" Flags passed to the compiler.");
    ("-lflags",      Set_string lflags,               "\"<flags>\" Flags passed to the linker.");
    ("-thread",      Set thread,                      " Add -thread option to both cflags and lflags.");
    ("-vmthread",    Set vmthread,                    " Add -vmthread option to both cflags and lflags.");
    ("-annot",       Set annot,                       " Add -annot option to cflags.");
    ("-bin-annot",   Set bin_annot,                   " Add -bin-annot option to cflags.");
    ("-pp",          Set_string pp,                   " Add -pp option to the compiler.");
    ("-inline",      Int (fun x -> inline := Some x), " Add -inline option to the compiler.");
    ("-o",           Set_string output_name,          "\"<filename>\" Output file name. Extension {.cm[x]a | [.opt][.exe]}
                                 is automatically added to the resulting filename according
                                 to the -a and -opt options and the type of O.S.");
    ("-run",         Unit (set_run_code Unspecified), " Run the resulting executable (native-code takes precedence) giving
                                 each argument after \"--\" on the command line
                                 (\"-run --\" for no arguments).");
    ("-run-byt",     Unit (set_run_code Bytecode),    " Run the resulting bytecode executable.");
    ("-run-opt",     Unit (set_run_code Native),      " Run the resulting native-code executable.");
    ("--",           Rest set_run_args,               "<run-args> Command line arguments for the -run, -run-byt or -run-opt options.");
    ("-clean",       Set is_clean,                    " Remove output files for the selected target and exit.");
    ("-dep",         Unit dep,                        " Print dependencies and exit.");
    ("-no-dep",      Set nodep,                       " Do not detect module dependencies.");
    ("-when",        String check_restrictions,       "\"<c1,c2,...>\" Exit immediately if any condition specified here is not true.
                                 Recognized conditions are:

                                   IS_UNIX    : O.S. type is Unix
                                   IS_WIN32   : O.S. type is Win32
                                   IS_CYGWIN  : O.S. type is Cygwin
                                   HAS_NATIVE : Native compilation is supported\n");
    ("-output-name", Set print_output_name,           " (undocumented)");
    ("-msvc",        Set ms_paths,                    " (undocumented)");
    ("-no-build",    Set no_build,                    " (undocumented)");
    ("-install",     String set_install,              " (undocumented)");
    ("-prof",        Set prof,                        " (undocumented)");
    ("-distclean",   Set is_distclean,                " (undocumented)");
(*    ("-cs", Set compile_separately, " Compile separately without recompiling unmodified modules.");*)
(*    ("-install", Set_string install, "\"<path>\" Copy the output to the specified directory. When building libraries the path is relative to " ^
      (win32 "%OCAMLLIB%" "$OCAMLLIB") ^ ". Create all non-existing directories.");*)
  ] in
  let command_name = Filename.basename Sys.argv.(0) in
  let help_message = sprintf "\nUsage:\n  %s target.ml... [options]\n\nOptions:" command_name in
  Arg.parse speclist add_target help_message;
  target := List.rev !target;
  if List.length !target = 0 then begin
    Arg.usage speclist help_message;
    exit 0;
  end;
  if not !enabled then (exit 0);
  (** Compilation mode *)
  let compilation = if !compilation = [] then [Bytecode] else !compilation in
  let compilation = List.sort Pervasives.compare compilation in
  (** print_output_name *)
  if !print_output_name then begin
    List.iter begin fun compilation ->
      let outname = get_output_name ~compilation ~outkind:!outkind ~outname:!output_name ~targets:!target in
      match outname with Some outname -> printf "%s\n%!" outname | _ -> ();
    end compilation;
    exit 0;
  end;
  (** Clean *)
  if !is_clean || !is_distclean then begin
    let deps = Dep.find ~pp:!pp ~with_errors:true !target in
    if !is_clean then (clean ~deps ());
    if !is_distclean then (distclean ());
    exit 0;
  end;
  (** Build, install and run *)
  let last_outname = ref None in
  let outnames =
    List.map begin fun compilation ->
      let outname = get_output_name ~compilation ~outkind:!outkind ~outname:!output_name ~targets:!target in
      match outname with
        | Some outname ->
          begin
            match
              if !no_build then Built_successfully, []
              else begin
                let deps =
                  if !nodep then !target else Dep.find ~pp:!pp ~with_errors:true !target
                in
                (build
                  ~compilation
                  ~package:!package
                  ~includes:!includes
                  ~libs:!libs
                  ~other_mods:!mods
                  ~outkind:!outkind
                  ~compile_only:!compile_only
                  ~thread:!thread
                  ~vmthread:!vmthread
                  ~annot:!annot
                  ~bin_annot:!bin_annot
                  ~pp:!pp
                  ?inline:!inline
                  ~cflags:!cflags
                  ~lflags:!lflags
                  ~outname
                  ~deps
                  ~targets:!target
                  ~prof:!prof
                  ~ms_paths ()), deps
              end
            with
              | Build_failed code, _ -> exit code (*(compilation, None)*)
              | Built_successfully, deps ->
                begin
                  match !install_flag with
                    | Some path -> install ~compilation ~outname ~outkind:!outkind ~deps ~path
                      ~ccomp_type:(Ocaml_config.can_compile_native ());
                    | _ -> ()
                end;
                last_outname := Some outname;
                (compilation, Some outname)
          end
        | _ -> (compilation, None)
    end compilation;
  in
  if !outkind = Executable then begin
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

let _ = Oebuild_util.crono ~label:"Build time" main ()
