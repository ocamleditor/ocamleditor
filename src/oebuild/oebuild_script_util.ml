(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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


open Arg
open Printf
open Oebuild
open Oebuild_util
open Task

type target = {
  num : int;
  id : int;
  output_name : string;
  target_type : Oebuild.output_type;
  compilation_bytecode : bool;
  compilation_native : bool;
  toplevel_modules : string;
  package : string;
  search_path : string;
  required_libraries : string;
  compiler_flags : string;
  linker_flags : string;
  thread : bool;
  vmthread : bool;
  pp : string;
  inline : int option;
  library_install_dir : string;
  other_objects : string;
  external_tasks : int list;
  restrictions : string list;
  dependencies : int list;
  show : bool;
  (*installer_task : int option;*)
}

type target_map_entry = int * (string * target)

exception Error

let pushd, popd =
  let stack = Stack.create () in
  begin fun dir ->
    let cwd = Sys.getcwd () in
    Stack.push cwd stack;
    Sys.chdir dir
  end, (fun () -> Sys.chdir (Stack.pop stack));;

let rpad txt c width =
  let result = txt ^ (String.make width c) in
  String.sub result 0 width;;

let get_compilation_types native t =
  (if t.compilation_bytecode then [Bytecode] else []) @ (if t.compilation_native && native then [Native] else [])

let string_of_compilation_type native t =
  let compilation = get_compilation_types native t in
  String.concat "/" (List.map string_of_compilation_type compilation)

let create_target ?dir f x =
  (match dir with Some dir -> pushd dir | _ -> ());
  f x;
  (match dir with Some _ -> popd() | _ -> ());;

let create_target_func ?tg targets =
  match targets with
    | default_target :: _ ->
      (match tg with Some f -> create_target f | _ -> create_target default_target)
    | [] -> fun _ -> ();;

let string_of_output_type = function
  | Executable -> "Executable"
  | Library -> "Library"
  | Plugin -> "Plugin"
  | Pack -> "Pack";;

(** system_config *)
let ccomp_type = Ocaml_config.can_compile_native ()

let system_config () =
  let ocaml_version = Cmd.expand ~first_line:true "ocamlc -v" in
  let std_lib = Cmd.expand ~first_line:true "ocamlc -where" in
  let properties = [
    "OCaml", ocaml_version;
    "Standard library directory", std_lib;
    "OCAMLLIB", (try Sys.getenv "OCAMLLIB" with Not_found -> "<Not_found>");
    "Native compilation supported", (match ccomp_type with Some ccomp_type -> ccomp_type | _ -> "No");
  ] in
  let buf = Buffer.create 100 in
  Buffer.add_string buf "\nSystem configuration\n";
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
  List.iter (fun (n, v) -> bprintf buf "  %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;
  Buffer.contents buf;;

(** Options *)
module Option = struct
  let prefix = ref ""
  let change_dir = ref "src"
end

(** ETask *)
module ETask = struct
  let filter tasks phase =
    List.filter begin fun task ->
      if task.et_always_run_in_script then
        match task.et_phase with
          | Some ph -> ph = phase
          | _ -> false
      else false
    end tasks;;

  let execute = Task.handle begin fun ~env ~dir ~prog ~args ->
    let cmd = sprintf "%s %s" prog (String.concat " " args) in
    let old_dir = Sys.getcwd () in
    Sys.chdir dir;
    let exit_code = Oebuild_util.exec ~env cmd in
    Sys.chdir old_dir;
    if exit_code > 0 then raise Error
  end
end

(** add_target *)
let target : target_map_entry list ref = ref []

let add_target targets name =
  try
    begin
      try
        let num = int_of_string name in
        if num <= 0 then (raise Exit);
        let name_tg = try List.find (fun (_, tg) -> tg.num = num) targets with Not_found -> raise Exit in
        target := (num, name_tg) :: !target;
      with _ ->
        let tg = List.assoc name targets in
        if tg.num <= 0 then (raise Exit);
        target := (tg.num, (name, tg)) :: !target;
    end
  with Exit | Not_found -> (raise (Arg.Bad (sprintf "Invalid target `%s'" name)));;

(** find_target_dependencies *)
let rec find_target_dependencies targets trg =
  remove_dupl (List.flatten (List.map begin fun id ->
    try
      let _, target = List.find (fun (_, tg) -> tg.id = id) targets in
      (find_target_dependencies targets target) @ [target]
    with Not_found -> []
  end trg.dependencies));;

(** Show *)
let show = fun targets -> function num, (name, t) ->
  let files = Str.split (Str.regexp " +") t.toplevel_modules in
  (*let deps = Dep.find ~pp:t.pp ~with_errors:true ~echo:false files in*)
  let b_deps = find_target_dependencies targets t in
  let b_deps = List.map begin fun tg ->
    let name, _ = List.find (fun (_, t) -> t.id = tg.id) targets in
    name
  end b_deps in
  let compilation =
    (if t.compilation_bytecode then [Bytecode] else []) @
    (if t.compilation_native && ccomp_type <> None then [Native] else [])
  in
  let outname = List.map begin fun compilation ->
    let oname = get_output_name ~compilation ~outkind:t.target_type ~outname:t.output_name ~targets:files in
    match oname with Some x -> x | _ -> ""
  end compilation
  in
  let outkind = string_of_output_type t.target_type in
  let compilation = string_of_compilation_type (ccomp_type <> None) t in
  let prop_1 = [
    "Restrictions", (String.concat "," t.restrictions);
    "Output name", (String.concat ", " outname);
  ] in
  let prop_2 = [
    "Findlib packages", t.package;
    "Search path", t.search_path;
    "Required libraries", t.required_libraries;
    "Compiler flags", t.compiler_flags;
    "Linker flags", t.linker_flags;
    "Toplevel modules", t.toplevel_modules;
    "Target dependencies", (String.concat ", " b_deps);
  ] in
  let properties = if t.target_type = Library then prop_1 @ [
    "Install directory", (Oebuild.ocamllib // t.library_install_dir)
  ] @ prop_2 else prop_1 @ prop_2 in
  printf "%2d) %s (%s, %s)\n%!" num name outkind compilation;
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
  List.iter (fun (n, v) -> printf "  %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;;

(** install *)
let install ~compilation ~outname ~external_tasks ~deps target =
  match target.target_type with
    | Library ->
      let deps = deps() in
      Oebuild.install ~compilation ~outkind:target.target_type ~outname ~deps ~path:target.library_install_dir ~ccomp_type
    | Executable (*->
      begin
        match target.installer_task with
          | Some installer_task ->
            let _, f = List.find (fun (num, _) -> num = installer_task) external_tasks in
            ETask.execute (f ())
          | _ -> eprintf "Command \"install\" is not available for target \"%d\"" target.num
      end;
      raise Exit;*)
    | Plugin | Pack ->
      eprintf "\"install\" not implemented for Executable, Plugin or Pack.";
      raise Exit;;

(** execute_target *)
let rec execute_target ~external_tasks ~targets ~command target =
  if Oebuild.check_restrictions target.restrictions then
    let compilation = (if target.compilation_bytecode then [Bytecode] else [])
      @ (if target.compilation_native && (ccomp_type <> None) then [Native] else []) in
    let files = Str.split (Str.regexp " +") target.toplevel_modules in
    let deps () = Dep.find ~pp:target.pp ~with_errors:true ~echo:false files in
    let etasks = List.map begin fun name ->
      let mktarget = try List.assoc name external_tasks with Not_found -> assert false in
      mktarget()
    end target.external_tasks in
    try
      List.iter begin fun compilation ->
        match get_output_name ~compilation ~outkind:target.target_type ~outname:target.output_name ~targets:files with
          | Some outname ->
            begin
              match command with
                | `Build -> build ~targets ~external_tasks ~etasks ~deps ~compilation ~outname ~files target
                | `Install -> install ~compilation ~outname ~external_tasks ~deps target
                | `Clean ->
                  List.iter ETask.execute (ETask.filter etasks Before_clean);
                  let deps = deps() in
                  Oebuild.clean ~deps ();
                  List.iter ETask.execute (ETask.filter etasks After_clean);
                | `Distclean ->
                  if files <> [] then (Oebuild_util.remove_file ~verbose:false outname);
                | `Show -> assert false
            end
          | _ -> ()
      end compilation
    with Exit -> ()

(** build *)
and build ~targets ~external_tasks ~etasks ~deps ~compilation ~outname ~files target =
  let b_deps = find_target_dependencies targets target in
  List.iter (execute_target ~external_tasks ~targets ~command:`Build) b_deps;
  List.iter ETask.execute (ETask.filter etasks Before_compile);
  let deps = deps() in
  (*List.iter ETask.execute (ETask.filter etasks Compile);*)
  match Oebuild.build
    ~compilation
    ~package:target.package
    ~includes:target.search_path
    ~libs:target.required_libraries
    ~other_mods:target.other_objects
    ~outkind:target.target_type
    ~compile_only:false
    ~thread:target.thread
    ~vmthread:target.vmthread
    ~annot:false
    ~pp:target.pp
    ?inline:target.inline
    ~cflags:target.compiler_flags
    ~lflags:target.linker_flags
    ~outname
    ~deps
    ~ms_paths:(ref false)
    ~targets:files ()
  with
    | Built_successfully ->
      List.iter ETask.execute (ETask.filter etasks After_compile);
    | Build_failed n -> popd(); exit n
;;

(** main *)
let main ~cmd_line_args ~external_tasks ~general_commands ~targets =
  let module Command = struct
    type t = [`Show | `Build | `Install | `Clean | `Distclean]

    exception Unrecognized_command of string

    let string_of_command = function
      | `Show -> "show"
      | `Build -> "build"
      | `Install -> "install"
      | `Clean -> "clean"
      | `Distclean -> "distclean";;

    let command_of_string = function
      | "show" -> `Show
      | "build" -> `Build
      | "install" -> `Install
      | "clean" -> `Clean
      | "distclean" -> `Distclean
      | x -> raise (Unrecognized_command (sprintf "`%s' is not a recognized command." x));;

    let options =
      List.map (fun (a, b, c, d) -> a, Arg.align b, c, d) [
        `Show,      [], "Show the build options of a target",          "";
        `Build,     [], "Build libraries and executables",             "";
        `Install,   [], "Install libraries and executables",           "";
        `Clean,     [], "Remove output files for the selected target", "";
        `Distclean, [], "Remove all build output",                     "";
      ];;

    let anon_fun = function
      | `Show -> add_target targets
      | `Build -> add_target targets
      | `Install -> add_target targets
      | `Clean -> add_target targets
      | `Distclean as x ->
        fun arg -> kprintf failwith "Invalid anonymous argument `%s' for command `%s'" arg (string_of_command x);;

    (** execute *)
    let execute command =
      pushd !Option.change_dir;
      let target = List.rev !target in
      try
        begin
          match command with
            | `Distclean ->
              List.iter (fun (_, t) -> execute_target ~external_tasks ~targets ~command t) targets;
              Oebuild.distclean();
              begin
                try
                  let task = List.assoc (List.assoc `Distclean general_commands) external_tasks in
                  ETask.execute (task ())
                with Not_found -> ()
              end;
            (*| `Install ->
              begin
                try
                  let task = List.assoc (List.assoc `Install general_commands) external_tasks in
                  ETask.execute (task ())
                with Not_found -> ()
              end;*)
            | `Show ->
              printf "%s\n%!" (system_config ());
              Printf.printf "\n%!" ;
              if target = [] then (raise (Arg.Bad "show: no target specified"));
              List.iter begin fun t ->
                show targets t;
                print_newline();
                print_newline();
              end target;
            | _ ->
              if target = [] then (raise (Arg.Bad (sprintf "%s: no target specified" (string_of_command command))));
              List.iter (fun (_, (_, t)) -> execute_target ~external_tasks ~targets ~command t) target
        end;
        popd();
      with Arg.Bad _ as ex ->
        popd();
        raise ex
      | ex ->
        popd();
        Printf.eprintf "File \"oebuild_script_util.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());;
  end in

  let module Argc = Argc.Make (Command) in

  let global_options = [
    ("-C",      Set_string Option.change_dir, "<dir> Change directory before running [default: src]");
    (*("-prefix", Set_string Option.prefix,     "<dir> When installing libraries use <dir> instead of `ocamlc -where` as root");*)
  ] @ cmd_line_args in
  let global_options = Arg.align global_options in
  let command_name = Filename.basename Sys.argv.(0) in
  (* Print targets *)
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 targets in
  let targets_shown = List.filter (fun (_, tg) -> tg.show) targets in
  let help_of_targets = String.concat "\n" (List.map begin fun (name, tg) ->
    let name = rpad name ' ' maxlength in
    sprintf "  %2d) %s %s, %s" tg.num name
      (string_of_output_type tg.target_type) (string_of_compilation_type (ccomp_type <> None) tg)
  end targets_shown) in
  let usage_msg = sprintf
    "\nUSAGE\n  ocaml %s [global_options*] <command> [options*] [targets*]\n  ocaml %s <command> -help"
      command_name command_name
  in
  let help_string () =
    sprintf "%s\n\nGLOBAL OPTIONS%s\nCOMMANDS%s\n\nTARGETS\n%s"
      usage_msg (Arg.usage_string global_options "") Argc.help_of_commands help_of_targets
  in
  try Argc.parse ~usage_msg ~global_options Command.execute
  with
    | Arg.Help _ -> print_endline (help_string ())
    | Arg.Bad msg -> prerr_endline msg
    | Argc.Help_Command (cmd, (specs, descr, usage), msg) ->
      let name = Command.string_of_command cmd in
      begin
        match cmd with
          | `Distclean ->
            printf "%s %s - %s\n\nUSAGE\n  ocaml %s [global_options*] %s\n\nOPTIONS%s"
              command_name name descr command_name name (Arg.usage_string specs "")
          | _ ->
            printf "%s %s - %s\n\nUSAGE\n  ocaml %s [global_options*] %s [options*] [targets*]\n\nOPTIONS%s"
              command_name name descr command_name name (Arg.usage_string specs "")
      end;
    | Command.Unrecognized_command msg -> prerr_endline msg
    | ex -> prerr_endline (Printexc.to_string ex)
;;

