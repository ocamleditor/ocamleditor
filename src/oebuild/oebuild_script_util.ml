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
  output_name : string;
  output_kind : output_kind;
  compilation_bytecode : bool;
  compilation_native : bool;
  toplevel_modules : string;
  search_path : string;
  required_libraries : string;
  compiler_flags : string;
  linker_flags : string;
  thread : bool;
  vmthread : bool;
  pp : string;
  library_install_dir : string;
  other_objects : string;
  external_tasks : int list;
  restrictions : string list;
}

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
  String.sub result 0 width

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

let string_of_outkind = function
  | Executable -> "Executable"
  | Library -> "Library"
  | Plugin -> "Plugin"
  | Pack -> "Pack";;

(** Options *)
module Option = struct
  let prefix = ref ""
  let change_dir = ref "src"
end

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

(** Show *)
let show = function num, (name, t) ->
  let files = Str.split (Str.regexp " +") t.toplevel_modules in
  let deps = Dep.find ~pp:t.pp ~with_errors:true ~echo:false files in
  let compilation =
    (if t.compilation_bytecode then [Bytecode] else []) @
    (if t.compilation_native && ccomp_type <> None then [Native] else [])
  in
  let outname = List.map (fun compilation ->
    get_output_name ~compilation ~outkind:t.output_kind ~outname:t.output_name ~targets:files) compilation
  in
  let outkind = string_of_outkind t.output_kind in
  let compilation = string_of_compilation_type (ccomp_type <> None) t in
  let prop_1 = [
    "Restrictions", (String.concat "," t.restrictions);
    "Output name", (String.concat ", " outname);
  ] in
  let prop_2 = [
    "Search path (-I)", t.search_path;
    "Required libraries", t.required_libraries;
    "Compiler flags", t.compiler_flags;
    "Linker flags", t.linker_flags;
    "Toplevel modules", t.toplevel_modules;
    "Dependencies", (String.concat " " deps);
  ] in
  let properties = if t.output_kind = Library then prop_1 @ [
    "Install directory", (Oebuild.ocamllib // t.library_install_dir)
  ] @ prop_2 else prop_1 @ prop_2 in
  printf "%d) %s (%s, %s)\n%!" num name outkind compilation;
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
  List.iter (fun (n, v) -> printf "  %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties
;;

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

(** Command *)
module Command = struct
  type t = Show | Build | Install | Clean | Distclean

  let command : t option ref = ref None

  let commands = [
    "show",      (Show,      " <target>... Show the build options of a target");
    "build",     (Build,     " <target>... Build a target (default)");
    "install",   (Install,   " <target>... Install a library");
    "clean",     (Clean,     " <target>... Remove output files for the selected target");
    "distclean", (Distclean, "Remove all build output");
  ]

  let set name =
    match !command with
      | Some _ -> false
      | None ->
        begin
          try
            let c, _ = List.assoc name commands in
            command := Some c;
            true
          with Not_found -> false
        end;;

  let execute_target ~(external_tasks : (int * Task.t) list) ~command  (_, (name, t)) =
    if Oebuild.check_restrictions t.restrictions then
      let compilation = (if t.compilation_bytecode then [Bytecode] else [])
        @ (if t.compilation_native && (ccomp_type <> None) then [Native] else []) in
      let files = Str.split (Str.regexp " +") t.toplevel_modules in
      let deps () = Dep.find ~pp:t.pp ~with_errors:true ~echo:false files in
      let etasks = List.map (fun x -> snd (List.nth external_tasks x)) t.external_tasks in
      List.iter begin fun compilation ->
        let outname = get_output_name ~compilation ~outkind:t.output_kind ~outname:t.output_name ~targets:files in
        match command with
          | Build ->
            List.iter ETask.execute (ETask.filter etasks Before_compile);
            let deps = deps() in
            (*List.iter ETask.execute (ETask.filter etasks Compile);*)
            begin
              match build
                ~compilation
                ~includes:t.search_path
                ~libs:t.required_libraries
                ~other_mods:t.other_objects
                ~outkind:t.output_kind
                ~compile_only:false
                ~thread:t.thread
                ~vmthread:t.vmthread
                ~annot:false
                ~pp:t.pp
                ~cflags:t.compiler_flags
                ~lflags:t.linker_flags
                ~outname
                ~deps
                ~ms_paths:(ref false)
                ~targets:files ()
              with
                | Built_successfully ->
                  List.iter ETask.execute (ETask.filter etasks After_compile);
                | Build_failed n -> popd(); exit n
            end
          | Install ->
            let deps = deps() in
            install_output ~compilation ~outkind:t.output_kind ~outname ~deps ~path:t.library_install_dir ~ccomp_type
          | Clean ->
            List.iter ETask.execute (ETask.filter etasks Before_clean);
            let deps = deps() in
            clean ~compilation ~outkind:t.output_kind ~outname ~targets:files ~deps ();
            List.iter ETask.execute (ETask.filter etasks After_clean);
          | Distclean ->
            let deps = deps() in
            List.iter ETask.execute (ETask.filter etasks Before_clean);
            clean ~compilation ~outkind:t.output_kind ~outname ~targets:files ~deps ~all:true ();
            List.iter ETask.execute (ETask.filter etasks After_clean);
          | Show -> assert false
      end compilation;;

  let execute ~external_tasks ~target targets =
    pushd !Option.change_dir;
    try
      let command = match !command with Some c -> c | _ -> assert false in
      begin
        match command with
          | Distclean ->
            List.iter (fun t -> execute_target external_tasks command (0, t)) targets;
            clean_all()
          | Show ->
            printf "%s\n%!" (system_config ());
            Printf.printf "\n%!" ;
            List.iter begin fun t ->
              show t;
              print_newline();
              print_newline();
            end target;
          | _ -> List.iter (execute_target ~external_tasks ~command) target
      end;
      popd();
    with ex -> popd();
end

(** add_target *)
let target : (int * (string * target)) list ref = ref []

let add_target targets name =
  try
    begin
      try
        let n = int_of_string name in
        target := (n, List.nth targets (n - 1)) :: !target
      with _ -> target := (0, (name, (List.assoc name targets))) :: !target
    end
  with Not_found -> ();;

(** main *)
let main ~external_tasks ~targets =
  let parse_anon targets x = if not (Command.set x) then (add_target targets x) in
  let speclist = [
    ("-C",      Set_string Option.change_dir, "<dir> Change directory before running (default is \"src\")");
    (*("-prefix", Set_string Option.prefix,     "<dir> When installing libraries use <dir> instead of `ocamlc -where` as root");*)
  ] in
  let speclist = Arg.align speclist in
  let command_name = Filename.basename Sys.argv.(0) in
  (* Print targets *)
  let i = ref 0 in
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 targets in
  let descr = String.concat "\n" (List.map begin fun (name, tg) ->
    incr i;
    let name = rpad name ' ' maxlength in
    sprintf "  %2d) %s %s, %s" !i name
      (string_of_outkind tg.output_kind) (string_of_compilation_type (ccomp_type <> None) tg)
  end targets) in
  (* Print commands *)
  let cmds = List.map begin fun (c, (_, d)) ->
    if d.[0] = ' ' then begin
      let pos = try String.index_from d 1 ' ' with Not_found -> String.length d in
      let arg = Str.string_before d pos in
      (c ^ arg), (try Str.string_after d (pos + 1) with _ -> "")
    end else c, d
  end Command.commands in
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 cmds in
  let cmds = String.concat "\n" (List.map begin fun (c, d) ->
    let c = rpad c ' ' maxlength in
    sprintf "  %s %s" c d
  end cmds) in
  (* Help message *)
  let help_message = sprintf "\nUsage\n  Please first edit the \"Build Configurations\" section at the end of\n  file \"%s\" to set the right options for your system, then do:\n\n    ocaml %s <command> [options]\n\nCommands\n%s\n\nTargets\n%s\n\nOptions"
    command_name command_name cmds descr in
  Arg.parse speclist (parse_anon targets) help_message;
  if !Arg.current = 1
  then (Arg.usage speclist help_message)
  else begin
    (match !Command.command with None -> Command.command := Some Command.Build | _ -> ());
    Command.execute ~external_tasks ~target:(List.rev !target) targets
  end;;


