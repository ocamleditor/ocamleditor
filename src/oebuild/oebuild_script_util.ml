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
}

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
let show = function
  | Some (name, t) ->
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
    let prop_1 = ["Output name", (String.concat ", " outname)] in
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
    printf "%s\n%!" (system_config ());
    printf "%s (%s, %s)\n%!" name outkind compilation;
    let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
    List.iter (fun (n, v) -> printf "  %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties
  | _ -> ();;

(** Command *)
module Command = struct
  type t = Show | Build | Install | Clean | Distclean

  let command : t option ref = ref None

  let commands = [
    "show",      (Show,      " <target> Show the build options of a target");
    "build",     (Build,     " <target> Build a target (default)");
    "install",   (Install,   " <target> Install a library");
    "clean",     (Clean,     " <target> Remove output files for the selected target");
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

  let execute_target command = function
    | Some (name, t) ->
      let compilation = (if t.compilation_bytecode then [Bytecode] else [])
        @ (if t.compilation_native && (ccomp_type <> None) then [Native] else []) in
      let files = Str.split (Str.regexp " +") t.toplevel_modules in
      let deps = Dep.find ~pp:t.pp ~with_errors:true ~echo:false files in
      List.iter begin fun compilation ->
        let outname = get_output_name ~compilation ~outkind:t.output_kind ~outname:t.output_name ~targets:files in
        match command with
          | Build -> begin
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
              | Built_successfully -> ()
              | Build_failed n -> popd(); exit n
          end
          | Install ->
            install_output ~compilation ~outkind:t.output_kind ~outname ~deps ~path:t.library_install_dir ~ccomp_type
          | Clean ->
            clean ~compilation ~outkind:t.output_kind ~outname ~targets:files ~deps ()
          | Distclean ->
            clean ~compilation ~outkind:t.output_kind ~outname ~targets:files ~deps ~all:true ();
          | Show -> assert false
      end compilation;
    | _ -> invalid_arg "No target was specified";;

  let execute ~target targets =
    pushd !Option.change_dir;
    let command = match !command with Some c -> c | _ -> assert false in
    begin
      match command with
        | Distclean ->
          List.iter (fun t -> execute_target command (Some t)) targets;
          clean_all()
        | Show -> show target;
        | _ -> execute_target command target
    end;
    popd();
end

(** set_target *)
let target : (string * target) option ref = ref None

let set_target targets name =
  try
    begin
      try
        let n = int_of_string name in
        target := Some (List.nth targets (n - 1))
      with _ -> target := Some (name, (List.assoc name targets))
    end
  with Not_found -> target := None;;

(** main *)
let main targets =
  let parse_anon targets x = if not (Command.set x) then (set_target targets x) in
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
    Command.execute ~target:!target targets
  end;;


