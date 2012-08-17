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


open Miscellanea
open Printf
open Target
open Task
open Build_script
open Build_script_args

(** print_targets *)
let print_targets ochan targets external_tasks =
  let i = ref 0 in
  let print = fprintf ochan "  %s\n" in
  output_string ochan "let targets = [\n";
  List.iter begin fun {bst_target=tg; bst_show; _} ->
    let installer_task =
      try
        let num, _ = List.find (fun (_, it) -> it) (List.assoc tg external_tasks) in
        sprintf "Some %d" num
      with Not_found -> "None"
    in
    if bst_show then (incr i);
    let num = if bst_show then !i else 0 in
    kprintf print "\n  (\x2A %d \x2A)" !i;
    kprintf print "%S, {" tg.name;
    kprintf print "  num                  = %d;" num;
    kprintf print "  id                   = %d;" tg.id;
    kprintf print "  output_name          = %S;" tg.outname;
    kprintf print "  target_type          = %s;" (string_of_target_type tg.target_type);
    kprintf print "  compilation_bytecode = %b;" tg.byt;
    kprintf print "  compilation_native   = %b;" tg.opt;
    kprintf print "  toplevel_modules     = %S;" tg.files;
    kprintf print "  search_path          = %S; (\x2A -I \x2A)" tg.includes;
    kprintf print "  required_libraries   = %S;" tg.libs;
    kprintf print "  compiler_flags       = %S;" tg.cflags;
    kprintf print "  linker_flags         = %S;" tg.lflags;
    kprintf print "  thread               = %b;" tg.thread;
    kprintf print "  vmthread             = %b;" tg.vmthread;
    kprintf print "  pp                   = %S;" tg.pp;
    kprintf print "  inline               = %s;" (match tg.inline with Some x -> sprintf "Some %d" x | _ -> "None");
    kprintf print "  library_install_dir  = %S; (\x2A Relative to the Standard Library Directory \x2A)" tg.lib_install_path;
    kprintf print "  other_objects        = %S;" tg.other_objects;
    kprintf print "  external_tasks       = [%s];" (String.concat "; " (List.map (fun (n, _) -> string_of_int n) (List.assoc tg external_tasks)));
    kprintf print "  restrictions         = [%s];" (String.concat "; " (List.map (sprintf "%S") tg.restrictions));
    kprintf print "  dependencies         = [%s];" (String.concat "; " (List.map (sprintf "%d") tg.dependencies));
    kprintf print "  show                 = %b;" bst_show;
    kprintf print "  installer_task       = %s;" installer_task;
    kprintf print "};";
  end targets;
  output_string ochan "];;\n";;

(** ident_of_arg *)
let ident_of_arg =
  let normalize_ident =
    let re = Str.regexp "[-]" in
    Str.global_replace re "_"
  in
  fun arg -> "arg_" ^ (normalize_ident (Str.string_after arg.bsa_key 1));;

(** print_add_args *)
let print_add_args bc et args =
  List.fold_left begin fun acc arg ->
    match arg.bsa_task with
      | Some (abc, aet) ->
        if bc.Target.id = abc.Target.id && et.Task.et_name = aet.Task.et_name then
          match arg.bsa_mode with
            | `add ->
              let arg =
                match arg.bsa_pass with
                  | `key -> sprintf "!%s,\"%s\"" (ident_of_arg arg) arg.bsa_key
                  | `value -> sprintf "true,\"%s\"" (ident_of_arg arg)
                  | `key_value -> sprintf "true,\"%s %s\"" arg.bsa_key (ident_of_arg arg)
              in
              arg :: acc
            | `replace _ -> acc (* TODO:  *)
        else acc
      | _ -> acc
  end [] args;;

(** print_external_tasks *)
let print_external_tasks ochan project =
  let targets = project.Prj.targets in
  let args = project.Prj.build_script.bs_args in
  let i = ref 0 in
  let print = fprintf ochan "  %s\n" in
  output_string ochan "let external_tasks = [\n";
  let targets = project.Prj.build_script.bs_targets in
  let ets = List.map begin fun {bst_target=tg; bst_installer_task; _} ->
    let ets =
      List.map begin fun et ->
        let base_args = Xlist.filter_map (fun (x, y) -> if x then Some y else None) et.et_args in
        let base_args = List.map (sprintf "true,%S") base_args in
        let custom_args = print_add_args tg et args in
        let args = base_args @ custom_args in
        let index = !i in
        kprintf print "\n  %d, (fun () -> {" index;
        kprintf print "  et_name                  = %S;" et.et_name;
        kprintf print "  et_env                   = [%s];"
          (String.concat ";" (List.map (sprintf "%S")
            (Xlist.filter_map (fun (x, y) -> if x then Some y else None) et.et_env)));
        kprintf print "  et_env_replace           = %b;" et.et_env_replace;
        kprintf print "  et_dir                   = %S;" et.et_dir;
        kprintf print "  et_cmd                   = %S;" et.et_cmd;
        kprintf print "  et_args                  = [%s];" (String.concat "; " args);
        kprintf print "  et_phase                 = %s;" (match et.et_phase with Some p -> "Some " ^ (Task.string_of_phase p) | _ -> "None");
        kprintf print "  et_always_run_in_project = %b;" et.et_always_run_in_project;
        kprintf print "  et_always_run_in_script  = %b;" et.et_always_run_in_script;
        kprintf print "});";
        incr i;
        index, (match bst_installer_task with Some task when task.et_name = et.et_name -> true | _ -> false)
      end tg.external_tasks
    in tg, ets
  end targets in
  output_string ochan "];;\n";
  ets;;

(** print_cmd_line_args *)
let print_cmd_line_args ochan project =
  let args = project.Prj.build_script.bs_args in
  List.iter begin fun arg ->
    let default =
      match arg.bsa_default with
        | `flag x -> string_of_bool x
        | `bool x -> string_of_bool x
        | `string x -> sprintf "%S" x
    in
    fprintf ochan "let %s = ref %s\n" (ident_of_arg arg) default;
  end args;
  fprintf ochan "let cmd_line_args = [\n";
  List.iter begin fun arg ->
    let typ =
      match arg.bsa_type with
        | Flag -> sprintf "Set %s" (ident_of_arg arg)
        | Bool -> "Bool (fun _ -> ())"
        | String -> sprintf "Set_string %s" (ident_of_arg arg)
    in
    let default_value =
      match arg.bsa_default with
        | `flag x -> if x then "Set" else "Not Set"
        | `bool x -> string_of_bool x
        | `string x -> String.escaped x
    in
    fprintf ochan "  %S, %s,\n    \" %s [default: %s]\";\n" arg.bsa_key typ (String.escaped arg.bsa_doc) default_value;
  end args;
  fprintf ochan "]\n";;

(** print *)
let print ~project ~filename () =
  let ochan = open_out_bin filename in
  let finally () = close_out_noerr ochan in
  try
    output_string ochan "(\x2A\n   Please edit the \"Targets\" section at the end\n   of this file to set the right options for your system.\n\x2A)\n\n";
    output_string ochan Oebuild_script.code;
    output_string ochan "open Arg\n";
    output_string ochan "open Task\n";
    output_string ochan "\n";
    print_cmd_line_args ochan project;
    output_string ochan "\n";
    let ets = print_external_tasks ochan project in
    output_string ochan "\n\n";
    output_string ochan "(\x2A Targets ==================================================== \x2A)\n\n";
    (*  *)
    print_targets ochan project.Prj.build_script.bs_targets ets;
    output_string ochan "\n";
    output_string ochan "(\x2A End of Targets ============================================= \x2A)\n\n";
    (*  *)
    output_string ochan "let _ = main ~cmd_line_args ~external_tasks ~targets\n";
    (*  *)
    finally();
  with ex -> begin
    finally();
    raise ex
  end;;













