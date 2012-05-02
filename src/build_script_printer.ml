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
open Bconf
open Task
open Build_script
open Build_script_args

let print_configs ochan bconfigs external_tasks =
  let i = ref 0 in
  let print = fprintf ochan "  %s\n" in
  output_string ochan "let targets = [\n";
  List.iter begin fun bc ->
    incr i;
    kprintf print "(\x2A %d \x2A)" !i;
    kprintf print "%S, {" bc.name;
    kprintf print "  output_name          = %S;" bc.outname;
    kprintf print "  output_kind          = %s;" (string_of_outkind bc.outkind);
    kprintf print "  compilation_bytecode = %b;" bc.byt;
    kprintf print "  compilation_native   = %b;" bc.opt;
    kprintf print "  toplevel_modules     = %S;" bc.files;
    kprintf print "  search_path          = %S; (\x2A -I \x2A)" bc.includes;
    kprintf print "  required_libraries   = %S;" bc.libs;
    kprintf print "  compiler_flags       = %S;" bc.cflags;
    kprintf print "  linker_flags         = %S;" bc.lflags;
    kprintf print "  thread               = %b;" bc.thread;
    kprintf print "  vmthread             = %b;" bc.vmthread;
    kprintf print "  pp                   = %S;" bc.pp;
    kprintf print "  library_install_dir  = %S; (\x2A Relative to the Standard Library Directory \x2A)" bc.lib_install_path;
    kprintf print "  other_objects        = %S;" bc.other_objects;
    kprintf print "  external_tasks       = [%s];" (String.concat "; " (List.assoc bc external_tasks));
    kprintf print "  restrictions         = [%s];" (String.concat "; " (List.map (sprintf "%S") bc.restrictions));
    kprintf print "};";
  end bconfigs;
  output_string ochan "];;\n";;

let print_external_tasks ochan bconfigs =
  let i = ref 0 in
  let print = fprintf ochan "  %s\n" in
  output_string ochan "let external_tasks = [\n";
  let ets = List.map begin fun bc ->
    let ets = List.map begin fun et ->
      let name = sprintf "%d" !i in
      kprintf print "%s, {" name;
      kprintf print "  et_name                  = %S;" et.et_name;
      kprintf print "  et_env                   = [%s];"
        (String.concat ";" (List.map (sprintf "%S")
          (Xlist.filter_map (fun (x, y) -> if x then Some y else None) et.et_env)));
      kprintf print "  et_env_replace           = %b;" et.et_env_replace;
      kprintf print "  et_dir                   = %S;" et.et_dir;
      kprintf print "  et_cmd                   = %S;" et.et_cmd;
      kprintf print "  et_args                  = [%s];"
        (String.concat "; " (List.map (sprintf "true,%S")
          (Xlist.filter_map (fun (x, y) -> if x then Some y else None) et.et_args)));
      kprintf print "  et_phase                 = %s;" (match et.et_phase with Some p -> "Some " ^ (Task.string_of_phase p) | _ -> "None");
      kprintf print "  et_always_run_in_project = %b;" et.et_always_run_in_project;
      kprintf print "  et_always_run_in_script  = %b;" et.et_always_run_in_script;
      kprintf print "};";
      incr i;
      name
    end bc.external_tasks in
    bc, ets
  end bconfigs in
  output_string ochan "];;\n";
  ets;;

let print_cmd_line_args ochan project =
  let args = project.Project.build_script.bs_args in
  let get_ident arg = "arg_" ^ Str.string_after arg.bsa_key 1 in
  List.iter begin fun arg ->
    let default =
      match arg.bsa_default with
        | `flag x -> string_of_bool x
        | `bool x -> string_of_bool x
        | `string x -> sprintf "%S" x
    in
    fprintf ochan "let %s = ref %s\n" (get_ident arg) default;
  end args;
  fprintf ochan "let cmd_line_args = [\n";
  List.iter begin fun arg ->
    let typ =
      match arg.bsa_type with
        | Flag_Set -> sprintf "Set %s" (get_ident arg)
        | Flag_Clear -> sprintf "Clear %s" (get_ident arg)
        | Bool -> "Bool (fun _ -> ())"
        | String -> sprintf "Set_string %s" (get_ident arg)
    in
    fprintf ochan "  %S, %s,\n    %S;\n" arg.bsa_key typ arg.bsa_doc;
  end args;
  fprintf ochan "]\n";
;;

let print ~project ~filename () =
  let ochan = open_out_bin filename in
  let finally () = close_out_noerr ochan in
  try
    output_string ochan "(\x2A\n   Please edit the \"Build Configurations\" section at the end\n   of this file to set the right options for your system.\n\x2A)\n\n";
    output_string ochan Oebuild_script.code;
    output_string ochan "open Arg\n";
    output_string ochan "open Task\n";
    output_string ochan "\n";
    print_cmd_line_args ochan project;
    output_string ochan "\n";
    let ets = print_external_tasks ochan project.Project.build in
    output_string ochan "\n\n";
    output_string ochan "(\x2A Build Configurations ==================================================== \x2A)\n\n";
    (*  *)
    print_configs ochan project.Project.build ets;
    output_string ochan "\n";
    output_string ochan "(\x2A End of Build Configurations ============================================= \x2A)\n\n";
    (*  *)
    output_string ochan "let _ = main ~external_tasks ~targets\n";
    (*  *)
    finally();
  with ex -> begin
    finally();
    raise ex
  end;;













