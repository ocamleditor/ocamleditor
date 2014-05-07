(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

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
open Prj
open Printf
open Target

type meta = {
  meta_target                 : Target.t;
  meta_name                   : string;
  meta_version                : string;
  meta_description            : string;
  meta_requires               : string;
  meta_archive_byte           : string list;
  meta_archive_native         : string list;
  mutable meta_subpackages    : meta list;
}

let default_fl_installer_basename = "fl_install.ml"

let convert_target_type =
  function
  | Target.Executable -> Oebuild.Executable
  | Target.Library -> Oebuild.Library
  | Target.Plugin -> Oebuild.Plugin
  | Target.Pack -> Oebuild.Pack
  | Target.External -> Oebuild.External

let create_meta proj ~parent tg =
  let archive compilation tg =
    match
      Oebuild.get_output_name
        ~compilation
        ~outkind:(convert_target_type tg.target_type)
        ~outname:(tg.outname)
        ~toplevel_modules:(Miscellanea.split " +"  tg.files)
    with Some x -> x | _ -> ""
  in
  let target_deps = Target.find_target_dependencies proj.targets tg in
  let target_deps =
    match parent with
      | Some parent -> List.filter (fun x ->
          x.id <> parent.id && not (List.mem x.id (List.map (fun y -> y.id) (Target.find_target_dependencies proj.targets parent)))) target_deps
      | None -> target_deps
  in
  let arb =
    if tg.byt
    then List.rev_append (List.map (fun d -> archive Oebuild.Bytecode d) target_deps) [archive Oebuild.Bytecode tg]
    else []
  in
  let arn =
    if tg.opt
    then List.rev_append (List.map (fun d -> archive Oebuild.Native d) target_deps) [archive Oebuild.Native tg]
    else []
  in
  {
    meta_target         = tg;
    meta_name           = tg.Target.name;
    meta_version        = proj.Prj.version;
    meta_description    = tg.descr;
    meta_requires       = tg.package;
    meta_archive_byte   = arb;
    meta_archive_native = arn;
    meta_subpackages    = [];
  }

let rec generate_def buf level meta =
  let indent = String.make (level * 2) ' ' in
  if level > 0 then bprintf buf "\n%spackage \"%s\" (\n%!" (String.make ((level - 1) * 2) ' ') meta.meta_name;
  bprintf buf "%sversion=\"%s\"\n%!" indent meta.meta_version;
  bprintf buf "%sdescription=\"%s\"\n%!" indent meta.meta_description;
  bprintf buf "%srequires=\"%s\"\n%!" indent meta.meta_requires;
  bprintf buf "%sarchive(byte)=\"%s\"\n%!" indent (String.concat "," meta.meta_archive_byte);
  bprintf buf "%sarchive(native)=\"%s\"\n%!" indent (String.concat "," meta.meta_archive_native);
  let archives =
    List.fold_left begin fun acc sp ->
      (generate_def buf (level + 1) sp) :: acc
    end [] meta.meta_subpackages
  in
  if level > 0 then bprintf buf "%s)\n%!" (String.make ((level - 1) * 2) ' ');
  meta.meta_archive_byte @ meta.meta_archive_native @ (List.flatten archives)

let generate_meta outchan meta =
  let buf = Buffer.create 100 in
  let archives = generate_def buf 0 meta in
  fprintf outchan "  %S,\n  %S,\n  [%s];\n%!"
    meta.meta_name (Buffer.contents buf) (String.concat "; " (List.map (sprintf "%S") archives))

let generate_fl_installer proj outchan =
  let processed = ref [] in
  let targets = ref proj.targets in
  let rec generate ~parent acc = function
    | tg :: tl ->
      let metas =
        if tg.is_fl_package && not (List.mem tg.id !processed) then begin
          let meta = create_meta proj ~parent tg in
          meta.meta_subpackages <- generate ~parent:(Some tg) [] tg.sub_targets;
          processed := tg.id :: !processed;
          meta :: acc
        end else acc
      in
      generate ~parent:None metas tl
    | [] -> acc
  in
  let metas = List.rev (generate ~parent:None [] !targets) in
  fprintf outchan "open Printf\n\n";
  fprintf outchan "let packages = [\n";
  List.iter (generate_meta outchan) metas;
  fprintf outchan "]\n\n";
  fprintf outchan "let _ = \n";
  fprintf outchan "  if Array.length Sys.argv < 2 then failwith \"Invalid parameters\";\n";
  fprintf outchan "  let cwd = Sys.getcwd() in\n";
  fprintf outchan "  Sys.chdir \"%s\";\n%!" Prj.default_dir_src;
  fprintf outchan "  printf \"Current working directory: %%s\\n%%!\" (Sys.getcwd());\n";
  fprintf outchan "  List.iter begin fun (name, defs, cmas) ->\n";
  fprintf outchan "    let arcs = String.concat \" \" (List.map (fun x -> (Filename.chop_extension x) ^ (if Sys.win32 then \".lib\" else \".a\")) (List.filter (fun x -> Filename.check_suffix x \".cmxa\") cmas)) in\n";
  fprintf outchan "    let cmis = \"\" in\n";
  fprintf outchan "    let mlis = \"\" in\n";
  fprintf outchan "    if Sys.file_exists \"META\" then failwith \"Cannot write META file: file exists\";\n";
  fprintf outchan "    let chan = open_out_bin \"META\" in\n";
  fprintf outchan "    try\n";
  fprintf outchan "      output_string chan defs;\n";
  fprintf outchan "      let cmd =\n";
  fprintf outchan "        if List.mem Sys.argv.(1) [\"-install\"; \"-reinstall\"] then \n";
  fprintf outchan "          sprintf \"ocamlfind install %%s META %%s %%s %%s %%s\" name (String.concat \" \" cmas) arcs cmis mlis\n";
  fprintf outchan "        else if List.mem Sys.argv.(1) [\"-uninstall\"] then \n";
  fprintf outchan "          sprintf \"ocamlfind remove %%s\" name\n";
  fprintf outchan "        else failwith \"Invalid parameters\"\n";
  fprintf outchan "      in\n";
  fprintf outchan "      printf \"%%s\\n%%!\" cmd;\n";
  fprintf outchan "      let exit_code = Sys.command cmd in\n";
  fprintf outchan "      if exit_code <> 0 then eprintf \"Error: command %%s exited with code %%d\\n%%!\" cmd exit_code;\n";
  fprintf outchan "      if Sys.file_exists \"META\" then (close_out_noerr chan; Sys.remove \"META\")\n";
  fprintf outchan "    with ex -> begin\n";
  fprintf outchan "      if Sys.file_exists \"META\" then (close_out_noerr chan; Sys.remove \"META\")\n";
  fprintf outchan "    end\n";
  fprintf outchan "  end packages;\n";
  fprintf outchan "  Sys.chdir cwd;\n";
  ()

let write proj =
  let has_fl_packages = List.exists (fun tg -> tg.Target.is_fl_package) proj.targets in
  if has_fl_packages then begin
    let dirname = proj.root // Prj.default_dir_tools in
    if not (Sys.file_exists dirname) then (Unix.mkdir dirname 0o777);
    let filename = dirname // default_fl_installer_basename in
    Printf.printf "--->%s -- %b\n%!" filename has_fl_packages;
    let outchan = open_out_bin filename in
    try
      generate_fl_installer proj outchan;
      close_out_noerr outchan;
    with ex ->
      close_out_noerr outchan;
      raise ex
  end
