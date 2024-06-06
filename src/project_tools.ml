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



open Utils
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
  meta_cmis                   : string list;
  meta_mlis                   : string list;
  mutable meta_subpackages    : meta list;
}

type files = {
  archives : string list;
  cmis     : string list;
  mlis     : string list;
}

let default_fl_installer_basename = "findlib.ml"
let default_rccompile_basename = "rc_compile.ml"
let default_editbin_basename = "editbin.ml"
let re_quote = Utils.regexp "\""

let convert_target_type =
  function
  | Target.Executable -> Oebuild.Executable
  | Target.Library -> Oebuild.Library
  | Target.Plugin -> Oebuild.Plugin
  | Target.Pack -> Oebuild.Pack
  | Target.External -> Oebuild.External

let preamble = "\
#load \"unix.cma\"
#load \"str.cma\"
open Printf

let unquote =
  let re = Str.regexp \"^\\\"\\\\(.*\\\\)\\\"$\" in
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
  end"

let create_meta proj ~parent tg =
  let output_name compilation tg =
    Oebuild.get_output_name
      ~compilation
      ~outkind:(convert_target_type tg.target_type)
      ~outname:(tg.outname)
      ~dontaddopt:tg.dontaddopt ()
  in
  let all_target_deps = Target.find_target_dependencies proj.targets tg in
  let findlib_target_deps =
    List.filter (fun t -> t.is_fl_package) all_target_deps
  in
  let target_deps =
    match parent with
    | Some parent -> List.filter (fun x ->
        x.id <> parent.id && not (List.mem x.id (List.map (fun y -> y.id) (Target.find_target_dependencies proj.targets parent)))) all_target_deps
    | None -> all_target_deps
  in
  let arb =
    if tg.byt
    then List.rev_append (List.map (fun d -> output_name Oebuild.Bytecode d) target_deps) [output_name Oebuild.Bytecode tg]
    else []
  in
  let arn =
    if tg.opt
    then List.rev_append (List.map (fun d -> output_name Oebuild.Native d) target_deps) [output_name Oebuild.Native tg]
    else []
  in
  let cmis =
    List.fold_left begin fun acc tg ->
      let cmoxs = (*List.filter (fun x -> not (Filename.check_suffix x ".cmi"))*) (Target.find_dependencies tg) in
      let cmis = List.map (fun x -> (Filename.chop_extension x) ^ ".cmi") cmoxs in
      cmis @ acc
    end [] (target_deps @ [tg]);
  in
  let cmis = Utils.ListExt.remove_dupl cmis in
  let mlis = List.filter (fun x -> Sys.file_exists ((Filename.chop_extension x) ^ ".mli")) cmis in
  let mlis = List.map (fun x -> (Filename.chop_extension x) ^ ".mli") mlis in
  {
    meta_target         = tg;
    meta_name           = tg.Target.name;
    meta_version        = proj.Prj.version;
    meta_description    = tg.descr;
    meta_requires       = (String.concat "," (tg.package :: (List.map (fun t -> t.Target.name) findlib_target_deps)));
    meta_archive_byte   = arb;
    meta_archive_native = arn;
    meta_cmis           = cmis;
    meta_mlis           = mlis;
    meta_subpackages    = [];
  }

let rec generate_def buf level meta =
  let indent = String.make (level * 2) ' ' in
  if level > 0 then bprintf buf "\n\n%spackage \"%s\" (\n%!" (String.make ((level - 1) * 2) ' ') meta.meta_name;
  bprintf buf "%sversion         = \"%s\"\n%!" indent meta.meta_version;
  bprintf buf "%sdescription     = \"%s\"\n%!" indent meta.meta_description;
  bprintf buf "%srequires        = \"%s\"\n%!" indent meta.meta_requires;
  bprintf buf "%sarchive(byte)   = \"%s\"\n%!" indent (String.concat "," (List.map Filename.basename meta.meta_archive_byte));
  bprintf buf "%sarchive(native) = \"%s\"%!" indent (String.concat "," (List.map Filename.basename meta.meta_archive_native));
  let (sub_archives, sub_cmis, sub_mlis) =
    List.fold_left begin fun (aa, ac, am) sp ->
      let sub = generate_def buf (level + 1) sp in
      sub.archives :: aa, sub.cmis :: ac, sub.mlis :: am
    end ([], [], []) meta.meta_subpackages
  in
  if level > 0 then bprintf buf "\n%s)%!" (String.make ((level - 1) * 2) ' ');
  {
    archives = meta.meta_archive_byte @ meta.meta_archive_native @ (List.flatten sub_archives);
    cmis     = meta.meta_cmis @ (List.flatten sub_cmis);
    mlis     = meta.meta_mlis @ (List.flatten sub_mlis);
  }

let generate_meta outchan meta =
  let buf = Buffer.create 100 in
  let files = generate_def buf 0 meta in
  fprintf outchan "  (\x2A\x2A %s \x2A)\n  %S,\"\\\n%s\",\n  [\n    %s\n  ], [\n    %s\n  ], [\n    %s\n  ];\n%!"
    meta.meta_name meta.meta_name
    (Str.global_replace re_quote "\\\"" (Buffer.contents buf))
    (String.concat ";\n    " (List.map (sprintf "%S") (List.sort compare (Utils.ListExt.remove_dupl files.archives))))
    (String.concat ";\n    " (List.map (sprintf "%S") (List.sort compare (Utils.ListExt.remove_dupl files.cmis))))
    (String.concat ";\n    " (List.map (sprintf "%S") (List.sort compare (Utils.ListExt.remove_dupl files.mlis))))

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
  fprintf outchan "(\x2A\n\n  This file is automatically generated by %s %s, do not edit.\n\n\x2A)\n\n" About.program_name About.version;
  fprintf outchan "%s\n\n" preamble;
  fprintf outchan "open Printf\n\n";
  fprintf outchan "let packages = [\n";
  List.iter (generate_meta outchan) metas;
  fprintf outchan "]\n\n";
  fprintf outchan "let is_mingw = List.exists ((=) \"system: mingw\") (get_command_output \"ocamlc -config\")\n\n";
  fprintf outchan "let _ = \n";
  fprintf outchan "  if Array.length Sys.argv < 2 then failwith \"Invalid parameters\";\n";
  fprintf outchan "  let cwd = Sys.getcwd() in\n";
  fprintf outchan "  Sys.chdir \"%s\";\n%!" Prj.default_dir_src;
  fprintf outchan "  let lib_ext = if Sys.win32 && not is_mingw then \".lib\" else \".a\" in\n";
  (*  fprintf outchan "  printf \"Current working directory: %%s\\n%%!\" (Sys.getcwd());\n";*)
  fprintf outchan "  List.iter begin fun (name, defs, archives, (cmis : string list), (mlis : string list)) ->\n";
  fprintf outchan "    let cmxas = List.filter (fun x -> Filename.check_suffix x \".cmxa\") archives in\n";
  fprintf outchan "    let libs = String.concat \" \" (List.map (fun x -> (Filename.chop_extension x) ^ lib_ext) cmxas) in\n";
  fprintf outchan "    let arcs = String.concat \" \" archives in\n";
  fprintf outchan "    let cmis = String.concat \" \" cmis in\n";
  fprintf outchan "    let mlis = String.concat \" \" mlis in\n";
  fprintf outchan "    let _META = \"META\" in\n";
  fprintf outchan "    if Sys.file_exists _META then failwith \"Cannot write META file: file exists\";\n";
  fprintf outchan "    let chan = open_out_bin _META in\n";
  fprintf outchan "    try\n";
  fprintf outchan "      let sudo = if Sys.win32 || List.mem Sys.argv.(1) [\"print\"] then \"\" else \"sudo -E \" in\n";
  fprintf outchan "      let cmd = ref [] in\n";
  fprintf outchan "      if not (List.mem Sys.argv.(1) [\"install\"; \"uninstall\"; \"reinstall\"; \"print\"]) then failwith \"Invalid parameters\";\n";

  fprintf outchan "      output_string chan defs;\n";
  fprintf outchan "      flush chan;\n";
  fprintf outchan "      if List.mem Sys.argv.(1) [\"uninstall\"; \"reinstall\"] then cmd := (sprintf \"%%socamlfind remove %%s\" sudo name) :: !cmd;\n";

  fprintf outchan "      if List.mem Sys.argv.(1) [\"install\"; \"reinstall\"; \"print\"] then \n";
  fprintf outchan "        cmd := (sprintf \"%%socamlfind install %%s %%s %%s %%s %%s %%s\" sudo name _META arcs libs cmis mlis) :: !cmd;\n";

  fprintf outchan "      assert (!cmd <> []);\n";
  fprintf outchan "      let cmd = String.concat \" && \" (List.rev !cmd) in\n";

  fprintf outchan "      if List.mem Sys.argv.(1) [\"print\"] then printf \"\\n##### %%s.%%s #####\\n\\n%%s\\n\\n# %%s\\n%%!\" _META name defs cmd;\n";

  fprintf outchan "      let exit_code = if Sys.argv.(1) = \"print\" then 0 else Sys.command cmd in\n";
  fprintf outchan "      if exit_code <> 0 then eprintf \"Error: command %%s exited with code %%d\\n%%!\" cmd exit_code;\n";
  fprintf outchan "      if Sys.file_exists _META then (close_out_noerr chan; Sys.remove _META)\n";
  fprintf outchan "    with ex -> begin\n";
  fprintf outchan "      if Sys.file_exists _META then (close_out_noerr chan; Sys.remove _META)\n";
  fprintf outchan "    end\n";
  fprintf outchan "  end packages;\n";
  fprintf outchan "  Sys.chdir cwd;\n";
  ()

let findlib_target_name = "FINDLIB-TOOLS"

let rm_tools tg tools =
  tg.external_tasks <- List.filter (fun et -> not (List.exists (fun ft -> ft.Task.et_name = et.Task.et_name) tools)) tg.external_tasks

(** write_findlib_tools *)
let write_findlib_tools proj =
  let has_fl_packages = List.exists (fun tg -> tg.Target.is_fl_package) proj.targets in
  let dirname = proj.root // Prj.default_dir_tools in
  let filename = dirname // default_fl_installer_basename in
  let tools = List_opt.find (fun tg -> tg.Target.name = findlib_target_name && tg.target_type = Target.External) proj.targets in
  let scriptname = Prj.default_dir_tools ^ "/" ^ default_fl_installer_basename in
  let findlib_tools = [
    Task.create ~name:"install" ~env:[] ~dir:".." ~cmd:"ocaml" ~args:[true, scriptname; true, "install"] ~readonly:true ~run_in_script:false ();
    Task.create ~name:"uninstall" ~env:[] ~dir:".." ~cmd:"ocaml" ~args:[true, scriptname; true, "uninstall"] ~readonly:true ~run_in_script:false ();
    Task.create ~name:"reinstall" ~env:[] ~dir:".." ~cmd:"ocaml" ~args:[true, scriptname; true, "reinstall"] ~readonly:true ~run_in_script:false ();
    Task.create ~name:"print" ~env:[] ~dir:".." ~cmd:"ocaml" ~args:[true, scriptname; true, "print"] ~readonly:true ~run_in_script:false ();
  ] in
  if has_fl_packages then begin
    if not (Sys.file_exists dirname) then (Unix.mkdir dirname 0o777);
    let outchan = open_out_bin filename in
    try
      generate_fl_installer proj outchan;
      let tg =
        match tools with
        | None ->
            let id = (List.fold_left (fun cand t -> max t.id cand) 0 proj.targets) + 1 in
            let tg = Target.create ~id ~name:findlib_target_name in
            tg.target_type <- External;
            tg.readonly <- true;
            proj.targets <- proj.targets @ [tg];
            tg
        | Some tg -> tg
      in
      rm_tools tg findlib_tools;
      tg.external_tasks <- tg.external_tasks @ findlib_tools;
      close_out_noerr outchan;
    with ex ->
      close_out_noerr outchan;
      raise ex
  end else begin
    if Sys.file_exists filename then Sys.remove filename;
    match tools with
    | Some tg ->
        rm_tools tg findlib_tools;
        if tg.external_tasks = [] then proj.targets <- List.filter (fun t -> t.id <> tg.id) proj.targets
    | _ -> ()
  end

(** write_subsystem_tools *)
let write_subsystem_tools proj =
  let dirname = proj.root // Prj.default_dir_tools in
  let filename = dirname // default_editbin_basename in
  if Sys.file_exists filename then Sys.remove filename;
  let has_script =
    List.fold_left begin fun has target ->
      let outname =
        Oebuild.get_output_name ~compilation:Oebuild.Native ~outkind:Oebuild.Executable ~outname:target.Target.outname ~dontaddopt:target.Target.dontaddopt ()
      in
      let editbin_task =
        Task.create ~name:"SUBSYSTEM:WINDOWS" ~env:[] ~dir:""
          ~cmd:"ocaml" ~args:[true, "../" ^ Prj.default_dir_tools ^ "/" ^ default_editbin_basename; true, outname]
          ~readonly:true ~run_in_script:true ~run_in_project:true ();
      in
      editbin_task.Task.et_visible <- false;
      editbin_task.Task.et_phase <- Some Task.After_compile;
      rm_tools target [editbin_task];
      if target.Target.subsystem = Some Windows then begin
        target.external_tasks <- editbin_task :: target.external_tasks;
        true
      end else has;
    end false proj.targets
  in
  if has_script then begin
    if not (Sys.file_exists dirname) then (Unix.mkdir dirname 0o777);
    let outchan = open_out_bin filename in
    try
      fprintf outchan "(\x2A\n\n  This file is automatically generated by %s %s, do not edit.\n\n\x2A)\n\n" About.program_name About.version;
      fprintf outchan "%s\n\n" preamble;
      fprintf outchan "let is_mingw = List.exists ((=) \"system: mingw\") (get_command_output \"ocamlc -config\")\n\n";
      fprintf outchan "let _ = if not Sys.win32 || is_mingw then exit 0\n\n";
      fprintf outchan "let _ = Printf.kprintf Sys.command \"editbin %%S /subsystem:windows 2>&1 1>NUL\" Sys.argv.(1)\n";
      close_out_noerr outchan;
    with ex ->
      close_out_noerr outchan;
      raise ex
  end

(** write_resource_file *)
let write_resource_file proj =
  let buffers = ref [] in
  let open Resource_file in
  List.iter begin fun target ->
    let rc_task =
      Task.create ~name:"RC_COMPILE" ~env:[] ~dir:""
        ~cmd:"ocaml" ~args:[true, "../" ^ Prj.default_dir_tools ^ "/" ^ default_rccompile_basename; true, ("\"" ^ target.Target.name ^ "\"")]
        ~readonly:true ~run_in_script:true ~run_in_project:true ();
    in
    rc_task.Task.et_visible <- false;
    rc_task.Task.et_phase <- Some Task.Before_compile;
    rm_tools target [rc_task];
    match target.Target.resource_file with
    | Some rc ->
        let exename = Oebuild.get_output_name
            ~compilation:Oebuild.Native
            ~outkind:Oebuild.Executable
            ~outname:target.Target.outname
            ~dontaddopt:target.Target.dontaddopt ()
        in
        let rcname = rc.rc_filename in
        let exedirname = Filename.dirname exename in
        let exedirname = if exedirname = "." then "" else exedirname in
        let buf = Buffer.create 100 in
        buffers := (target.Target.name, rcname, buf(*, rc.rc_icons_data*)) :: !buffers;
        let src = proj.Prj.root // Prj.default_dir_src in
        (* Copy icons in the right directory (same as executable) and update icon filenames in the rc record *)
        let new_iconames =
          List.map begin fun fn ->
            if Filename.is_implicit fn && Sys.file_exists (src // fn)
            then Some fn
            else begin
              match Utils.filename_relative src fn with
              | Some rel when (Filename.dirname rel = Filename.dirname exename) && Sys.file_exists (src // rel) -> Some rel
              | Some _ -> None
              | _ ->
                  if Sys.file_exists fn then begin
                    let ico = Filename.basename fn in
                    File_util.cp fn (src // exedirname // ico);
                    Some (exedirname // ico)
                  end else None
            end
          end rc.rc_icons;
        in
        let new_iconames = List.filter_map (fun x -> x) new_iconames in
        rc.rc_icons <- new_iconames;
        (* Print resource script *)
        let i = ref 0 in
        List.iter begin fun iconame ->
          Printf.bprintf buf "%d ICON %s\n" (101 + !i) iconame;
          incr i;
        end new_iconames;
        if !i > 0 then Printf.bprintf buf "\n";
        Printf.bprintf buf "1 VERSIONINFO\n";
        let a,b,c,d = rc.rc_file_version in
        bprintf buf "    FILEVERSION     %d,%d,%d,%d\n" a b c d;
        bprintf buf "    PRODUCTVERSION  %d,%d,%d,%d\n" a b c d;
        bprintf buf "    FILEOS          0x00000004L\n";
        bprintf buf "    FILETYPE        0x00000001L\n";
        bprintf buf "{\n";
        bprintf buf "    BLOCK \"StringFileInfo\"\n";
        bprintf buf "    {\n";
        bprintf buf "        BLOCK \"040904E4\"\n";
        bprintf buf "        {\n";
        bprintf buf "            VALUE \"CompanyName\", \"%s\\000\"\n" (String.escaped rc.rc_company);
        bprintf buf "            VALUE \"FileDescription\", \"%s\\000\"\n" (String.escaped rc.rc_title);
        bprintf buf "            VALUE \"FileVersion\", \"%d.%d.%d.%d\\000\"\n" a b c d;
        bprintf buf "            VALUE \"InternalName\", \"%s\\000\"\n" (String.escaped (Filename.basename exename));
        bprintf buf "            VALUE \"ProductName\", \"%s\\000\"\n" (String.escaped rc.rc_product);
        bprintf buf "            VALUE \"LegalCopyright\", \"%s\\000\"\n" (Str.global_replace (Utils.regexp "©") "\\\\251" (String.escaped rc.rc_copyright));
        bprintf buf "        }\n";
        bprintf buf "    }\n";
        bprintf buf "    BLOCK \"VarFileInfo\"\n";
        bprintf buf "    {\n";
        bprintf buf "      VALUE \"Translation\", 1033, 1252\n";
        bprintf buf "    }\n";
        bprintf buf "}\n";
        (*  *)
        target.external_tasks <- rc_task :: target.external_tasks;
    | _ -> ()
  end proj.targets;
  let dirname = proj.root // Prj.default_dir_tools in
  let filename = dirname // default_rccompile_basename in
  if !buffers <> [] then begin
    if not (Sys.file_exists dirname) then (Unix.mkdir dirname 0o777);
    let outchan = open_out_bin filename in
    try
      fprintf outchan "(\x2A\n\n  This file is automatically generated by %s %s, do not edit.\n\n\x2A)\n\n" About.program_name About.version;
      fprintf outchan "%s\n\n" preamble;
      fprintf outchan "let is_mingw = List.exists ((=) \"system: mingw\") (get_command_output \"ocamlc -config\")\n\n";
      fprintf outchan "let _ = if not Sys.win32 || is_mingw then exit 0;;\n\n";
      fprintf outchan "let resources = [\n" ;
      List.iter begin fun (tgname, rcname, rc) ->
        fprintf outchan "  %S, (%S, " tgname rcname;
        let script = String.escaped (Buffer.contents rc) in
        (*let script = Str.global_replace (!~ "\\\\n") "\n" script in*)
        fprintf outchan "\"\\\n%s\");\n\n" script;
      end !buffers;
      fprintf outchan "]\n\n" ;
      fprintf outchan "let _ = \n";
      fprintf outchan "  let exit_code = Sys.command \"where rc 1>NUL\" in\n";
      fprintf outchan "  if exit_code <> 0 then failwith \"Cannot find 'rc' command.\";\n";
      fprintf outchan "  let exit_code = Sys.command \"where cvtres 1>NUL\" in\n";
      fprintf outchan "  if exit_code <> 0 then failwith \"Cannot find 'cvtres' command.\";\n";
      (*  *)
      fprintf outchan "  let rcname, rc = try List.assoc Sys.argv.(1) resources with Not_found -> (try List.assoc (unquote Sys.argv.(1)) resources with Not_found -> kprintf failwith \"rc_compile.ml: resource name %%s not found\" Sys.argv.(1)) in\n";
      fprintf outchan "  let outchan = open_out_bin rcname in\n";
      fprintf outchan "  output_string outchan rc;\n";
      fprintf outchan "  close_out_noerr outchan;\n";
      (*  *)
      fprintf outchan "  let exit_code = Sys.command (\"rc /nologo \" ^ rcname) in\n";
      fprintf outchan "  let exit_code = if exit_code = 0 then Sys.command (\"cvtres /nologo /machine:x86 \" ^ (Filename.chop_extension rcname) ^ \".res\") else exit_code in\n";
      fprintf outchan "  if Sys.file_exists rcname then Sys.remove rcname;\n";
      fprintf outchan "  let name = (Filename.chop_extension rcname) ^ \".res\" in\n  if Sys.file_exists name then Sys.remove name;\n";
      fprintf outchan "  exit exit_code;\n";
      close_out_noerr outchan;
    with ex ->
      close_out_noerr outchan;
      raise ex
  end else begin
    if Sys.file_exists filename then Sys.remove filename;
  end
;;

(** write *)
let write proj =
  write_findlib_tools proj;
  write_subsystem_tools proj;
  write_resource_file proj;
