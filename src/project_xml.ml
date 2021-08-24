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


open Project
open Printf
open Miscellanea
open Oe

(** write *)
let write proj =
  let open Prj in
  Xml.Element ("project", [], [
      Xml.Element ("ocaml_home", [], [Xml.PCData proj.ocaml_home]);
      Xml.Element ("ocamllib", [], [Xml.PCData
                                      (if proj.ocamllib_from_env then proj.ocamllib else "")]);
      Xml.Element ("encoding", [], [Xml.PCData (match proj.encoding with None -> "" | Some x -> x)]);
      Xml.Element ("name", [], [Xml.PCData proj.name]);
      Xml.Element ("author", [], [Xml.PCData proj.author]);
      (*Xml.Element ("description", [], [Xml.PCData proj.description]);*)
      Xml.Element ("description", [],
                   (List.map (fun x -> Xml.Element ("line", [], [Xml.PCData x])) (Miscellanea.split "\n" proj.description)));
      Xml.Element ("version", [], [Xml.PCData proj.version]);
      Xml.Element ("autocomp", [
          "enabled", string_of_bool proj.autocomp_enabled;
          "delay", string_of_float proj.autocomp_delay;
          "cflags", proj.autocomp_cflags], []);
      Xml.Element ("targets", [], List.map begin fun t ->
          Xml.Element ("target", [
              "name", t.Target.name;
              "default", string_of_bool t.Target.default;
              "id", string_of_int t.Target.id;
              "sub_targets", (String.concat "," (List.map (fun tg -> string_of_int tg.Target.id) t.Target.sub_targets));
              "is_fl_package", string_of_bool t.Target.is_fl_package;
              "subsystem", (match t.Target.subsystem with Some x -> Target.string_of_subsystem x | _ -> "");
              "readonly", string_of_bool t.Target.readonly;
              "visible", string_of_bool t.Target.visible;
              "node_collapsed", string_of_bool t.Target.node_collapsed;
            ], [
                         Xml.Element ("descr", [], [Xml.PCData (t.Target.descr)]);
                         Xml.Element ("byt", [], [Xml.PCData (string_of_bool t.Target.byt)]);
                         Xml.Element ("opt", [], [Xml.PCData (string_of_bool t.Target.opt)]);
                         Xml.Element ("libs", [], [Xml.PCData t.Target.libs]);
                         Xml.Element ("other_objects", [], [Xml.PCData t.Target.other_objects]);
                         Xml.Element ("files", [], [Xml.PCData t.Target.files]);
                         Xml.Element ("package", [], [Xml.PCData t.Target.package]);
                         Xml.Element ("includes", [], [Xml.PCData t.Target.includes]);
                         Xml.Element ("thread", [], [Xml.PCData (string_of_bool t.Target.thread)]);
                         Xml.Element ("vmthread", [], [Xml.PCData (string_of_bool t.Target.vmthread)]);
                         Xml.Element ("pp", [], [Xml.PCData t.Target.pp]);
                         Xml.Element ("inline", [], [Xml.PCData (match t.Target.inline with Some num -> string_of_int num | _ -> "")]);
                         Xml.Element ("nodep", [], [Xml.PCData (string_of_bool t.Target.nodep)]);
                         Xml.Element ("dontlinkdep", [], [Xml.PCData (string_of_bool t.Target.dontlinkdep)]);
                         Xml.Element ("dontaddopt", [], [Xml.PCData (string_of_bool t.Target.dontaddopt)]);
                         Xml.Element ("cflags", [], [Xml.PCData t.Target.cflags]);
                         Xml.Element ("lflags", [], [Xml.PCData t.Target.lflags]);
                         Xml.Element ("target_type", [], [Xml.PCData (Target.string_of_target_type t.Target.target_type)]);
                         Xml.Element ("outname", [], [Xml.PCData t.Target.outname]);
                         Xml.Element ("lib_install_path", [], [Xml.PCData t.Target.lib_install_path]);
                         Xml.Element ("external_tasks", [],
                                      List.map begin fun task ->
                                        Xml.Element ("task", ["name", task.Task.et_name], [
                                            Xml.Element ("always_run_in_project", [], [Xml.PCData (string_of_bool task.Task.et_always_run_in_project)]);
                                            Xml.Element ("always_run_in_script", [], [Xml.PCData (string_of_bool task.Task.et_always_run_in_script)]);
                                            Xml.Element ("readonly", [], [Xml.PCData (string_of_bool task.Task.et_readonly)]);
                                            Xml.Element ("visible", [], [Xml.PCData (string_of_bool task.Task.et_visible)]);
                                            Xml.Element ("env", ["replace", string_of_bool task.Task.et_env_replace],
                                                         List.map (fun (e, v) -> Xml.Element ("var", ["enabled", string_of_bool e], [Xml.PCData v])) task.Task.et_env);
                                            Xml.Element ("dir", [], [Xml.PCData (task.Task.et_dir)]);
                                            Xml.Element ("cmd", [], [Xml.PCData (task.Task.et_cmd)]);
                                            Xml.Element ("args", [],
                                                         List.map (fun (e, v) -> Xml.Element ("arg", ["enabled", string_of_bool e], [Xml.PCData v])) task.Task.et_args);
                                            Xml.Element ("phase", [], [Xml.PCData
                                                                         (match task.Task.et_phase with Some x -> Task.string_of_phase x | _ -> "")]);
                                          ])
                                      end t.Target.external_tasks);
                         Xml.Element ("restrictions", [], [Xml.PCData (String.concat "&" t.Target.restrictions)]);
                         Xml.Element ("dependencies", [], [Xml.PCData (String.concat "," (List.map string_of_int t.Target.dependencies))]);
                       ] @ (match t.Target.resource_file with
              | None -> []
              | Some rc -> [
                  Xml.Element ("resource_file", [],
                               let open Resource_file in [
                                 Xml.Element ("filename", [], [Xml.PCData rc.rc_filename]);
                                 Xml.Element ("title", [], [Xml.PCData rc.rc_title]);
                                 Xml.Element ("company", [], [Xml.PCData rc.rc_company]);
                                 Xml.Element ("product", [], [Xml.PCData rc.rc_product]);
                                 Xml.Element ("copyright", [], [Xml.PCData rc.rc_copyright]);
                                 Xml.Element ("file_version", [], [Xml.PCData (match rc.rc_file_version with a,b,c,d -> sprintf "%d.%d.%d.%d" a b c d)]);
                                 Xml.Element ("icons", [], List.map (fun (fn(*, _*)) -> Xml.Element ("icon", [], [Xml.PCData fn])) rc.rc_icons(*_data*));
                               ])])
            ) end proj.targets);
      Xml.Element ("executables", [], List.map begin fun t ->
          Xml.Element ("executable", [
              "name", t.Rconf.name;
              "default", string_of_bool t.Rconf.default;
              "target_id", string_of_int t.Rconf.target_id;
              "id", string_of_int t.Rconf.id], [
                         Xml.Element ("build_task", [], [Xml.PCData (Target.string_of_task t.Rconf.build_task)]);
                         Xml.Element ("env", ["replace", string_of_bool t.Rconf.env_replace],
                                      List.map (fun (e, v) -> Xml.Element ("var", ["enabled", string_of_bool e], [Xml.PCData v])) t.Rconf.env
                                     );
                         Xml.Element ("args", [],
                                      List.map (fun (e, v) -> Xml.Element ("arg", ["enabled", string_of_bool e], [Xml.PCData v])) t.Rconf.args);
                       ])
        end proj.executables);
      Xml.Element ("build_script", ["filename", proj.build_script.Build_script.bs_filename],
                   let targets =
                     List.map begin fun target ->
                       Xml.Element ("target", [
                           "target_id", (string_of_int target.Build_script.bst_target.Target.id);
                           "show", (string_of_bool target.Build_script.bst_show);
                         ], [])
                     end proj.build_script.Build_script.bs_targets
                   in
                   let args =
                     List.map begin fun arg ->
                       Xml.Element ("arg", [
                           "id", (string_of_int arg.Build_script_args.bsa_id);
                           "type", (Build_script_args.string_of_type arg.Build_script_args.bsa_type);
                           "key", arg.Build_script_args.bsa_key;
                           "pass", (Build_script_args.string_of_pass arg.Build_script_args.bsa_pass);
                           "command", (Build_script_command.string_of_command arg.Build_script_args.bsa_cmd);
                         ], [
                                      Xml.Element ("task",
                                                   (match arg.Build_script_args.bsa_task with Some (bc, et) -> [
                                                         "target_id", string_of_int bc.Target.id;
                                                         "task_name", et.Task.et_name;
                                                       ] | None -> []), []);
                                      Xml.Element ("mode", [], [Xml.PCData
                                                                  (match arg.Build_script_args.bsa_mode with `add -> Build_script_args.string_of_add | `replace x -> x)]);
                                      Xml.Element ("default", [
                                          "type", (match arg.Build_script_args.bsa_default with `flag _ -> "flag" | `bool _ -> "bool" | `string _ -> "string");
                                          "override", (string_of_bool arg.Build_script_args.bsa_default_override);
                                        ], [Xml.PCData
                                              (match arg.Build_script_args.bsa_default with `flag x -> string_of_bool x | `bool x -> string_of_bool x | `string x -> x)]);
                                      Xml.Element ("doc", [], [Xml.PCData arg.Build_script_args.bsa_doc]);
                                    ])
                     end proj.build_script.Build_script.bs_args
                   in
                   let commands =
                     List.map begin fun cmd ->
                       Xml.Element ("command", [
                           "name", (Build_script.string_of_command cmd.Build_script.bsc_name);
                           "descr", cmd.Build_script.bsc_descr;
                           "target_id", string_of_int cmd.Build_script.bsc_target.Target.id;
                           "task_name", (cmd.Build_script.bsc_task.Task.et_name);
                         ], [])
                     end proj.build_script.Build_script.bs_commands
                   in [
                     Xml.Element ("targets", [], targets);
                     Xml.Element ("args", [], args);
                     Xml.Element ("commands", [], commands);
                   ])
    ]);;

let value xml =
  try String.concat "\n" (List.map Xml.pcdata (Xml.children xml)) with Xml.Not_element _ -> "";;

let fold node tag f init =
  Xml.fold begin fun acc node ->
    if Xml.tag node = tag then f node else acc end init node;;

let attrib node name f default =
  match List_opt.assoc name (Xml.attribs node) with Some v -> f v | _ -> default;;

let fattrib node name f default =
  match List_opt.assoc name (Xml.attribs node) with Some v -> f v | _ -> (default ());;

(** xml_bs_targets *)
let xml_bs_targets proj node =
  let open Prj in
  List.rev (Xml.fold begin fun acc target_node ->
      try
        {Build_script.
          bst_target         = (match fattrib target_node "target_id" (find_target_string proj) (fun _ -> None) with Some x -> x | _ -> raise Exit);
          bst_show           = fattrib target_node "show" bool_of_string (fun () -> true);
        } :: acc
      with Exit -> acc
    end [] node);;

(** xml_bs_args *)
let xml_bs_args proj node =
  let open Prj in
  let count_bsa_id = ref 0 in
  Xml.map begin fun arg ->
    let bsa_doc              = ref "" in
    let bsa_mode             = ref `add in
    let bsa_default_override = ref true in
    let bsa_default          = ref (`flag false) in
    let bsa_task             = ref (None, None) in
    Xml.iter begin fun tp ->
      match Xml.tag tp with
      | "doc" -> bsa_doc := value tp
      | "mode" ->
          bsa_mode := begin
            match value tp with
            | x when x = Build_script_args.string_of_add -> `add
            | x -> `replace x
          end
      | "default" ->
          bsa_default_override := attrib tp "override" bool_of_string !bsa_default_override;
          bsa_default := begin
            match fattrib tp "type" (fun x -> x) (fun () -> "string") with
            | "flag" -> `flag (bool_of_string (value tp))
            | "bool" -> `bool (bool_of_string (value tp))
            | "string" -> `string (value tp)
            | _ -> !bsa_default
          end
      | "task" ->
          bsa_task := begin
            let target = fattrib tp "target_id" (find_target_string proj) (fun _ -> None) in
            let task = fattrib tp "task_name" (find_task proj) (fun _ -> None) in
            target, task
          end
      | _ -> ()
    end arg;
    {Build_script_args.
      bsa_id               = fattrib arg "id" int_of_string (fun () -> incr count_bsa_id; !count_bsa_id);
      bsa_type             = fattrib arg "type" Build_script_args.type_of_string (fun _ -> Build_script_args.String);
      bsa_key              = attrib arg "key" (fun x -> x) "";
      bsa_doc              = !bsa_doc;
      bsa_mode             = !bsa_mode;
      bsa_default_override = !bsa_default_override;
      bsa_default          = !bsa_default;
      bsa_task             = begin
        match !bsa_task with
        | Some bc, Some et -> Some (bc, et)
        | _ -> None
      end;
      bsa_pass             = fattrib arg "pass" Build_script_args.pass_of_string (fun _ -> `key_value);
      bsa_cmd              = fattrib arg "command" Build_script_command.command_of_string (fun _ -> `Show);
    }
  end node;;

(** xml_commands *)
let xml_commands proj node =
  let open Prj in
  List.rev (Xml.fold begin fun acc target_node ->
      try
        {Build_script.
          bsc_name   = (fattrib target_node "name" Build_script.command_of_string (fun _ -> raise Exit));
          bsc_descr  = (attrib target_node "descr" (fun x -> x) "");
          bsc_target = (match fattrib target_node "target_id" (find_target_string proj) (fun _ -> None) with Some x -> x | _ -> raise Exit);
          bsc_task   = fattrib target_node "task_name"
              (fun y -> match find_task proj y with Some x -> x | _ -> raise Exit) (fun _ -> raise Exit)
        } :: acc
      with Exit -> acc
    end [] node);;

(** read *)
let read filename =
  let open Prj in
  let proj = create ~filename () in
  let parser = XmlParser.make () in
  let xml = XmlParser.parse parser (XmlParser.SFile filename) in
  let get_offset xml = try int_of_string (Xml.attrib xml "offset") with Xml.No_attribute _ -> 0 in
  let get_active xml = try bool_of_string (Xml.attrib xml "active") with Xml.No_attribute _ -> false in
  let task_map = ref [] in
  Xml.iter begin fun node ->
    match Xml.tag node with
    | "ocaml_home" -> proj.ocaml_home <- value node
    | "ocamllib" -> proj.ocamllib <- value node
    | "encoding" -> proj.encoding <- (match value node with "" -> None | x -> Some x)
    | "name" -> proj.name <- value node
    | "author" -> proj.author <- value node
    | "description" -> proj.description <- String.concat "\n" (Xml.map value node)
    | "version" -> proj.version <- value node
    | "autocomp" ->
        proj.autocomp_enabled <- (attrib node "enabled" bool_of_string true);
        proj.autocomp_delay <- (float_of_string (Xml.attrib node "delay"));
        proj.autocomp_cflags <- (Xml.attrib node "cflags");
    | "open_files" | "load_files" -> (* backward compatibility with 1.7.2 *)
        let files = Xml.fold (fun acc x -> ((value x), 0, (get_offset x), (get_active x)) :: acc) [] node in
        proj.open_files <- List.rev files;
    | "executables" | "runtime" (* Backward compatibility with 1.7.5 *) ->
        let runtime = Xml.fold begin fun acc tnode ->
            let config  = {
              Rconf.id    = (attrib tnode "id" int_of_string 0);
              target_id   = (try (attrib tnode "target_id" int_of_string 0) with Xml.No_attribute _ -> attrib tnode "id_build" int_of_string 0); (* Backward compatibility with 1.7.5 *)
              name        = (attrib tnode "name" (fun x -> x) "");
              default     = (attrib tnode "default" bool_of_string false);
              build_task  = `NONE;
              env         = [];
              env_replace = false;
              args        = []
            } in
            Xml.iter begin fun tp ->
              match Xml.tag tp with
              | "id" -> config.Rconf.id <- int_of_string (value tp) (* Backward compatibility with 1.7.0 *)
              | "id_build" -> config.Rconf.target_id <- int_of_string (value tp) (* Backward compatibility with 1.7.0 *)
              | "name" -> config.Rconf.name <- value tp; (* Backward compatibility with 1.7.0 *)
              | "default" -> config.Rconf.default <- bool_of_string (value tp); (* Backward compatibility with 1.7.0 *)
              | "build_task" ->
                  config.Rconf.build_task <- `NONE;
                  task_map := (config, (value tp)) :: !task_map
              | "env" ->
                  config.Rconf.env <-
                    List.rev (Xml.fold (fun acc var ->
                        (attrib var "enabled" bool_of_string true, value var) :: acc) [] tp);
                  config.Rconf.env_replace <- (try bool_of_string (Xml.attrib tp "replace") with Xml.No_attribute _ -> false)
              | "args" ->
                  begin
                    try
                      config.Rconf.args <-
                        List.rev (Xml.fold (fun acc arg ->
                            (attrib arg "enabled" bool_of_string true, value arg) :: acc) [] tp);
                    with Xml.Not_element _ -> (config.Rconf.args <- [true, (value tp)])
                  end;
              | _ -> ()
            end tnode;
            config :: acc
          end [] node in
        proj.executables <- List.rev runtime;
    | "targets" | "build" (* Backward compatibility with 1.7.5 *) ->
        let i = ref 0 in
        let sub_targets = ref [] in
        let open Target in
        let targets =
          Xml.fold begin fun acc tnode ->
            let target = Target.create ~id:0 ~name:(sprintf "Config_%d" !i) in
            let runtime_build_task = ref "" in
            let runtime_env = ref (false, "") in
            let runtime_args = ref (false, "") in
            let create_default_runtime = ref false in
            target.id <- attrib tnode "id" int_of_string 0;
            target.Target.name <- attrib tnode "name" (fun x -> x) "";
            target.default <- attrib tnode "default" bool_of_string false;
            sub_targets := (target.id, List.map int_of_string (attrib tnode "sub_targets" (Str.split (Str.regexp "[;, ]+")) [])) :: !sub_targets;
            target.is_fl_package <- attrib tnode "is_fl_package" bool_of_string false;
            target.subsystem <- (try attrib tnode "subsystem" (fun x -> Some (Target.subsystem_of_string x)) None with Failure _ -> None);
            target.readonly <- attrib tnode "readonly" bool_of_string false;
            target.visible <- attrib tnode "visible" bool_of_string true;
            target.node_collapsed <- attrib tnode "node_collapsed" bool_of_string false;
            Xml.iter begin fun tp ->
              match Xml.tag tp with
              | "descr" -> target.descr <- value tp
              | "id" -> target.id <- int_of_string (value tp) (* Backward compatibility with 1.7.0 *)
              | "name" -> target.Target.name <- value tp (* Backward compatibility with 1.7.0 *)
              | "default" -> target.default <- bool_of_string (value tp) (* Backward compatibility with 1.7.0 *)
              | "byt" -> target.byt <- bool_of_string (value tp)
              | "opt" -> target.opt <- bool_of_string (value tp)
              | "libs" -> target.libs <- value tp
              | "other_objects" -> target.other_objects <- value tp
              | "mods" -> target.other_objects <- value tp (*  *)
              | "files" -> target.Target.files <- value tp
              | "package" -> target.package <- value tp
              | "includes" -> target.includes <- value tp
              | "thread" -> target.thread <- bool_of_string (value tp)
              | "vmthread" -> target.vmthread <- bool_of_string (value tp)
              | "pp" -> target.pp <- value tp
              | "inline" -> target.inline <- (let x = value tp in if x = "" then None else Some (int_of_string x))
              | "nodep" -> target.nodep <- bool_of_string (value tp)
              | "dontlinkdep" -> target.dontlinkdep <- bool_of_string (value tp)
              | "dontaddopt" -> target.dontaddopt <- bool_of_string (value tp)
              | "cflags" -> target.cflags <- value tp
              | "lflags" -> target.lflags <- value tp
              | "is_library" -> target.target_type <- (if bool_of_string (value tp) then Target.Library else Target.Executable)
              | "target_type" | "outkind" -> target.target_type <- target_type_of_string (value tp)
              | "outname" -> target.outname <- value tp
              | "runtime_build_task" ->
                  runtime_build_task := (value tp);
                  create_default_runtime := true;
              | "runtime_env" | "env" ->
                  runtime_env := (attrib tp "enabed" bool_of_string true, value tp);
              | "runtime_args" | "run" ->
                  runtime_args := (attrib tp "enabed" bool_of_string true, value tp);
              | "lib_install_path" -> target.lib_install_path <- value tp
              | "resource_file" ->
                  let rc = Resource_file.create () in
                  Xml.iter begin fun nrc ->
                    let open Resource_file in
                    match Xml.tag nrc with
                    | "filename" ->
                        target.resource_file <- Some rc;
                        rc.rc_filename <- value nrc
                    | "title" -> rc.rc_title <- value nrc
                    | "company" -> rc.rc_company <- value nrc
                    | "product" -> rc.rc_product <- value nrc
                    | "file_version" -> rc.rc_file_version <- (match Str.split (!~ "\\.") (value nrc) with
                        | [a;b;c;d] -> int_of_string a, int_of_string b, int_of_string c, int_of_string d | _ -> assert false)
                    | "copyright" -> rc.rc_copyright <- value nrc
                    | "icons" ->
                        rc.rc_icons(*_data*) <- List.rev (Xml.fold (fun acc name -> (value name(*, Buffer.create 1000*)) :: acc) [] nrc);
                    | _ -> ()
                  end tp;
              | "external_tasks" ->
                  let external_tasks =
                    Xml.fold begin fun acc tnode ->
                      let task = Task.create ~name:"" ~env:[] ~dir:"" ~cmd:"" ~args:[] () in
                      task.Task.et_name <- attrib tnode "name" (fun x -> x) "";
                      Xml.iter begin fun tp ->
                        match Xml.tag tp with
                        | "name" -> task.Task.et_name <- value tp (* Backward compatibility with 1.7.0 *)
                        | "always_run" -> task.Task.et_always_run_in_project <- bool_of_string (value tp) (* Backward compatibility with 1.7.0 *)
                        | "always_run_in_project" -> task.Task.et_always_run_in_project <- bool_of_string (value tp)
                        | "always_run_in_script" -> task.Task.et_always_run_in_script <- bool_of_string (value tp)
                        | "readonly" -> task.Task.et_readonly <- bool_of_string (value tp)
                        | "visible" -> task.Task.et_visible <- bool_of_string (value tp)
                        | "env" ->
                            task.Task.et_env <-
                              List.rev (Xml.fold (fun acc var ->
                                  (attrib var "enabled" bool_of_string true, value var) :: acc) [] tp);
                            task.Task.et_env_replace <- (try bool_of_string (Xml.attrib tp "replace") with Xml.No_attribute _ -> false)
                        | "dir" -> task.Task.et_dir <- value tp
                        | "cmd" -> task.Task.et_cmd <- value tp
                        | "args" ->
                            task.Task.et_args <-
                              List.rev (Xml.fold (fun acc arg ->
                                  (attrib arg "enabled" bool_of_string true, value arg) :: acc) [] tp);
                        | "phase" -> task.Task.et_phase <-
                              (match value tp with "" -> None | x -> Some (Task.phase_of_string x))
                        | _ -> ()
                      end tnode;
                      task :: acc
                    end [] tp
                  in
                  target.external_tasks <- List.rev external_tasks;
              | "restrictions" -> target.restrictions <- (Str.split (!~ "&") (value tp))
              | "dependencies" -> target.dependencies <- (List.map int_of_string (Str.split (!~ ",") (value tp)))
              | _ -> ()
            end tnode;
            incr i;
            (*target.runtime_build_task <- Target.task_of_string target !runtime_build_task;*)
            if !create_default_runtime && target.target_type = Target.Executable then begin
              proj.executables <- {
                Rconf.id    = (List.length proj.executables);
                target_id   = target.id;
                name        = target.Target.name;
                default     = target.default;
                build_task  = Target.task_of_string target !runtime_build_task;
                env         = [!runtime_env];
                env_replace = false;
                args        = [!runtime_args]
              } :: proj.executables;
            end;
            target :: acc;
          end [] node
        in
        proj.targets <- List.rev targets;
        List.iter begin fun tg ->
          tg.sub_targets <-
            let ids = try List.assoc tg.id !sub_targets with Not_found -> [] in
            List.map (fun id -> try List.find (fun t -> t.id = id) proj.targets with Not_found -> assert false) ids
        end proj.targets
    | "build_script" ->
        let filename = (attrib node "filename" (fun x -> x) "") in
        proj.build_script <- {Build_script.
                               bs_filename = filename;
                               bs_targets  = fold node "targets" (xml_bs_targets proj) [];
                               bs_args     = fold node "args" (xml_bs_args proj) [];
                               bs_commands = fold node "commands" (xml_commands proj) [];
                             }
    | _ -> ()
  end xml;
  (* Patch build tasks connected to the runtime *)
  List.iter (fun (rconf, task_string) -> set_runtime_build_task proj rconf task_string) !task_map;
  (* Set default runtime configuration *)
  begin
    match List_opt.find (fun x -> x.Rconf.default) proj.executables with
    | None ->
        (match proj.executables with pr :: _ -> pr.Rconf.default <- true | _ -> ());
    | _ -> ()
  end;
  (* Translate ocamllib: "" -> 'ocamlc -where' *)
  set_ocaml_home ~ocamllib:proj.ocamllib proj;
  (*  *)
  proj;;

(** from_local_xml *)
let from_local_xml proj =
  let open Prj in
  let filename = Project.filename_local proj in
  let filename = if Sys.file_exists filename then filename else Project.mk_old_filename_local proj in
  if Sys.file_exists filename then begin
    let parser = XmlParser.make () in
    let xml = XmlParser.parse parser (XmlParser.SFile filename) in
    let value xml =
      try String.concat "\n" (List.map Xml.pcdata (Xml.children xml))
      with Xml.Not_element _ -> ""
    in
    let get_int name xml = try int_of_string (Xml.attrib xml name) with Xml.No_attribute _ -> 0 in
    let get_cursor xml = try int_of_string (Xml.attrib xml "cursor") with Xml.No_attribute _ -> 0 in
    let get_active xml = try bool_of_string (Xml.attrib xml "active") with Xml.No_attribute _ -> false in
    Xml.iter begin fun node ->
      match Xml.tag node with
      | "open_files" ->
          let files = Xml.fold (fun acc x -> ((value x), (get_int "scroll" x), (get_cursor x), (get_active x)) :: acc) [] node in
          proj.open_files <- List.rev files;
      | "bookmarks" ->
          Xml.iter begin fun xml ->
            let bm = {
              bm_filename = (value xml);
              bm_loc      = Offset (get_int "offset" xml);
              bm_num      = (get_int "num" xml);
              bm_marker   = None;
            } in
            Project.set_bookmark bm proj
          end node;

      | _ -> ()
    end xml;
  end;;

let init () =
  Project.write_xml := write;
  Project.read_xml := read;
  Project.from_local_xml := from_local_xml;



