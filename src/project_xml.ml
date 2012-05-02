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


open Project
open Printf
open Miscellanea

(** write *)
let write proj =
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
    Xml.Element ("open_files", [],
      (List.map (fun (x, off, active) -> Xml.Element ("filename",
        ["offset", string_of_int off; "active", (string_of_bool active)],
        [Xml.PCData x])) proj.open_files));
    Xml.Element ("runtime", [],
      List.map begin fun t ->
        Xml.Element ("configuration", [
            "name", t.Rconf.name;
            "default", string_of_bool t.Rconf.default;
            "id_build", string_of_int t.Rconf.id_build;
            "id", string_of_int t.Rconf.id], [
          Xml.Element ("id", [], [Xml.PCData (string_of_int t.Rconf.id)]); (* Deprecated from version 1.7.1 *)
          Xml.Element ("id_build", [], [Xml.PCData (string_of_int t.Rconf.id_build)]); (* Deprecated from version 1.7.1 *)
          Xml.Element ("name", [], [Xml.PCData t.Rconf.name]); (* Deprecated from version 1.7.1 *)
          Xml.Element ("default", [], [Xml.PCData (string_of_bool t.Rconf.default)]); (* Deprecated from version 1.7.1 *)
          Xml.Element ("build_task", [], [Xml.PCData (Bconf.string_of_rbt t.Rconf.build_task)]);
          Xml.Element ("env", ["replace", string_of_bool t.Rconf.env_replace],
            List.map (fun (e, v) -> Xml.Element ("var", ["enabled", string_of_bool e], [Xml.PCData v])) t.Rconf.env
          );
          Xml.Element ("args", [],
            List.map (fun (e, v) -> Xml.Element ("arg", ["enabled", string_of_bool e], [Xml.PCData v])) t.Rconf.args);
        ])
      end proj.runtime
    );
    Xml.Element ("build", [],
      List.map begin fun t ->
        Xml.Element ("configuration", [
            "name", t.Bconf.name;
            "default", string_of_bool t.Bconf.default;
            "id", string_of_int t.Bconf.id], [
          Xml.Element ("id", [], [Xml.PCData (string_of_int t.Bconf.id)]); (* Deprecated from version 1.7.1 *)
          Xml.Element ("name", [], [Xml.PCData t.Bconf.name]);             (* Deprecated from version 1.7.1 *)
          Xml.Element ("default", [], [Xml.PCData (string_of_bool t.Bconf.default)]); (* Deprecated from version 1.7.1 *)
          Xml.Element ("byt", [], [Xml.PCData (string_of_bool t.Bconf.byt)]);
          Xml.Element ("opt", [], [Xml.PCData (string_of_bool t.Bconf.opt)]);
          Xml.Element ("libs", [], [Xml.PCData t.Bconf.libs]);
          Xml.Element ("other_objects", [], [Xml.PCData t.Bconf.other_objects]);
          Xml.Element ("files", [], [Xml.PCData t.Bconf.files]);
          Xml.Element ("includes", [], [Xml.PCData t.Bconf.includes]);
          Xml.Element ("thread", [], [Xml.PCData (string_of_bool t.Bconf.thread)]);
          Xml.Element ("vmthread", [], [Xml.PCData (string_of_bool t.Bconf.vmthread)]);
          Xml.Element ("pp", [], [Xml.PCData t.Bconf.pp]);
          Xml.Element ("cflags", [], [Xml.PCData t.Bconf.cflags]);
          Xml.Element ("lflags", [], [Xml.PCData t.Bconf.lflags]);
          Xml.Element ("outkind", [], [Xml.PCData (Bconf.string_of_outkind t.Bconf.outkind)]);
          Xml.Element ("outname", [], [Xml.PCData t.Bconf.outname]);
          Xml.Element ("lib_install_path", [], [Xml.PCData t.Bconf.lib_install_path]);
          Xml.Element ("external_tasks", [],
            List.map begin fun task ->
              Xml.Element ("task", ["name", task.Task.et_name], [
                Xml.Element ("name", [], [Xml.PCData (task.Task.et_name)]); (* Deprecated from version 1.7.1 *)
                Xml.Element ("always_run_in_project", [], [Xml.PCData (string_of_bool task.Task.et_always_run_in_project)]);
                Xml.Element ("always_run_in_script", [], [Xml.PCData (string_of_bool task.Task.et_always_run_in_script)]);
                Xml.Element ("env", ["replace", string_of_bool task.Task.et_env_replace],
                  List.map (fun (e, v) -> Xml.Element ("var", ["enabled", string_of_bool e], [Xml.PCData v])) task.Task.et_env);
                Xml.Element ("dir", [], [Xml.PCData (task.Task.et_dir)]);
                Xml.Element ("cmd", [], [Xml.PCData (task.Task.et_cmd)]);
                Xml.Element ("args", [],
                  List.map (fun (e, v) -> Xml.Element ("arg", ["enabled", string_of_bool e], [Xml.PCData v])) task.Task.et_args);
                Xml.Element ("phase", [], [Xml.PCData
                  (match task.Task.et_phase with Some x -> Task.string_of_phase x | _ -> "")]);
              ])
            end t.Bconf.external_tasks);
          Xml.Element ("restrictions", [], [Xml.PCData (String.concat "," t.Bconf.restrictions)]);
        ])
      end proj.build
    );
    Xml.Element ("build_script", ["filename", proj.build_script.Build_script.bs_filename],
      List.map begin fun arg ->
        Xml.Element ("arg", [
          "type", (Build_script_args.string_of_type arg.Build_script_args.bsa_type);
          "key", arg.Build_script_args.bsa_key;
          "pass", (Build_script_args.string_of_pass arg.Build_script_args.bsa_pass);
        ], [
          Xml.Element ("task",
            (let bc, et = arg.Build_script_args.bsa_task in [
              "bconf", string_of_int bc.Bconf.id;
              "task_name", et.Task.et_name;
            ]), []);
          Xml.Element ("mode", [], [Xml.PCData
            (match arg.Build_script_args.bsa_mode with `add -> Build_script_args.string_of_add | `replace x -> x)]);
          Xml.Element ("default", [
            "type", (match arg.Build_script_args.bsa_default with `flag _ -> "flag" | `bool _ -> "bool" | `string _ -> "string")
          ], [Xml.PCData
            (match arg.Build_script_args.bsa_default with `flag x -> string_of_bool x | `bool x -> string_of_bool x | `string x -> x)]);
          Xml.Element ("doc", [], [Xml.PCData arg.Build_script_args.bsa_doc]);
        ])
      end proj.build_script.Build_script.bs_args)
  ]);;

(** read *)
let read filename =
  let proj = create ~filename () in
  let parser = XmlParser.make () in
  let xml = XmlParser.parse parser (XmlParser.SFile filename) in
  let value xml =
    try String.concat "\n" (List.map Xml.pcdata (Xml.children xml))
    with Xml.Not_element _ -> ""
  in
  let identity x = x in
  let attrib node name f default = match List_opt.assoc name (Xml.attribs node) with Some v -> f v | _ -> default in
  let fattrib node name f default = match List_opt.assoc name (Xml.attribs node) with Some v -> f v | _ -> (default ()) in
  let get_offset xml = try int_of_string (Xml.attrib xml "offset") with Xml.No_attribute _ -> 0 in
  let get_active xml = try bool_of_string (Xml.attrib xml "active") with Xml.No_attribute _ -> false in
  let values node =
    List.rev (Xml.fold (fun acc x -> (value x) :: acc) [] node)
  in
  let rbt_map = ref [] in
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
      | "open_files" | "load_files" ->
        let files = Xml.fold (fun acc x -> ((value x), (get_offset x), (get_active x)) :: acc) [] node in
        proj.open_files <- List.rev files;
      | "runtime" ->
        let runtime = Xml.fold begin fun acc tnode ->
          let config  = {
            Rconf.id    = (attrib tnode "id" int_of_string 0);
            id_build    = (attrib tnode "id_build" int_of_string 0);
            name        = (attrib tnode "name" identity "");
            default     = (attrib tnode "default" bool_of_string false);
            build_task  = `NONE;
            env         = [];
            env_replace = false;
            args        = []
          } in
          Xml.iter begin fun tp ->
            match Xml.tag tp with
              | "id" -> config.Rconf.id <- int_of_string (value tp) (* Backward compatibility with 1.7.0 *)
              | "id_build" -> config.Rconf.id_build <- int_of_string (value tp) (* Backward compatibility with 1.7.0 *)
              | "name" -> config.Rconf.name <- value tp; (* Backward compatibility with 1.7.0 *)
              | "default" -> config.Rconf.default <- bool_of_string (value tp); (* Backward compatibility with 1.7.0 *)
              | "build_task" ->
                config.Rconf.build_task <- `NONE;
                rbt_map := (config, (value tp)) :: !rbt_map
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
        proj.runtime <- List.rev runtime;
      | "build" ->
        let i = ref 0 in
        let bconfigs = Xml.fold begin fun acc tnode ->
          let target = Bconf.create ~id:0 ~name:(sprintf "Config_%d" !i) in
          let runtime_build_task = ref "" in
          let runtime_env = ref (false, "") in
          let runtime_args = ref (false, "") in
          let create_default_runtime = ref false in
          target.Bconf.id <- attrib tnode "id" int_of_string 0;
          target.Bconf.name <- attrib tnode "name" identity "";
          target.Bconf.default <- attrib tnode "default" bool_of_string false;
          Xml.iter begin fun tp ->
            match Xml.tag tp with
              | "id" -> target.Bconf.id <- int_of_string (value tp) (* Backward compatibility with 1.7.0 *)
              | "name" -> target.Bconf.name <- value tp (* Backward compatibility with 1.7.0 *)
              | "default" -> target.Bconf.default <- bool_of_string (value tp) (* Backward compatibility with 1.7.0 *)
              | "byt" -> target.Bconf.byt <- bool_of_string (value tp)
              | "opt" -> target.Bconf.opt <- bool_of_string (value tp)
              | "libs" -> target.Bconf.libs <- value tp
              | "other_objects" -> target.Bconf.other_objects <- value tp
              | "mods" -> target.Bconf.other_objects <- value tp (*  *)
              | "files" -> target.Bconf.files <- value tp
              | "includes" -> target.Bconf.includes <- value tp
              | "thread" -> target.Bconf.thread <- bool_of_string (value tp)
              | "vmthread" -> target.Bconf.vmthread <- bool_of_string (value tp)
              | "pp" -> target.Bconf.pp <- value tp
              | "cflags" -> target.Bconf.cflags <- value tp
              | "lflags" -> target.Bconf.lflags <- value tp
              | "is_library" -> target.Bconf.outkind <- (if bool_of_string (value tp) then Bconf.Library else Bconf.Executable)
              | "outkind" -> target.Bconf.outkind <- Bconf.outkind_of_string (value tp)
              | "outname" -> target.Bconf.outname <- value tp
              | "runtime_build_task" ->
                runtime_build_task := (value tp);
                create_default_runtime := true;
              | "runtime_env" | "env" ->
                runtime_env := (attrib tp "enabed" bool_of_string true, value tp);
              | "runtime_args" | "run" ->
                runtime_args := (attrib tp "enabed" bool_of_string true, value tp);
              | "lib_install_path" -> target.Bconf.lib_install_path <- value tp
              | "external_tasks" ->
                let external_tasks = Xml.fold begin fun acc tnode ->
                  let task = Task.create ~name:"" ~env:[] ~dir:"" ~cmd:"" ~args:[] () in
                  task.Task.et_name <- attrib tnode "name" identity "";
                  Xml.iter begin fun tp ->
                    match Xml.tag tp with
                      | "name" -> task.Task.et_name <- value tp (* Backward compatibility with 1.7.0 *)
                      | "always_run" -> task.Task.et_always_run_in_project <- bool_of_string (value tp) (* Backward compatibility with 1.7.0 *)
                      | "always_run_in_project" -> task.Task.et_always_run_in_project <- bool_of_string (value tp)
                      | "always_run_in_script" -> task.Task.et_always_run_in_script <- bool_of_string (value tp)
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
                end [] tp in
                target.Bconf.external_tasks <- List.rev external_tasks;
              | "restrictions" -> target.Bconf.restrictions <- (Str.split (!~ ",") (value tp))
              | _ -> ()
          end tnode;
          incr i;
          (*target.Bconf.runtime_build_task <- Bconf.rbt_of_string target !runtime_build_task;*)
          if !create_default_runtime && target.Bconf.outkind = Bconf.Executable then begin
            proj.runtime <- {
              Rconf.id = (List.length proj.runtime);
              id_build    = target.Bconf.id;
              name        = target.Bconf.name;
              default     = target.Bconf.default;
              build_task  = Bconf.rbt_of_string target !runtime_build_task;
              env         = [!runtime_env];
              env_replace = false;
              args        = [!runtime_args]
            } :: proj.runtime;
          end;
          target :: acc;
        end [] node in
        proj.build <- List.rev bconfigs
      | "build_script" ->
        proj.build_script <- {
          Build_script.bs_filename = proj.root // (attrib node "filename" identity "");
          bs_args                  =
            Xml.map begin fun arg ->
              let bsa_doc     = ref "" in
              let bsa_mode    = ref `add in
              let bsa_default = ref (`flag false) in
              let bsa_task    = ref (None, None) in
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
                    bsa_default := begin
                      match fattrib tp "type" identity
                          (fun _ -> invalid_arg "from_file, build_script, default") with
                        | "flag" -> `flag (bool_of_string (value tp))
                        | "bool" -> `bool (bool_of_string (value tp))
                        | "string" -> `string (value tp)
                        | _ -> !bsa_default
                    end
                  | "task" ->
                    bsa_task := begin
                      let find_bconf id =
                        let id = int_of_string id in
                        List_opt.find (fun bc -> bc.Bconf.id = id) proj.build
                      in
                      let find_task name =
                        List_opt.find (fun et -> et.Task.et_name = name)
                          (List.flatten (List.map (fun bc -> bc.Bconf.external_tasks) proj.build))
                      in
                      let bconf = fattrib tp "bconf" find_bconf
                        (fun _ -> invalid_arg "from_file, build_script, task, bconf(1)") in
                      let task = fattrib tp "task_name" find_task
                        (fun _ -> invalid_arg "from_file, build_script, task, task(1)") in
                      bconf, task
                    end
                  | _ -> ()
              end arg;
              {Build_script_args.
                bsa_type    = fattrib arg "type" Build_script_args.type_of_string
                  (fun _ -> invalid_arg "from_file, build_script, type");
                bsa_key     = attrib arg "key" identity "";
                bsa_doc     = !bsa_doc;
                bsa_mode    = !bsa_mode;
                bsa_default = !bsa_default;
                bsa_task    = begin
                  match !bsa_task with
                    | Some bc, Some et -> bc, et
                    | _ -> invalid_arg "from_file, build_script, bsa_task"
                end;
                bsa_pass    = fattrib arg "pass" Build_script_args.pass_of_string
                  (fun _ -> invalid_arg "from_file, build_script, pass");
              }
            end node
        }
      | _ -> ()
  end xml;
  (* Patch build tasks connected to the runtime *)
  List.iter (fun (rconf, rbt_string) -> set_runtime_build_task proj rconf rbt_string) !rbt_map;
  (* Set default runtime configuration *)
  begin
    match List_opt.find (fun x -> x.Rconf.default) proj.runtime with
      | None ->
        (match proj.runtime with pr :: _ -> pr.Rconf.default <- true | _ -> ());
      | _ -> ()
  end;
  (* Translate ocamllib: "" -> 'ocamlc -where' *)
  set_ocaml_home ~ocamllib:proj.ocamllib proj;
  (*  *)
  proj;;

let init () =
  Project.write_xml := write;
  Project.read_xml := read

