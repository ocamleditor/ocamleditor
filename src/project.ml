
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

exception Project_already_exists of string

exception Cannot_rename of string

type t = {
  (* ROOT directory of the project (non-persistent) *)
  mutable root               : string;
  mutable ocaml_home         : string; (* Unused *)
  (* Full path of the std. lib., either from the OCAMLLIB e. v. or from 'ocamlc -where';
     when written to the XML project file, if equals to 'ocamlc -where', it is translated in "". *)
  mutable ocamllib           : string;
  mutable ocamllib_from_env  : bool;
  mutable encoding           : string option;
  mutable name               : string;
  mutable modified           : bool;
  mutable author             : string;
  mutable description        : string;
  mutable version            : string;
  mutable files              : (File.file * int) list; (* Currently open files in the editor (non-persistent) *)
  mutable open_files         : (string * int * bool) list; (* filename, current offset, active *)
  mutable build              : Bconf.t list; (* Build configurations *)
  mutable runtime            : Rconf.t list; (* Runtime configurations *)
  mutable autocomp_enabled   : bool;
  mutable autocomp_delay     : float;
  mutable autocomp_cflags    : string;
  mutable autocomp_compiler  : string;
  mutable in_source_path     : string -> string option;
  mutable source_paths       : string list;
  mutable can_compile_native : bool;
  mutable symbols            : Oe.symbol_cache;
}

let extension = ".xml"
let src = "src"
let bak = "bak"
let tmp = ".tmp"

let path_src p = p.root // src
let path_bak p = p.root // bak
let path_tmp p = p.root // tmp
let path_cache p = p.root // ".cache"


(** abs_of_tmp *)
let abs_of_tmp proj filename =
  match Miscellanea.filename_relative (".." // tmp) filename with
    | None -> filename
    | Some relname -> (path_src proj) // relname

(** set_ocaml_home *)
let set_ocaml_home ~ocamllib project =
  let ocamllib, from_env =
    match ocamllib with
      | "" -> Ocaml_config.ocamllib (), false
      | path when Sys.file_exists path -> path, true
      | _ -> Ocaml_config.ocamllib (), false
  in
  project.ocaml_home <- "";
  project.ocamllib <- ocamllib;
  project.ocamllib_from_env <- from_env;
  Unix.putenv "OCAML_HOME" project.ocaml_home;
  Ocaml_config.putenv_ocamllib (Some project.ocamllib);
  project.autocomp_compiler <- sprintf "%s -c -thread -annot" (Ocaml_config.ocamlc());
  ignore (Thread.create begin fun () ->
    project.can_compile_native <- (Ocaml_config.can_compile_native ~ocaml_home:project.ocaml_home ()) <> None;
  end ())

(** can_compile_native *)
let can_compile_native proj = (Ocaml_config.can_compile_native ~ocaml_home:proj.ocaml_home ()) <> None

(** create *)
let create ~filename () =
  let root = Filename.dirname filename in
  let ocamllib, from_env = match Oe_config.getenv_ocamllib with None -> "", false | Some x -> x, true in
  let proj = {
    root               = root;
    ocaml_home         = "";
    ocamllib           = ocamllib;
    ocamllib_from_env  = from_env;
    encoding           = Some "UTF-8";
    name               = Filename.chop_extension (Filename.basename filename);
    modified           = true;
    author             = "";
    description        = "";
    version            = "1.0.0";
    files              = [];
    open_files         = [];
    build              = [];
    runtime            = [];
    autocomp_enabled   = true;
    autocomp_delay     = 1.0;
    autocomp_cflags    = "";
    autocomp_compiler  = "";
    in_source_path     = Miscellanea.filename_relative (root // src);
    source_paths       = (try File.readtree (root // src) with Sys_error _ -> []);
    can_compile_native = true;
    symbols            = {
      Oe.syt_table = [];
      syt_ts       = Hashtbl.create 7;
      syt_odoc     = Hashtbl.create 7;
      syt_critical = Mutex.create()
    };
  } in
  proj;;

(** set_runtime_build_task *)
let set_runtime_build_task proj rconf rbt_string =
  rconf.Rconf.build_task <- try
    let bconf = List.find (fun b -> b.Bconf.id = rconf.Rconf.id_build) proj.build in
    Bconf.rbt_of_string bconf rbt_string
  with Not_found -> `NONE

(** to_xml *)
let to_xml proj =
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
        Xml.Element ("configuration", [], [
          Xml.Element ("id", [], [Xml.PCData (string_of_int t.Rconf.id)]);
          Xml.Element ("id_build", [], [Xml.PCData (string_of_int t.Rconf.id_build)]);
          Xml.Element ("name", [], [Xml.PCData t.Rconf.name]);
          Xml.Element ("default", [], [Xml.PCData (string_of_bool t.Rconf.default)]);
          Xml.Element ("build_task", [], [Xml.PCData (Bconf.string_of_rbt t.Rconf.build_task)]);
          Xml.Element ("env", ["replace", string_of_bool t.Rconf.env_replace],
            List.map (fun e -> Xml.Element ("var", [], [Xml.PCData e])) t.Rconf.env
          );
          Xml.Element ("args", [], [Xml.PCData t.Rconf.args]);
        ])
      end proj.runtime
    );
    Xml.Element ("build", [],
      List.map begin fun t ->
        Xml.Element ("configuration", [], [
          Xml.Element ("id", [], [Xml.PCData (string_of_int t.Bconf.id)]);
          Xml.Element ("name", [], [Xml.PCData t.Bconf.name]);
          Xml.Element ("default", [], [Xml.PCData (string_of_bool t.Bconf.default)]);
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
              Xml.Element ("task", [], [
                Xml.Element ("name", [], [Xml.PCData (task.Task.name)]);
                Xml.Element ("always_run", [], [Xml.PCData (string_of_bool task.Task.always_run)]);
                Xml.Element ("env", ["replace", string_of_bool task.Task.env_replace],
                  List.map (fun x -> Xml.Element ("var", [], [Xml.PCData x])) task.Task.env);
                Xml.Element ("dir", [], [Xml.PCData (task.Task.dir)]);
                Xml.Element ("cmd", [], [Xml.PCData (task.Task.cmd)]);
                Xml.Element ("args", [],
                  List.map (fun x -> Xml.Element ("arg", [], [Xml.PCData x])) task.Task.args);
                Xml.Element ("phase", [], [Xml.PCData
                  (match task.Task.phase with Some x -> Task.string_of_phase x | _ -> "")]);
              ])
            end t.Bconf.external_tasks)
        ])
      end proj.build
    );
  ]);;

(** from_file *)
let from_file filename =
  let proj = create ~filename () in
  let parser = XmlParser.make () in
  let xml = XmlParser.parse parser (XmlParser.SFile filename) in
  let value xml =
    try String.concat "\n" (List.map Xml.pcdata (Xml.children xml))
    with Xml.Not_element _ -> ""
  in
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
        proj.autocomp_enabled <- (bool_of_string (Xml.attrib node "enabled"));
        proj.autocomp_delay <- (float_of_string (Xml.attrib node "delay"));
        proj.autocomp_cflags <- (Xml.attrib node "cflags");
      | "open_files" | "load_files" ->
        let files = Xml.fold (fun acc x -> ((value x), (get_offset x), (get_active x)) :: acc) [] node in
        proj.open_files <- List.rev files;
      | "runtime" ->
        let runtime = Xml.fold begin fun acc tnode ->
          let config =
            { Rconf.id = 0; id_build = 0; name = ""; default = false; build_task = `NONE; env = []; env_replace = false; args = "" } in
          Xml.iter begin fun tp ->
            match Xml.tag tp with
              | "id" -> config.Rconf.id <- int_of_string (value tp)
              | "id_build" -> config.Rconf.id_build <- int_of_string (value tp)
              | "name" -> config.Rconf.name <- value tp;
              | "default" -> config.Rconf.default <- bool_of_string (value tp);
              | "build_task" ->
                config.Rconf.build_task <- `NONE;
                rbt_map := (config, (value tp)) :: !rbt_map
              | "env" ->
                config.Rconf.env <- (Xml.map value tp);
                config.Rconf.env_replace <- (try bool_of_string (Xml.attrib tp "replace") with Xml.No_attribute _ -> false)
              | "args" -> config.Rconf.args <- value tp
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
          let runtime_env = ref "" in
          let runtime_args = ref "" in
          let create_default_runtime = ref false in
          Xml.iter begin fun tp ->
            match Xml.tag tp with
              | "id" -> target.Bconf.id <- int_of_string (value tp)
              | "name" -> target.Bconf.name <- value tp
              | "default" -> target.Bconf.default <- bool_of_string (value tp)
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
                runtime_env := value tp;
                (*target.Bconf.runtime_env <- value tp*)
              | "runtime_args" | "run" ->
                runtime_args := value tp;
                (*target.Bconf.runtime_args <- value tp*)
              | "lib_install_path" -> target.Bconf.lib_install_path <- value tp
              | "external_tasks" ->
                let external_tasks = Xml.fold begin fun acc tnode ->
                  let task = Task.create ~name:"" ~env:[] ~dir:"" ~cmd:"" ~args:[] () in
                  Xml.iter begin fun tp ->
                    match Xml.tag tp with
                      | "name" -> task.Task.name <- value tp
                      | "always_run" -> task.Task.always_run <- bool_of_string (value tp)
                      | "env" ->
                        task.Task.env <- values tp;
                        task.Task.env_replace <- (try bool_of_string (Xml.attrib tp "replace") with Xml.No_attribute _ -> false)
                      | "dir" -> task.Task.dir <- value tp
                      | "cmd" -> task.Task.cmd <- value tp
                      | "args" -> task.Task.args <- values tp
                      | "phase" -> task.Task.phase <-
                        (match value tp with "" -> None | x -> Some (Task.phase_of_string x))
                      | _ -> ()
                  end tnode;
                  task :: acc
                end [] tp in
                target.Bconf.external_tasks <- List.rev external_tasks;
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
              args        = !runtime_args
            } :: proj.runtime;
          end;
          target :: acc;
        end [] node in
        proj.build <- List.rev bconfigs
      | _ -> ()
  end xml;
  (* Patch build tasks connected to the runtime *)
  List.iter begin fun (rconf, rbt_string) ->
    set_runtime_build_task proj rconf rbt_string
  end !rbt_map;
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

(** convert_to_utf8 *)
let convert_to_utf8 proj text = match proj.encoding with
  | None -> Convert.to_utf8 text
  | Some from_codeset -> Glib.Convert.convert ~from_codeset ~to_codeset:"utf8" text

(** convert_from_utf8 *)
let convert_from_utf8 proj text = match proj.encoding with
  | None -> Convert.from_utf8 text
  | Some to_codeset -> Glib.Convert.convert ~from_codeset:"utf8" ~to_codeset text

(** Returns the full filename of the project. *)
let filename proj = Filename.concat proj.root (proj.name ^ extension)

(** get_includes*)
let get_includes =
  let re = Str.regexp "[ \t\r\n]+" in
  fun proj ->
    let includes = String.concat " " (List.map (fun t -> t.Bconf.includes) proj.build) in
    let includes = Str.split re includes in
    Xlist.remove_dupl includes

(** Returns the {i load path} of the project: includes and [src]. *)
let get_load_path proj =
  let includes = get_includes proj in
  let includes = "+threads" :: includes in
  let ocamllib = proj.ocamllib in
  let paths = List.map begin fun inc ->
    if inc.[0] = '+' then (Filename.concat ocamllib (String.sub inc 1 (String.length inc - 1)))
    else inc
  end includes in
  (*List.filter ((<>) ocamllib)*) (ocamllib :: (proj.root // src) :: paths)

(** [load_path proj where] adds to [where] the {i load path} of [proj].*)
let load_path proj where = where := !where @ (get_load_path proj)

(** [unload_path proj where] removes from [where] the {i load path} of [proj].*)
let unload_path proj where =
  let path = get_load_path proj in
  where := List.filter (fun w -> not (List.mem w path)) !where

(** save. Creates [home], [home/src], [home/bak], [home/tmp] if not existing. *)
let save ?editor proj =
  let active_filename =
    match editor with None -> ""
      | Some editor ->
        proj.files <- List.map begin fun (file, offset) ->
          file,
          match editor#get_page (`FILENAME file#path) with
            | None -> 0
            | Some page ->
              if page#load_complete then (page#buffer#get_iter `INSERT)#offset
              else page#initial_offset
        end proj.files;
        let active_filename =
          match editor#get_page `ACTIVE with None -> "" | Some page -> page#get_filename
        in active_filename
  in
  let filename = filename proj in
  try
    if not (Sys.file_exists proj.root) then (Unix.mkdir proj.root 0o777);
    if not (Sys.file_exists (proj.root // src)) then (Unix.mkdir (proj.root // src) 0o777);
    if not (Sys.file_exists (proj.root // bak)) then (Unix.mkdir (proj.root // bak) 0o777);
    if not (Sys.file_exists (proj.root // tmp)) then (Unix.mkdir (proj.root // tmp) 0o777);
    proj.modified <- false;
    unload_path proj Config.load_path;
    proj.open_files <- List.rev_map begin fun (file, offset) ->
      let active = active_filename = file#path in
      begin
        match proj.in_source_path file#path with
          | None ->
            if Filename.is_implicit file#path then (filename_unix_implicit file#path) else file#path
          | Some rel -> filename_unix_implicit rel
      end, offset, active
    end proj.files;
    let root = proj.root in
    let files = proj.files in
    proj.root <- "";
    proj.files <- [];
    (* output XML *)
    let xml = Xml.to_string_fmt (to_xml proj) in
    let xml = "<!-- OCamlEditor XML Project -->\n" ^ xml in
    let outchan = kprintf open_out_bin "%s%s" (Filename.chop_extension filename) extension in
    lazy (output_string outchan xml) /*finally*/ lazy (close_out outchan);
    (* restore non persistent values *)
    proj.root <- root;
    proj.files <- files;
    load_path proj Config.load_path;
  with Unix.Unix_error (err, _, _) -> print_endline (Unix.error_message err)


(** load *)
let load filename =
  let proj = if filename ^^ ".mlp" then begin
    (* Check for old version project file *)
    let old_project_filename = sprintf "%s%s"
      (Filename.chop_extension filename) Project_old_1.project_name_extension in
    let old = Project_old_1.load old_project_filename in
    let proj = create ~filename () in
    proj.modified <- old.Project_old_1.modified;
    proj.author <- old.Project_old_1.author;
    proj.description <- old.Project_old_1.description;
    proj.version <- old.Project_old_1.version;
    proj.files <- List.map (fun x -> x, 0) old.Project_old_1.files;
    proj.open_files <- List.map (fun x -> x, 0, false) old.Project_old_1.loadfiles;
    (* Check for old version targets file *)
    let old_filename = proj.root // Bconf_old_1.filename in
    if Sys.file_exists old_filename then begin
      proj.build <- Bconf.convert_from_1 old_filename;
      Sys.rename old_filename (old_filename^".bak");
    end;
    Sys.rename old_project_filename (old_project_filename^".bak");
    save proj;
    proj;
  end else begin
    from_file filename;
  end in
  proj.root <- Filename.dirname filename;
  (*  *)
  if not (Sys.file_exists (proj.root // tmp)) then (Unix.mkdir (proj.root // tmp) 0o777);
  (*  *)
  proj.open_files <- List.map begin fun (filename, offset, active) ->
    (if Filename.is_implicit filename then proj.root // src // filename else filename), offset, active
  end proj.open_files;
  proj

(** backup_file *)
let backup_file project (file : File.file) =
  let src = (project.root // src) in
  if starts_with src file#path then begin
    let rel = match Miscellanea.filename_relative src file#path with
      | None -> assert false | Some x -> Filename.dirname x in
    let move_to = project.root // bak // rel in
    ignore (file#backup ~move_to ())
  end else print_endline ("Cannot create backup copy of \""^(Filename.quote file#path)^"\".")

(** rename_file. Updates project file on disk. Raise [Cannot_rename "..."] if [file] is read-only or
    the project file is read-only. *)
let rename_file proj file new_name =
  let project_file = File.create (filename proj) () in
  if not project_file#is_writable then (raise (Cannot_rename "Cannot rename: project file is read-only."))
  else if not file#is_writable then (raise (Cannot_rename "Cannot rename: file is read-only."))
  else (file#rename new_name)

(** Adds filename to the open file list. *)
let add_file proj ~offset file =
  if not (List.mem_assoc file proj.files) then begin
    proj.files <- (file, offset) :: proj.files;
  end

(** Removes filename from the open file list. *)
let remove_file proj filename =
  proj.files <- List.filter (fun (f, _) -> f#path <> filename) proj.files

(** Returns the names of the libraries of all build configurations. *)
let get_libraries proj =
  let libs = String.concat " " (List.map (fun t -> t.Bconf.libs) proj.build) in
  Miscellanea.Xlist.remove_dupl (Miscellanea.split "[ \r\n\t]+" libs)

(** clean_tmp *)
let clean_tmp proj =
  let path = path_tmp proj in
  Array.iter begin fun filename ->
    try Sys.remove (path // filename)
    with _ -> ()
  end (Sys.readdir path)

(** default_build_config *)
let default_build_config project =
  try Some (List.find (fun x -> x.Bconf.default) project.build) with Not_found -> None

(** refresh *)
let refresh proj =
  let np = from_file (filename proj) in
  proj.root <- np.root;
  proj.encoding <- np.encoding;
  proj.author <- np.author;
  proj.description <- np.description;
  proj.version <- np.version;
  proj.in_source_path <- Miscellanea.filename_relative (np.root // src);
  proj.source_paths <- (try File.readtree (np.root // src) with Sys_error _ -> [])

(** clear_cache *)
let clear_cache proj =
  let rmr = if Sys.os_type = "Win32" then "DEL /F /Q /S" else "rm -fr" in
  let dir = path_cache proj in
  let files = Sys.readdir dir in
  Array.iter begin fun x ->
    let filename = dir // x in
    if Sys.file_exists filename then begin
      Sys.remove filename;
      printf "File removed %s...\n%!" filename;
    end
  end files;
  let cmd = sprintf "%s \"%s\"\\*" rmr (path_tmp proj) in
  Printf.printf "%s\n%!" cmd;
  Sys.command cmd;;


