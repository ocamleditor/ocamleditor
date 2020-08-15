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
open Printf
open Prj

module Log = Common.Log.Make(struct let prefix = "Project" end)

exception Project_already_exists of string

exception Cannot_rename of string

let write_xml = ref (fun _ -> failwith "write_xml")
let read_xml = ref (fun _ -> failwith "read_xml")
let from_local_xml : (t -> unit) ref = ref (fun _ -> failwith "from_local_xml")

let path_src p = p.root // default_dir_src
let path_bak p = p.root // default_dir_bak
let path_tmp p = p.root // default_dir_tmp
let path_cache p = p.root // ".cache"
let path_dot_oebuild p = p.root // default_dir_src // ".oebuild"


(** abs_of_tmp *)
let abs_of_tmp proj filename =
  match Miscellanea.filename_relative (".." // default_dir_tmp) filename with
    | None -> filename
    | Some relname -> (path_src proj) // relname

(** tmp_of_abs *)
let tmp_of_abs proj filename =
  let tmp = path_tmp proj in
  match proj.Prj.in_source_path filename with
    | None -> None
    | Some rel_name -> Some (tmp, rel_name) ;;

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
  project.autocomp_compiler <- Ocaml_config.ocamlc();
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
    targets            = [];
    executables        = [];
    autocomp_enabled   = true;
    autocomp_delay     = 1.0;
    autocomp_cflags    = "";
    autocomp_dflags    = [|"-c"; "-w"; "+a-48"; "-thread"; "-bin-annot"|];
    autocomp_compiler  = "";
    search_path        = [];
    in_source_path     = Miscellanea.filename_relative (root // default_dir_src);
    source_paths       = (try File_util.readtree (root // default_dir_src) with Sys_error _ -> []);
    can_compile_native = true;
    symbols            = {
      Oe.syt_table = [];
      syt_ts       = Hashtbl.create 7;
      syt_odoc     = Hashtbl.create 7;
      syt_critical = Mutex.create()
    };
    build_script       = {
      Build_script.bs_filename = Build_script.default_filename;
      bs_targets               = [];
      bs_args                  = [];
      bs_commands              = [];
    };
    bookmarks          = [];
  } in
  proj;;

(** set_runtime_build_task *)
let set_runtime_build_task proj rconf task_string =
  rconf.Rconf.build_task <- try
    let target = List.find (fun b -> b.Target.id = rconf.Rconf.target_id) proj.targets in
    Target.task_of_string target task_string
  with Not_found -> `NONE

(** to_xml *)

(** from_file *)

(** convert_to_utf8 *)
let convert_to_utf8 proj text = match proj.encoding with
  | None -> Convert.to_utf8 text
  | Some from_codeset -> Glib.Convert.convert ~from_codeset ~to_codeset:"UTF-8" text

(** convert_from_utf8 *)
let convert_from_utf8 proj text = match proj.encoding with
  | None -> Convert.from_utf8 text
  | Some to_codeset -> Glib.Convert.convert ~from_codeset:"UTF-8" ~to_codeset text

(** Returns the full filename of the project configuration file. *)
let filename proj = Filename.concat proj.root (proj.name ^ default_extension)
let mk_old_filename filename = (Filename.chop_extension filename) ^ old_extension

(** Returns the full filename of the project local configuration file. *)
let filename_local proj = (filename proj) ^ ".local"
let mk_old_filename_local proj = (Filename.chop_extension (filename proj)) ^ ".local" ^ old_extension

(** get_includes*)
let get_includes =
  let re = Str.regexp "[ \t\r\n]+" in
  fun proj ->
    let includes = String.concat " " (List.map (fun t -> t.Target.includes) proj.targets) in
    let includes = Str.split re includes in
    Xlist.remove_dupl includes

(** get_search_path *)
let get_search_path proj =
  let package = String.concat "," (List.map (fun t -> t.Target.package) proj.targets) in
  let package = Str.split (Miscellanea.regexp ",") package in
  let package = List.filter ((<>) "") package in
  let package = List.flatten (List.map begin fun package ->
    kprintf Shell.get_command_output "ocamlfind query %s -r %s" package Shell.redirect_stderr
  end (Xlist.remove_dupl package)) in
  let package = Xlist.remove_dupl (List.filter ((<>) "") package) in
  let includes = Xlist.remove_dupl (get_includes proj) in
  package @ includes;;

(** get_search_path_i_format *)
let get_search_path_i_format proj =
  if proj.search_path = [] then "" else "-I " ^ (String.concat " -I " proj.search_path);;

(** get_search_path_local *)
let get_search_path_local proj =
  let paths = proj.search_path in
  (proj.root // default_dir_src) ::
  List.filter_map begin fun path ->
    if path.[0] = '+' then None
    else if not (Filename.is_relative path) then None
    else Some (proj.root // default_dir_src // path)
  end paths;;

(** Returns the {i load path} of the project: includes and [src]. *)
let get_load_path proj =
  let includes = proj.search_path in
  let includes = "+threads" :: includes in
  let ocamllib = proj.ocamllib in
  let paths = List.map begin fun inc ->
    if inc.[0] = '+' then (Filename.concat ocamllib (String.sub inc 1 (String.length inc - 1)))
    else inc
  end includes in
  ocamllib :: (List.filter ((<>) ocamllib) ((proj.root // default_dir_src) :: paths))

(** [load_path proj] adds to module [Load_path] the {i load path} of [proj].*)
let load_path proj = List.iter Load_path.add_dir (get_load_path proj)

(** [unload_path proj] removes from [Load_path] the {i load path} of [proj].*)
let unload_path proj = List.iter Load_path.remove_dir (get_load_path proj)

(** output_xml *)
let output_xml filename xml =
  let xml = (sprintf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!-- %s-%s XML Project -->\n" About.program_name About.version) ^ xml in
  let outchan = open_out_bin filename in
  lazy (output_string outchan xml) /*finally*/ lazy (close_out outchan);;

let xml_of_open_files proj =
  Xml.Element ("open_files", [],
    (List.map (fun (x, scroll_off, off, active) -> Xml.Element ("filename", [
      "scroll", string_of_int scroll_off;
      "cursor", string_of_int off;
      "active", (string_of_bool active)
    ], [Xml.PCData x])) proj.open_files));;

let xml_of_bookmarks proj =
  Xml.Element ("bookmarks", [],
    List.map begin fun bm ->
      Xml.Element ("bookmark", [
        "num", string_of_int bm.Oe.bm_num;
        "offset", string_of_int (Bookmark.apply bm begin function
          | `ITER iter -> GtkText.Iter.get_offset iter
          | `OFFSET offset -> offset
        end)
      ], [Xml.PCData bm.Oe.bm_filename])
    end proj.bookmarks);;

let xml_of_local childs = Xml.Element ("local", [], childs);;

let save_local filename proj =
  let xml = Xml.to_string_fmt (xml_of_local [(xml_of_open_files proj); (xml_of_bookmarks proj)]) in
  output_xml filename xml;;

(** save. Creates [home], [home/src], [home/bak], [home/tmp] if not existing. *)
let save ?editor proj =
  let active_filename =
    match editor with None -> ""
      | Some editor ->
        proj.files <- List.map begin fun (file, (_scroll_offset, _offset)) ->
          file,
          match editor#get_page (`FILENAME file#filename) with
            | None -> 0, 0
            | Some page ->
              let scroll_top = page#view#get_scroll_top () in
              if page#load_complete then scroll_top, (page#buffer#get_iter `INSERT)#offset
              else page#scroll_offset, page#initial_offset
        end proj.files;
        let active_filename =
          match editor#get_page `ACTIVE with None -> "" | Some page -> page#get_filename
        in active_filename
  in
  let filename = filename proj in
  let filename_local = filename_local proj in
  try
    if not (Sys.file_exists proj.root) then (Unix.mkdir proj.root 0o777);
    if not (Sys.file_exists (proj.root // default_dir_src)) then (Unix.mkdir (proj.root // default_dir_src) 0o777);
    if not (Sys.file_exists (proj.root // default_dir_bak)) then (Unix.mkdir (proj.root // default_dir_bak) 0o777);
    if not (Sys.file_exists (proj.root // default_dir_tmp)) then (Unix.mkdir (proj.root // default_dir_tmp) 0o777);
    proj.modified <- false;
    unload_path proj;
    proj.open_files <- List.rev_map begin fun (file, (scroll_offset, offset)) ->
      let active = active_filename = file#filename in
      begin
        match proj.in_source_path file#filename with
          | None ->
            if Filename.is_implicit file#filename then (filename_unix_implicit file#filename) else file#filename
          | Some rel -> filename_unix_implicit rel
      end, scroll_offset, offset, active
    end proj.files;
    (* output tools *)
    Project_tools.write proj;
    (*  *)
    let root = proj.root in
    let files = proj.files in
    proj.root <- "";
    proj.files <- [];
    (* output XML *)
    let xml = Xml.to_string_fmt (!write_xml proj) in
    output_xml filename xml;
    save_local filename_local proj;
    (* restore non persistent values *)
    proj.root <- root;
    proj.files <- files;
    load_path proj;
  with Unix.Unix_error (err, _, _) -> print_endline (Unix.error_message err);;

(** save_bookmarks *)
let save_bookmarks proj =
  let filename = filename_local proj in
  let xml =
    if Sys.file_exists filename then begin
      let parser = XmlParser.make () in
      let xml = XmlParser.parse parser (XmlParser.SFile filename) in
      let xml = Xml.map begin fun node ->
        match Xml.tag node with
          | "bookmarks" -> xml_of_bookmarks proj
          | _ -> node
      end xml in
      xml_of_local xml;
    end else (xml_of_local [xml_of_bookmarks proj])
  in
  output_xml filename (Xml.to_string_fmt xml);;

(** remove_bookmark *)
let remove_bookmark num proj =
  proj.bookmarks <- List.filter (fun x -> x.Oe.bm_num <> num) proj.bookmarks;;

(** set_bookmark *)
let set_bookmark bookmark proj =
  begin
    match List_opt.find (fun x -> x.Oe.bm_num = bookmark.Oe.bm_num) proj.bookmarks with
      | Some bookmark ->
        Bookmark.remove bookmark;
        remove_bookmark bookmark.Oe.bm_num proj;
      | _ -> ()
  end;
  proj.bookmarks <- bookmark :: proj.bookmarks;
  save_bookmarks proj;;

(** find_bookmark *)
let find_bookmark proj filename buffer iter =
  List_opt.find begin fun bm ->
    if bm.Oe.bm_filename = filename then begin
      let mark = Bookmark.offset_to_mark buffer bm in
      iter#line = (buffer#get_iter (`MARK mark))#line
    end else false
  end proj.bookmarks;;

(** get_actual_maximum_bookmark *)
let get_actual_maximum_bookmark project =
  List.fold_left (fun acc bm -> max acc bm.Oe.bm_num) 0 project.bookmarks;;

(** load *)
let load filename =
  let filename = if Sys.file_exists filename then filename else mk_old_filename filename in
  let proj = !read_xml filename in
  !from_local_xml proj;
  (*  *)
  proj.root <- Filename.dirname filename;
  (*  *)
  if not (Sys.file_exists (proj.root // default_dir_tmp)) then (Unix.mkdir (proj.root // default_dir_tmp) 0o777);
  (*  *)
  proj.open_files <- List.map begin fun (filename, scroll_offset, offset, active) ->
    (if Filename.is_implicit filename then proj.root // default_dir_src // filename else filename), scroll_offset, offset, active
  end proj.open_files;
  (*  *)
  proj.search_path <- get_search_path proj;
  (* Remove old version filenames *)
  let old = mk_old_filename filename in
  if Sys.file_exists old then (Sys.remove old);
  let old = mk_old_filename_local proj in
  if Sys.file_exists old then (Sys.remove old);
  (*  *)
  proj;;

(** backup_file *)
let backup_file project (file : Editor_file.file) =
  let src = (project.root // default_dir_src) in
  if starts_with src file#filename then begin
    let rel = match Miscellanea.filename_relative src file#filename with
      | None -> assert false | Some x -> Filename.dirname x in
    let move_to = project.root // default_dir_bak // rel in
    ignore (file#backup ~move_to ())
  end else print_endline ("Cannot create backup copy of \""^(Filename.quote file#filename)^"\".")

(** rename_file. Updates project file on disk. Raise [Cannot_rename "..."] if [file] is read-only or
    the project file is read-only. *)
let rename_file proj file new_name =
  let is_writeable = File_util.is_writeable (filename proj) in
  if not is_writeable then (raise (Cannot_rename "Cannot rename: project file is read-only."))
  else if not is_writeable then (raise (Cannot_rename "Cannot rename: file is read-only."))
  else (file#rename new_name)

(** Adds filename to the open file list. *)
let add_file proj ~scroll_offset ~offset file =
  if not (List.mem_assoc file proj.files) then begin
    proj.files <- (file, (scroll_offset, offset)) :: proj.files;
  end

(** Removes filename from the open file list. *)
let remove_file proj filename =
  proj.files <- List.filter (fun (f, _) -> f#filename <> filename) proj.files

(** Returns the names of the libraries of all targets. *)
let get_libraries proj =
  let libs = String.concat " " (List.map (fun t -> t.Target.libs) proj.targets) in
  Miscellanea.Xlist.remove_dupl (Miscellanea.split "[ \r\n\t]+" libs)

(** clean_tmp *)
let clean_tmp proj =
  let path = path_tmp proj in
  Array.iter begin fun filename ->
    try Sys.remove (path // filename)
    with _ -> ()
  end (Sys.readdir path)

(** default_target *)
let default_target project =
  try Some (List.find (fun x -> x.Target.default) project.targets) with Not_found -> None

(** refresh *)
let refresh proj =
  let np = !read_xml (filename proj) in
  proj.root <- np.root;
  proj.encoding <- np.encoding;
  proj.author <- np.author;
  proj.description <- np.description;
  proj.version <- np.version;
  proj.in_source_path <- Miscellanea.filename_relative (np.root // default_dir_src);
  proj.source_paths <- (try File_util.readtree (np.root // default_dir_src) with Sys_error _ -> [])

(** clear_cache *)
let clear_cache proj =
  (* Remove the compile timestamps cache *)
  let dot_oebuild = path_dot_oebuild proj in
  if Sys.file_exists dot_oebuild then begin
    Sys.remove dot_oebuild;
    printf "File removed %s...\n%!" dot_oebuild;
  end;
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
  Log.println `TRACE "%s\n%!" cmd;
  Sys.command cmd;;

(*(** load_rc_icons *)
let load_rc_icons proj =
  Miscellanea.pushd (proj.root // Prj.default_dir_src);
  let script = Filename.concat (Filename.concat ".." "tools") "rc_compile.ml" in
  let cmd = sprintf "ocaml %s %S %S" script in
  List.iter begin fun target ->
    match target.Target.resource_file with
      | None -> ()
      | Some rc ->
        List.iter begin fun (iconame, data) ->
          let process_in ic =
            try while true do Buffer.add_channel data ic 1024 done
            with End_of_file -> ()
          in
          Oebuild_util.exec ~verbose:false ~join:false ~process_in (cmd target.Target.name iconame) |> ignore
        end rc.Resource_file.rc_icons_data;
  end proj.targets;
  Miscellanea.popd();*)


