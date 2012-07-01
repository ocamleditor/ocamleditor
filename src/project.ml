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
open Project_type

exception Project_already_exists of string

exception Cannot_rename of string

let write_xml = ref (fun _ -> failwith "write_xml")
let read_xml = ref (fun _ -> failwith "read_xml")
let from_local_xml : (t -> unit) ref = ref (fun _ -> failwith "from_local_xml")

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
    build_script       = {
      Build_script.bs_filename = Build_script.default_filename;
      bs_args                  = [];
    };
    bookmarks          = [];
  } in
  proj;;

(** set_runtime_build_task *)
let set_runtime_build_task proj rconf rbt_string =
  rconf.Rconf.build_task <- try
    let bconf = List.find (fun b -> b.Bconf.id = rconf.Rconf.id_build) proj.build in
    Bconf.rbt_of_string bconf rbt_string
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
let filename proj = Filename.concat proj.root (proj.name ^ extension)

(** Returns the full filename of the project local configuration file. *)
let filename_local proj = (Filename.chop_extension (filename proj)) ^ ".local" ^ extension

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

(** output_xml *)
let output_xml filename xml =
  let xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!-- OCamlEditor XML Project -->\n" ^ xml in
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
        proj.files <- List.map begin fun (file, (scroll_offset, offset)) ->
          file,
          match editor#get_page (`FILENAME file#path) with
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
    if not (Sys.file_exists (proj.root // src)) then (Unix.mkdir (proj.root // src) 0o777);
    if not (Sys.file_exists (proj.root // bak)) then (Unix.mkdir (proj.root // bak) 0o777);
    if not (Sys.file_exists (proj.root // tmp)) then (Unix.mkdir (proj.root // tmp) 0o777);
    proj.modified <- false;
    unload_path proj Config.load_path;
    proj.open_files <- List.rev_map begin fun (file, (scroll_offset, offset)) ->
      let active = active_filename = file#path in
      begin
        match proj.in_source_path file#path with
          | None ->
            if Filename.is_implicit file#path then (filename_unix_implicit file#path) else file#path
          | Some rel -> filename_unix_implicit rel
      end, scroll_offset, offset, active
    end proj.files;
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
    load_path proj Config.load_path;
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

(** load *)
let load filename =
  let proj = !read_xml filename in
  !from_local_xml proj;
  proj.root <- Filename.dirname filename;
  (*  *)
  if not (Sys.file_exists (proj.root // tmp)) then (Unix.mkdir (proj.root // tmp) 0o777);
  (*  *)
  proj.open_files <- List.map begin fun (filename, scroll_offset, offset, active) ->
    (if Filename.is_implicit filename then proj.root // src // filename else filename), scroll_offset, offset, active
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
let add_file proj ~scroll_offset ~offset file =
  if not (List.mem_assoc file proj.files) then begin
    proj.files <- (file, (scroll_offset, offset)) :: proj.files;
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
  let np = !read_xml (filename proj) in
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


