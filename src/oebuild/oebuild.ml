(*

  OCamlEditor
  Copyright (C) 2010, 2011 Francesco Tovagliari

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

open Printf
open Oebuild_util
open Miscellanea

type compilation_type = Bytecode | Native | Unspecified
type build_exit = Built_successfully | Build_failed of int
type cache = (string, float(* * bool*)) Hashtbl.t
type process_err_func = (stderr:in_channel -> unit)

let ocamlc = Ocaml_config.ocamlc()
let ocamlopt = Ocaml_config.ocamlopt()
let ocamllib = Ocaml_config.ocamllib()

let oebuild_times_filename = ".oebuild"

(** Hash table: <filename, last-compiled> *)
let read_cache () =
  if not (Sys.file_exists oebuild_times_filename) then begin
    let ochan = open_out_bin oebuild_times_filename in
    Marshal.to_channel ochan (Hashtbl.create 7) [];
    close_out ochan
  end;
  let ichan = open_in_bin oebuild_times_filename in
  let times = Marshal.from_channel ichan in
  close_in ichan;
  (times : cache)

(** write_cache *)
let write_cache (times : cache) =
  if Hashtbl.length times > 0 then begin
    let ochan = open_out_bin oebuild_times_filename in
    Marshal.to_channel ochan times [];
    close_out ochan
  end

(** update_cache *)
let update_cache =
  let get_last_compiled_time ~opt cache filename =
    try
      let time(*, c_opt*) = Hashtbl.find cache filename in
      (*if opt <> c_opt then (Hashtbl.remove cache filename; raise Not_found);*)
      let ext = if opt then "cmx" else "cmo" in
      let cm = sprintf "%s.%s" (Filename.chop_extension filename) ext in
      if Sys.file_exists cm then time
      else begin
        Hashtbl.remove cache filename;
        raise Not_found
      end
    with Not_found -> 0.0
  in
  fun ~deps ~opt (cache : cache) filename ->
    let ctime = get_last_compiled_time ~opt cache filename in
    (*let mtime = ((Unix.stat filename).Unix.st_mtime) in
    if filename = "test.ml" then (eprintf "oebuild: %s %f %f\n%!" filename mtime ctime);*)
    if ctime > 0.0 && (*mtime*) ((Unix.stat filename).Unix.st_mtime) >= ctime then begin
      Hashtbl.remove cache filename;
    end

(** compile *)
let compile ?(times : cache option) ~opt ~compiler ~cflags ~includes ~filename
    ?(process_err : process_err_func option) () =
  if Sys.file_exists filename then begin
    try
      begin
        match times with
          | Some times -> ignore (Hashtbl.find times filename); 0 (* exit code *)
          | _ -> raise Not_found
      end
    with Not_found -> begin
      let cmd = (sprintf "%s -c %s %s %s" compiler cflags includes filename) in
      let exit_code = match process_err with
        | None -> command cmd
        | Some process_err -> exec ~process_err cmd
      in
      may times (fun times -> Hashtbl.add times filename ((Unix.gettimeofday())(*, opt*)));
      exit_code
    end
  end else 0

(** link *)
let link ~compiler ~a ~lflags ~includes ~libs ~outname ~deps
    ?(process_err : process_err_func option) () =
  let opt = compiler = ocamlopt in
  let libs =
    if opt && a then "" else
      let ext = if opt then "cmxa" else "cma" in
      let libs = List.map begin fun x ->
        if Filename.check_suffix x ".o" then begin
          let x = Filename.chop_extension x in
          let ext = if opt then "cmx" else "cmo" in
          sprintf "%s.%s" x ext
        end else if Filename.check_suffix x ".obj" then begin
          sprintf "%s" x
        end else (sprintf "%s.%s" x ext)
      end libs in
      String.concat " " libs
  in
  let deps = String.concat " " deps in
  kprintf (exec (*command*) ?process_err) "%s %s %s -o %s %s %s %s"
    compiler
    (if a then "-a" else "")
    lflags
    outname
    includes
    libs
    deps

(** get_output_name *)
let get_output_name ~compilation ~is_library ~outname ~targets =
  let o_ext =
    if is_library then begin
      if compilation = Native then ".cmxa" else ".cma"
    end else begin
      if compilation = Native then ".opt" ^ (win32 ".exe" "") else (win32 ".exe" "")
    end
  in
  let name =
    if outname = "" then begin
      try
        let last = List.hd (List.rev targets) in
        Filename.chop_extension last
      with Failure "hd" -> assert false
    end else outname
  in
  name ^ o_ext

(** install_output *)
let install_output ~compilation ~is_library ~outname ~deps ~path =
  if is_library then begin
    let path =
      let path = ocamllib // path in
      mkdir_p path;
      path
    in
    cp outname (path // outname);
    let cmis = List.map (fun d -> sprintf "%s.cmi" (Filename.chop_extension d)) deps in
    let mlis = List.map (fun cmi -> sprintf "%s.mli" (Filename.chop_extension cmi)) cmis in
    let mlis = List.filter Sys.file_exists mlis in
    List.iter (fun x -> ignore (cp x (path // x))) cmis;
    List.iter (fun x -> ignore (cp x (path // x))) mlis;
    if compilation = Native then begin
      let ext = win32 "lib" "a" in
      let basename = sprintf "%s.%s" (Filename.chop_extension outname) ext in
      cp basename (path // basename);
    end;
  end else begin
    mkdir_p path;
    cp outname (path // outname);
  end

(** run_output *)
let run_output ~outname ~args =
  let args = List.rev args in
  if is_win32 then begin
    let cmd = Str.global_replace (Str.regexp "/") "\\\\" outname in
    let args = String.concat " " args in
    ignore (kprintf command "%s %s" cmd args)
  end else begin
    let cmd = Filename.current_dir_name // outname in
    (* From the execv manpage:
      "The first argument, by convention, should point to the filename associated
       with the file being executed." *)
    let args = cmd :: args in
    let args = Array.of_list args in
    Unix.execv cmd args
  end

(** sort_dependencies *)
let sort_dependencies ~deps subset =
  let result = ref [] in
  List.iter begin fun x ->
    if List.mem x subset then (result := x :: !result)
  end deps;
  List.rev !result

(** filter_inconsistent_assumptions_error *)
let filter_inconsistent_assumptions_error ~compiler_output ~recompile ~deps ~(cache : cache) =
  let re_inconsistent_assumptions = Str.regexp
    ".*make[ \t\r\n]+inconsistent[ \t\r\n]+assumptions[ \t\r\n]+over[ \t\r\n]+\\(interface\\|implementation\\)[ \t\r\n]+\\([^ \t\r\n]+\\)[ \t\r\n]+"
  in
  let re_error = Str.regexp "Error: " in
  ((fun ~stderr ->
    let line = input_line stderr in
    Buffer.add_string compiler_output (line ^ "\n");
    let messages = Buffer.contents compiler_output in
    let len = String.length messages in
    try
      let pos = Str.search_backward re_error messages len in
      let last_error = String.sub messages pos (len - pos) in
      begin
        try
          let _ = Str.search_backward re_inconsistent_assumptions last_error (String.length last_error) in
          let modname = Str.matched_group 2 last_error in
          let dependants = Dep.find_dependants ~modname in
          let dependants = sort_dependencies ~deps dependants in
          let original_error = Buffer.contents compiler_output in
          eprintf "Warning (oebuild): the following files make inconsistent assumptions over interface/implementation %s: %s\n%!"
            modname (String.concat ", " dependants);
          List.iter begin fun filename ->
            Hashtbl.remove cache filename;
            let basename = Filename.chop_extension filename in
            let cmi = basename ^ ".cmi" in
            if Sys.file_exists cmi then (Sys.remove cmi);
            let cmo = basename ^ ".cmo" in
            if Sys.file_exists cmo then (Sys.remove cmo);
            let cmx = basename ^ ".cmx" in
            if Sys.file_exists cmx then (Sys.remove cmx);
          end dependants;
          recompile := dependants;
        with Not_found -> ()
      end
    with Not_found -> ()) : process_err_func)

(** Building *)
let build ~compilation ~includes ~libs ~other_mods ~is_library ~compile_only
    ~thread ~vmthread ~annot ~pp ~cflags ~lflags ~outname ~deps ~ms_paths ?(prof=false) () =
  (* includes *)
  let includes = ref includes in
  includes := Ocaml_config.expand_includes !includes;
  (* libs *)
  let libs = Miscellanea.split " +" libs in
  (* flags *)
  let cflags = ref cflags in
  let lflags = ref lflags in
  (* thread *)
  if thread then (cflags := !cflags ^ " -thread"; lflags := !lflags ^ " -thread");
  if vmthread then (cflags := !cflags ^ " -vmthread"; lflags := !lflags ^ " -vmthread");
  if annot then (cflags := !cflags ^ " -annot");
  if pp <> "" then (cflags := !cflags ^ " -pp " ^ pp);
  (* compiling *)
  let compiler = if prof then "ocamlcp -p a" else if compilation = Native then ocamlopt else ocamlc in
  (*printf "%s%!" (Miscellanea.expand (compiler ^ " -v"));*)
  let mods = Miscellanea.split " +" other_mods in
  let mods = if compilation = Native then List.map (sprintf "%s.cmx") mods else List.map (sprintf "%s.cmo") mods in
  if compilation = Native && !ms_paths then begin
    lflags := !lflags ^
      " -ccopt \"-LC:\\Programmi\\MIC977~1\\Lib -LC:\\Programmi\\MID05A~1\\VC\\lib -LC:\\GTK\\lib\""
  end;
  let times = read_cache () in
  let build_exit =
    let compilation_exit = ref 0 in
    begin
      try
        let opt = compilation = Native in
        let compiler_output = Buffer.create 100 in
        let rec try_compile filename =
          let recompile = ref [] in
          let compile_exit =
            update_cache ~deps ~opt times filename;
            let exit_code = compile
              ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~deps ~cache:times)
              ~times ~opt ~compiler ~cflags:!cflags ~includes:!includes ~filename ()
            in
            if exit_code <> 0 then (Hashtbl.remove times filename);
            exit_code
          in
          if List.length !recompile > 0 then begin
            List.iter begin fun filename ->
              compilation_exit := compile ~times ~opt ~compiler ~cflags:!cflags ~includes:!includes ~filename ();
              if !compilation_exit <> 0 then (raise Exit)
            end !recompile;
            print_newline();
            Buffer.clear compiler_output;
            try_compile filename;
          end else begin
            if Buffer.length compiler_output > 0 then (eprintf "%s\n%!" (Buffer.contents compiler_output));
            compile_exit
          end
        in
        List.iter begin fun filename ->
          compilation_exit := try_compile filename;
          if !compilation_exit <> 0 then (raise Exit)
        end deps;
      with Exit -> ()
    end;
    if !compilation_exit = 0 then begin
      let opt = compilation = Native in
      let obj_deps =
        let ext = if compilation = Native then "cmx" else "cmo" in
        let deps = List.filter (fun x -> x ^^ "ml") deps in
        List.map (fun x -> sprintf "%s.%s" (Filename.chop_extension x) ext) deps
      in
      if compile_only then !compilation_exit else begin
        let obj_deps = mods @ obj_deps in
        let compiler_output = Buffer.create 100 in
        let rec try_link () =
          let recompile = ref [] in
          let link_exit =
            link ~compiler ~a:is_library ~lflags:!lflags
            ~includes:!includes ~libs ~deps:obj_deps ~outname
            ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~deps ~cache:times) ()
          in
          if List.length !recompile > 0 then begin
            List.iter begin fun filename ->
              ignore (compile ~times ~opt ~compiler ~cflags:!cflags ~includes:!includes ~filename ())
            end !recompile;
            Buffer.clear compiler_output;
            try_link()
          end else begin
            eprintf "%s\n%!" (Buffer.contents compiler_output);
            link_exit
          end
        in
        try_link()
      end
    end else !compilation_exit
  in
  write_cache times;
  if build_exit = 0 then Built_successfully else (Build_failed build_exit)

(** clean *)
let clean ?(all=false) ~compilation ~is_library ~outname ~targets ~deps () =
  let outname = get_output_name ~compilation ~is_library ~outname ~targets in
  let files = List.map begin fun name ->
    let name = Filename.chop_extension name in
    (if true || not is_library || all then [name ^ ".cmi"] else []) @
    [ (name ^ ".cmo");
      (name ^ ".cmx");
      (name ^ ".obj");
      (name ^ ".o");
      (name ^ ".annot");
    ]
  end deps in
  let files = List.flatten files in
  let files = if not is_library || all then outname :: files else files in
  let files = remove_dupl files in
  List.iter (fun file -> remove_file ~verbose:true file) files

(** clean_all *)
let suffixes sufs name = List.exists (fun suf -> name ^^ suf) sufs

let clean_all () =
  let cwd = Sys.getcwd() in
  let rec clean_dir dir =
    if not ((Unix.lstat dir).Unix.st_kind = Unix.S_LNK) then begin
      let files = Sys.readdir dir in
      let files = Array.to_list files in
      let files = List.map (fun x -> dir // x) files in
      let directories, files = List.partition Sys.is_directory files in
      let files = List.filter
        (suffixes [".cmi"; ".cmo"; ".cmx"; ".obj"; ".cma"; ".cmxa"; ".lib"; ".a"; ".o"; ".annot"]) files in
      List.iter (remove_file ~verbose:false) files;
      let oebuild_times_filename = dir // oebuild_times_filename in
      remove_file ~verbose:false oebuild_times_filename;
      List.iter clean_dir directories;
    end
  in
  clean_dir cwd;
  exit 0



















