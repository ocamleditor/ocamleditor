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


open Printf
open Oebuild_util

module Table = Oebuild_table

type compilation_type = Bytecode | Native | Unspecified
type output_type = Executable | Library | Plugin | Pack
type build_exit = Built_successfully | Build_failed of int
type process_err_func = (stderr:in_channel -> unit)

let string_of_compilation_type = function
  | Bytecode -> "Bytecode"
  | Native -> "Native"
  | Unspecified -> "Unspecified"

let ocamlc = Ocaml_config.ocamlc()
let ocamlopt = Ocaml_config.ocamlopt()
let ocamllib = Ocaml_config.ocamllib()

(** compile *)
let compile ?(times : Table.t option) ~opt ~compiler ~cflags ~package ~includes ~filename
    ?(process_err : process_err_func option) () =
  if Sys.file_exists filename then begin
    try
      begin
        match times with
          | Some times -> ignore (Table.find times filename opt); 0 (* exit code *)
          | _ -> raise Not_found
      end
    with Not_found -> begin
      let cmd = (sprintf "%s -c %s %s %s" compiler cflags includes filename) in
      let exit_code = match process_err with
        | None -> command cmd
        | Some process_err -> exec ~process_err cmd
      in
      may times (fun times -> Table.add times filename opt (Unix.gettimeofday()));
      exit_code
    end
  end else 0

(** link *)
let link ~compilation ~compiler ~outkind ~lflags ~package ~includes ~libs ~outname ~deps
    ?(process_err : process_err_func option) () =
  let opt = compilation = Native && ocamlopt <> None in
  let libs =
    if (*opt &&*) outkind <> Executable then "" else
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
  let use_findlib = package <> "" in
  let deps = String.concat " " deps in
  kprintf (exec (*command*) ?process_err) "%s%s %s %s -o %s %s %s %s"
    compiler
    (if use_findlib && outkind = Executable then " -linkpkg" else "")
    (match outkind with Library -> "-a" | Plugin -> "-shared" | Pack -> "-pack" | Executable -> "")
    lflags
    outname
    includes
    libs
    deps
;;

(** get_output_name *)
let get_output_name ~compilation ~outkind ~outname ~targets =
  match targets with
    | [] -> None
    | _ -> Some begin
      let o_ext =
        match outkind with
          | Library when compilation = Native -> ".cmxa"
          | Library -> ".cma"
          | Executable when compilation = Native -> ".opt" ^ (win32 ".exe" "")
          | Executable -> win32 ".exe" ""
          | Plugin -> ".cmxs"
          | Pack -> ".cmx"
      in
      let name =
        if outname = "" then begin
          match (List.rev targets) with
            | last :: _ -> Filename.chop_extension last
            | _ -> assert false
        end else outname
      in
      name ^ o_ext
    end
;;

(** install_output *)
let install_output ~compilation ~outkind ~outname ~deps ~path ~ccomp_type =
  let dest_outname = Filename.basename outname in
  match outkind with
    | Library ->
      let path =
        let path = ocamllib // path in
        mkdir_p path;
        path
      in
      cp outname (path // dest_outname);
      let deps_mod = List.map Filename.chop_extension deps in
      let deps_mod = remove_dupl deps_mod in
      let cmis = List.map (fun d -> sprintf "%s.cmi" d) deps_mod in
      let mlis = List.map (fun cmi -> sprintf "%s.mli" (Filename.chop_extension cmi)) cmis in
      let mlis = List.filter Sys.file_exists mlis in
      List.iter (fun x -> ignore (cp x (path // (Filename.basename x)))) cmis;
      List.iter (fun x -> ignore (cp x (path // (Filename.basename x)))) mlis;
      if compilation = Native then begin
        let ext = match ccomp_type with Some "msvc" -> ".lib" | Some _ ->  ".a" | None -> assert false in
        let basename = sprintf "%s%s" (Filename.chop_extension outname) ext in
        cp basename (path // (Filename.basename basename));
      end;
    | Executable ->
      mkdir_p path;
      cp outname (path // dest_outname)
    | Plugin | Pack -> eprintf "\"install_output\" not implemented for Plugin or Pack."
;;

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
;;

(** sort_dependencies *)
let sort_dependencies ~deps subset =
  let result = ref [] in
  List.iter begin fun x ->
    if List.mem x subset then (result := x :: !result)
  end deps;
  List.rev !result
;;

(** filter_inconsistent_assumptions_error *)
let filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~(cache : Table.t) ~opt =
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
          let dependants = Dep.find_dependants ~targets ~modname in
          let dependants = sort_dependencies ~deps dependants in
          let _ (*original_error*) = Buffer.contents compiler_output in
          eprintf "Warning (oebuild): the following files make inconsistent assumptions over interface/implementation %s: %s\n%!"
            modname (String.concat ", " dependants);
          List.iter begin fun filename ->
            Table.remove cache filename opt;
            let basename = Filename.chop_extension filename in
            let cmi = basename ^ ".cmi" in
            if Sys.file_exists cmi then (Sys.remove cmi);
            let cmo = basename ^ ".cmo" in
            if Sys.file_exists cmo then (Sys.remove cmo);
            let cmx = basename ^ ".cmx" in
            if Sys.file_exists cmx then (Sys.remove cmx);
            let obj = basename ^ (win32 ".obj" ".o") in
            if Sys.file_exists obj then (Sys.remove obj);
          end dependants;
          recompile := dependants;
        with Not_found -> ()
      end
    with Not_found -> ()) : process_err_func)
;;

(** Building *)
let build ~compilation ~package ~includes ~libs ~other_mods ~outkind ~compile_only
    ~thread ~vmthread ~annot ~pp ?inline ~cflags ~lflags ~outname ~deps ~ms_paths
    ~targets ?(prof=false) () =
  let split_space = Str.split (Str.regexp " +") in
  (* includes *)
  let includes = ref includes in
  includes := Ocaml_config.expand_includes !includes;
  (* libs *)
  let libs = split_space libs in
  (* flags *)
  let cflags = ref cflags in
  let lflags = ref lflags in
  (* thread *)
  if thread then (cflags := !cflags ^ " -thread"; lflags := !lflags ^ " -thread");
  if vmthread then (cflags := !cflags ^ " -vmthread"; lflags := !lflags ^ " -vmthread");
  if annot then (cflags := !cflags ^ " -annot");
  if pp <> "" then (cflags := !cflags ^ " -pp " ^ pp);
  begin
    match inline with
      | Some inline when compilation = Native ->
        let inline = string_of_int inline in
        cflags := !cflags ^ " -inline " ^ inline;
        lflags := !lflags ^ " -inline " ^ inline;
      | _ -> ()
  end;
  (* compiling *)
  let compiler =
    if prof then "ocamlcp -p a"
    else begin
      let compiler =
        if compilation = Native then
          (match ocamlopt with Some x -> x | _ -> failwith "ocamlopt was not found")
        else ocamlc
      in
      let use_findlib = package <> "" in
      if use_findlib then
        let cmp = try Filename.chop_extension compiler with Invalid_argument "Filename.chop_extension" -> compiler in
        sprintf "ocamlfind %s -package %s" cmp package
      else compiler
    end
  in
  (*printf "%s%!" (Cmd.expand (compiler ^ " -v"));*)
  let mods = split_space other_mods in
  let mods = if compilation = Native then List.map (sprintf "%s.cmx") mods else List.map (sprintf "%s.cmo") mods in
  if compilation = Native && !ms_paths then begin
    lflags := !lflags ^
      " -ccopt \"-LC:\\Programmi\\MIC977~1\\Lib -LC:\\Programmi\\MID05A~1\\VC\\lib -LC:\\GTK\\lib\""
  end;
  let times = Table.read () in
  let build_exit =
    let compilation_exit = ref 0 in
    (** Compile *)
    begin
      try
        let opt = compilation = Native in
        let compiler_output = Buffer.create 100 in
        let rec try_compile filename =
          let recompile = ref [] in
          let compile_exit =
            Table.update ~deps ~opt times filename;
            let exit_code = compile
              ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~cache:times ~opt)
              ~times ~opt ~compiler ~cflags:!cflags ~package ~includes:!includes ~filename ()
            in
            if exit_code <> 0 then (Table.remove times filename opt);
            exit_code
          in
          if List.length !recompile > 0 then begin
            List.iter begin fun filename ->
              compilation_exit := compile ~times ~opt ~compiler ~cflags:!cflags ~package ~includes:!includes ~filename ();
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
    (** Link *)
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
            link ~compilation ~compiler ~outkind ~lflags:!lflags
              ~package
              ~includes:!includes ~libs ~deps:obj_deps ~outname
              ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~cache:times ~opt) ()
          in
          if List.length !recompile > 0 then begin
            List.iter begin fun filename ->
              ignore (compile ~times ~opt ~compiler ~cflags:!cflags ~package ~includes:!includes ~filename ())
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
  Table.write times;
  if build_exit = 0 then Built_successfully else (Build_failed build_exit)
;;

(** clean *)
let clean ?(all=false) ~compilation ~outkind ~outname ~targets ~deps () =
  let outname = get_output_name ~compilation ~outkind ~outname ~targets in
  let files = List.map begin fun name ->
    let name = Filename.chop_extension name in
    [name ^ ".cmi"] @
    (if true || outkind <> Library || all then [name ^ ".cmi"] else []) @
    [ (name ^ ".cmo");
      (name ^ ".cmx");
      (name ^ ".obj");
      (name ^ ".o");
      (name ^ ".annot");
    ]
  end deps in
  let files = List.flatten files in
  let files = if outkind = Executable || all then (match outname with Some x -> x :: files | _ -> files) else files in
  let files = remove_dupl files in
  List.iter (fun file -> remove_file ~verbose:true file) files
;;

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
      let oebuild_times_filename = dir // Table.oebuild_times_filename in
      remove_file ~verbose:false oebuild_times_filename;
      List.iter clean_dir directories;
    end
  in
  clean_dir cwd;
  exit 0
;;

(** check_restrictions *)
let check_restrictions restr =
  List.for_all begin function
    | "IS_UNIX" -> Sys.os_type = "Unix"
    | "IS_WIN32" -> Sys.os_type = "Win32"
    | "IS_CYGWIN" -> Sys.os_type = "Cygwin"
    | "HAS_NATIVE" -> Ocaml_config.can_compile_native () <> None (* should be cached *)
    | _ -> false
  end restr;;
