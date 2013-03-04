(*

  OCamlEditor
  Copyright (C) 2010-2013 Francesco Tovagliari

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
type output_type = Executable | Library | Plugin | Pack | External
type build_exit = Built_successfully | Build_failed of int
type process_err_func = (stderr:in_channel -> unit)

let string_of_compilation_type = function
  | Bytecode -> "Bytecode"
  | Native -> "Native"
  | Unspecified -> "Unspecified"

let string_of_output_type = function
  | Executable -> "Executable"
  | Library -> "Library"
  | Plugin -> "Plugin"
  | Pack -> "Pack"
  | External -> "External";;

let ocamlc = Ocaml_config.ocamlc()
let ocamlopt = Ocaml_config.ocamlopt()
let ocamllib = Ocaml_config.ocamllib()

(** check_package_list *)
let check_package_list =
  let redirect_stderr = if Sys.os_type = "Win32" then " 1>NUL 2>NUL" else " 2>/dev/null" in
  fun package_list ->
    let package_list = Str.split (Str.regexp "[, ]") package_list in
    let available, unavailable =
      List.partition begin fun package ->
        kprintf (Oebuild_util.command ~echo:false) "ocamlfind query %s %s" package redirect_stderr = 0
      end package_list
    in
    if unavailable <> [] then begin
      eprintf "Warning (oebuild): the following packages are not found: %s\n"
        (String.concat ", " unavailable);
    end;
    String.concat "," available;;

(** compile *)
let compile ?(times : Table.t option) ~opt ~compiler ~cflags ~package ~includes ~filename
    ?(process_err : process_err_func option) ~verbose () =
  if Sys.file_exists filename then begin
    try
      begin
        match times with
          | Some times -> ignore (Table.find times filename opt); 0 (* exit code *)
          | _ -> raise Not_found
      end
    with Not_found -> begin
      let verbose_opt = if verbose >= 4 then "-verbose" else "" in
      let cmd = (sprintf "%s -c %s %s %s %s" compiler cflags includes filename verbose_opt) in
      let exit_code = match process_err with
        | None -> command cmd
        | Some process_err -> exec ~process_err ~verbose:(verbose>=2) cmd
      in
      may times (fun times -> Table.add times filename opt (Unix.gettimeofday()));
      exit_code
    end
  end else 0

(** link *)
let link ~compilation ~compiler ~outkind ~lflags ~package ~includes ~libs ~outname ~deps
    ?(process_err : process_err_func option) ~verbose () =
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
  kprintf (exec (*command*) ?process_err ~verbose:(verbose>=2)) "%s%s %s %s -o %s %s %s %s %s"
    compiler
    (if use_findlib && (outkind <@ [Executable]) then " -linkpkg" else "")
    (match outkind with Library -> "-a" | Plugin when opt -> "-shared" | Plugin -> "" | Pack -> "-pack" | Executable | External -> "")
    lflags
    outname
    includes
    libs
    deps
    (if verbose >= 3 then "-verbose" else "")
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
          | Plugin when compilation = Native -> ".cmxs"
          | Plugin -> ".cma"
          | Pack -> ".cmx"
          | External -> ""
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

(** install *)
let install ~compilation ~outkind ~outname ~deps ~path ~ccomp_type =
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
    | Executable (*->
      mkdir_p path;
      cp outname (path // dest_outname)*)
    | Plugin | Pack | External -> eprintf "\"Oebuild.install\" not implemented for Executable, Plugin, Pack or External."
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
          let dependants = Dep.find_dependants ~path:(List.map Filename.dirname targets) ~modname in
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
    ~thread ~vmthread ~annot ~bin_annot ~pp ?inline ~cflags ~lflags ~outname ~deps ~dontlinkdep (*~ms_paths*)
    ~targets ?(prof=false) ?(verbose=2) () =

  if verbose >= 1 then begin
    printf "%s %s"
      (string_of_compilation_type compilation) (string_of_output_type outkind);
    if outname <> ""    then (printf " %s\n" outname);
    if dontlinkdep      then (printf "  ~dontlinkdep\n");
    if compile_only     then (printf "  ~compile_only\n");
    if thread           then (printf "  ~thread\n");
    if vmthread         then (printf "  ~vmthread\n");
    if bin_annot        then (printf "  ~binannot\n");
    if package <> ""    then (printf "  ~package ... : %s\n" package);
    if includes <> ""   then (printf "  ~search_path : %s\n" includes);
    if libs <> ""       then (printf "  ~libs ...... : %s\n" libs);
    if other_mods <> "" then (printf "  ~other_mods  : %s\n" other_mods);
    (match inline with Some n ->
                              printf "  ~inline .... : %d\n" n | _ -> ());
    if cflags <> ""     then (printf "  ~cflags .... : %s\n" cflags);
    if lflags <> ""     then (printf "  ~lflags .... : %s\n" cflags);
    if targets <> []    then (printf "  ~source .... : %s\n" (String.concat "," targets));
    if verbose >= 2 then (printf "\n");
    printf "%!";
  end;

  let split_space = Str.split re_spaces in
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
  if bin_annot then (cflags := !cflags ^ " -bin-annot");
  if pp <> "" then (cflags := !cflags ^ " -pp " ^ pp);
  (* inline *)
  begin
    match inline with
      | Some inline when compilation = Native ->
        let inline = string_of_int inline in
        cflags := !cflags ^ " -inline " ^ inline;
        lflags := !lflags ^ " -inline " ^ inline;
      | _ -> ()
  end;
  (* compiling *)
  let package = check_package_list package in
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
  (*  *)
  let mods = split_space other_mods in
  let mods = if compilation = Native then List.map (sprintf "%s.cmx") mods else List.map (sprintf "%s.cmo") mods in
  (*if compilation = Native && !ms_paths then begin
    lflags := !lflags ^
      " -ccopt \"-LC:\\Programmi\\MIC977~1\\Lib -LC:\\Programmi\\MID05A~1\\VC\\lib -LC:\\GTK\\lib\""
  end;*)
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
            Table.update ~opt times filename;
            let exit_code = compile
              ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~cache:times ~opt)
              ~times ~opt ~compiler ~cflags:!cflags ~package ~includes:!includes ~filename ~verbose ()
            in
            if exit_code <> 0 then (Table.remove times filename opt);
            exit_code
          in
          if List.length !recompile > 0 then begin
            List.iter begin fun filename ->
              compilation_exit := compile ~times ~opt ~compiler ~cflags:!cflags ~package ~includes:!includes ~filename ~verbose ();
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
      let find_objs filenames =
        let ext = if compilation = Native then "cmx" else "cmo" in
        let filenames = List.filter (fun x -> x ^^ "ml") filenames in
        List.map (fun x -> sprintf "%s.%s" (Filename.chop_extension x) ext) filenames
      in
      let obj_deps = if dontlinkdep then find_objs targets else mods @ (find_objs deps) in
      if compile_only then !compilation_exit else begin
        let compiler_output = Buffer.create 100 in
        let rec try_link () =
          let recompile = ref [] in
          let link_exit =
            link ~compilation ~compiler ~outkind ~lflags:!lflags
              ~package
              ~includes:!includes ~libs ~deps:obj_deps ~outname
              ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~cache:times ~opt)
              ~verbose ()
          in
          if List.length !recompile > 0 then begin
            List.iter begin fun filename ->
              ignore (compile ~times ~opt ~compiler ~cflags:!cflags ~package ~includes:!includes ~filename ~verbose ())
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
let obj_extensions = [".cmi"; ".cmo"; ".cmx"; ".o"; ".obj"; ".annot"; ".cmt"; ".cmti"]
let lib_extensions = [".cma"; ".cmxa"; ".lib"; ".a"]

let clean ~deps () =
  let files = List.map begin fun name ->
    let name = Filename.chop_extension name in
    List.map ((^) name) obj_extensions
  end deps in
  let files = List.flatten files in
  (*let files = if outkind = Executable || all then (match outname with Some x -> x :: files | _ -> files) else files in*)
  let files = remove_dupl files in
  List.iter (fun file -> remove_file ~verbose:false file) files
;;

(** distclean - doesn't remove executables *)
let distclean () =
  let cwd = Sys.getcwd() in
  let exists_suffix sufs name = List.exists (fun suf -> name ^^ suf) sufs in
  let rec clean_dir dir =
    if not ((Unix.lstat dir).Unix.st_kind = Unix.S_LNK) then begin
      let files = Sys.readdir dir in
      let files = Array.to_list files in
      let files = List.map (fun x -> dir // x) files in
      let directories, files = List.partition Sys.is_directory files in
      let files = List.filter (exists_suffix (obj_extensions @ lib_extensions)) files in
      List.iter (remove_file ~verbose:false) files;
      let oebuild_times_filename = dir // Table.oebuild_times_filename in
      remove_file ~verbose:false oebuild_times_filename;
      List.iter clean_dir directories;
    end
  in
  clean_dir cwd;
;;

let re_fl_pkg_exist = Str.regexp "HAVE_FL_PKG(\\([-A-Za-z0-9_., ]+\\))"
let re_comma = Str.regexp " *, *"

(** check_restrictions *)
let check_restrictions restr =
  List.for_all begin function
    | "IS_UNIX" -> Sys.os_type = "Unix"
    | "IS_WIN32" -> Sys.os_type = "Win32"
    | "IS_CYGWIN" -> Sys.os_type = "Cygwin"
    | "HAVE_NATIVE" | "HAS_NATIVE" -> Ocaml_config.can_compile_native () <> None (* should be cached *)
    | res when Str.string_match re_fl_pkg_exist res 0 ->
      let packages = Str.matched_group 1 res in
      let packages = Str.split re_comma packages in
      let redirect_stderr = if Sys.os_type = "Win32" then " 1>NUL 2>NUL" else " 2>/dev/null" in
      packages = [] || List.for_all begin fun package ->
        kprintf (Oebuild_util.command ~echo:false) "ocamlfind query %s %s" package redirect_stderr = 0
      end packages
    | _ -> false
  end restr;;
