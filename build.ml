#directory "+threads" #load "str.cma" #load "unix.cma" #load "threads.cma"
let split re = Str.split (Str.regexp re)
module Quote = struct let path = if Sys.os_type = "Win32" then (fun x -> Filename.quote (Filename.quote x)) else (fun x -> x) let arg = if Sys.os_type = "Win32" then (fun x -> Filename.quote x) else (fun x -> x) end
module Argc = struct type command_descr = string type command_usage = string type speclist = (Arg.key * Arg.spec * Arg.doc) list module type COMMAND = sig type t val string_of_command : t -> string val command_of_string : string -> t val options : (t * speclist * command_descr * command_usage) list val anon_fun : t -> string -> unit end module Make (C : COMMAND) = struct open Printf exception Command_found exception Help_Command of C.t * (speclist * command_descr * command_usage) * string let command : C.t option ref = ref None;; let commands, cmd_map = List.fold_left (fun (x, y) (a, b, c, d) -> a::x, (a, (b, c, d))::y) ([], []) C.options let rpad txt c width = let result = txt ^ (String.make width c) in String.sub result 0 width;; let help_of_commands = let help_of_command maxlength cmd = let spec , descr, _ = List.assoc cmd cmd_map in (sprintf "  %s  %s" (rpad (C.string_of_command cmd) ' ' maxlength) descr) in let maxlength = List.fold_left (fun cand x -> max cand (String.length (C.string_of_command x))) 0 commands in "\n" ^ (String.concat "\n" (List.map (help_of_command maxlength) (List.rev commands))) let create_help_msg global_speclist usage_msg = sprintf "%s\n\nGLOBAL OPTIONS%s\nCOMMANDS\n%s\n" usage_msg (Arg.usage_string global_speclist "") help_of_commands;; let with_command f = match !command with Some cmd -> f cmd | _ -> assert false;; let parse_argv args ~(global_options : speclist) ?default_command ?(usage_msg=sprintf "\nUSAGE\n  %s [global_options*] <command> [options*] [args*]\n  %s <command> --help" args.(0) args.(0)) execute_command = command := None; Arg.current := 0; let parse_anon arg = match !command with | None -> let cmd = try C.command_of_string arg with ex -> decr Arg.current; (match default_command with Some c -> c | _ -> raise ex) in command := Some cmd; raise Command_found | _ -> assert false in let help_string () = match !command with | Some cmd -> let spec, descr, _ = List.assoc cmd cmd_map in let cmd = C.string_of_command cmd in Arg.usage_string spec (sprintf "%s %s - %s\n\nUSAGE\n  %s [global_options*] %s [options*] [args*]\n\nOPTIONS" args.(0) cmd descr args.(0) cmd) | _ -> create_help_msg global_options usage_msg in if Array.length args = 1 then (raise (Arg.Help (help_string()))); try Arg.parse_argv args global_options parse_anon usage_msg; with | Command_found -> let len = Array.length args - !Arg.current in let command_args = Array.create len "" in Array.blit args !Arg.current command_args 0 len; let parse_anon = with_command (fun cmd -> C.anon_fun cmd) in begin try Arg.current := 0; let speclist = with_command (fun cmd -> let sp, _, _ = try List.assoc cmd cmd_map with Not_found -> assert false in sp) in Arg.parse_argv command_args speclist parse_anon usage_msg; let f = function | Some cmd -> execute_command cmd | None -> raise (Arg.Help (create_help_msg global_options usage_msg)) in f !command with | Arg.Help _ -> with_command begin fun cmd -> let cmd_specs, cmd_descr, cmd_usage = List.assoc cmd cmd_map in raise (Help_Command  (cmd, (cmd_specs, cmd_descr, cmd_usage), help_string ())) end | Arg.Bad msg as ex -> with_command begin fun cmd -> try let first_line = String.sub msg 0 (String.index msg '\n') in raise (Arg.Bad (sprintf "%s\n%s" first_line (help_string()))) with Not_found -> raise ex end end; | Arg.Bad _ -> raise (Arg.Bad (sprintf "unknown global option `%s'\n%s" (args.(!Arg.current)) (help_string()))) | Arg.Help _ -> raise (Arg.Help (help_string()));; let parse ~global_options ?default_command ?usage_msg f = parse_argv Sys.argv ~global_options ?default_command ?usage_msg f;; end;; end
module Cmd = struct open Printf let expand = let trimfunc = let replace = Str.global_replace (Str.regexp "\\(^[ \t\r\n]+\\)\\|\\([ \t\r\n]+$\\)") in fun str -> replace "" str in fun ?(trim=true) ?(first_line=false) ?filter command -> let ichan = Unix.open_process_in command in let finally () = ignore (Unix.close_process_in ichan) in let data = Buffer.create 100 in begin try let get_line ichan = if trim then trimfunc (input_line ichan) else input_line ichan in while true do let line = get_line ichan in if first_line && String.length line = 0 then begin end else if first_line then begin Buffer.add_string data line; raise End_of_file end else begin match filter with | None -> Buffer.add_string data line; Buffer.add_char data '\n' | Some f when (f line) -> Buffer.add_string data line; Buffer.add_char data '\n' | _ -> () end done with | End_of_file -> () | ex -> (finally(); raise ex) end; finally(); if Buffer.length data = 0 then (kprintf failwith "Cmd.expand: %s" command); Buffer.contents data;; end
module Ocaml_config = struct open Printf let putenv_ocamllib value = match Sys.os_type with | "Win32" -> let value = match value with None -> "" | Some x -> x in Unix.putenv "OCAMLLIB" value | _ -> ignore (Sys.command "unset OCAMLLIB") let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null" let find_best_compiler compilers = try Some (List.find begin fun comp -> try ignore (kprintf Cmd.expand "%s -version%s" comp redirect_stderr); true with _ -> false end compilers) with Not_found -> None;; let find_tool which path = let commands = match which with | `BEST_OCAMLC -> ["ocamlc.opt"; "ocamlc"] | `BEST_OCAMLOPT -> ["ocamlopt.opt"; "ocamlopt"] | `BEST_OCAMLDEP -> ["ocamldep.opt"; "ocamldep"] | `BEST_OCAMLDOC -> ["ocamldoc.opt"; "ocamldoc"] | `OCAMLC -> ["ocamlc"] | `OCAML -> ["ocaml"] in let quote    = if path <> "" && Sys.os_type = "Win32" && String.contains path ' ' then Filename.quote else (fun x -> x) in let path     = if path <> "" then Filename.concat path "bin" else "" in find_best_compiler (List.map quote (List.map (Filename.concat path) commands)) let get_home () = try Sys.getenv "OCAML_HOME" with Not_found -> "" let expand_includes = let split = Str.split (Str.regexp " +") in fun compact -> if String.length compact > 0 then ("-I " ^ (String.concat " -I " (split compact))) else ""  let ocamlc ()   = match find_tool `BEST_OCAMLC (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocamlc'" let ocamlopt () = find_tool `BEST_OCAMLOPT (get_home ()) let ocamldep () = match find_tool `BEST_OCAMLDEP (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocamldep'" let ocamldoc () = match find_tool `BEST_OCAMLDOC (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocamldoc'" let ocaml ()    = match find_tool `OCAML (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocaml'" let ocamllib () = Cmd.expand ~first_line:true ((ocamlc()) ^ " -where")  let ocaml_version ?(compiler=ocamlc()) () = Cmd.expand (compiler ^ " -v " ^ redirect_stderr)  let can_compile_native ?ocaml_home () = let result = ref false in let filename = Filename.temp_file "test_native" ".ml" in let ochan = open_out filename in begin try output_string ochan ("0"); close_out ochan with _ -> (close_out ochan) end; let outname = Filename.chop_extension filename in let compiler = match ocaml_home with | Some home -> find_tool `BEST_OCAMLOPT home | _ -> Some "ocamlopt" in match compiler with | Some compiler -> let cmd = sprintf "%s -o %s %s%s" compiler outname filename redirect_stderr in result := (Sys.command cmd) = 0; if Sys.file_exists filename then (Sys.remove filename); if Sys.file_exists outname then (Sys.remove outname); let cmi = outname ^ ".cmi" in if Sys.file_exists cmi then (Sys.remove cmi); let cmx = outname ^ ".cmx" in if Sys.file_exists cmx then (Sys.remove cmx); let obj = outname ^ ".o" in if Sys.file_exists obj then (Sys.remove obj); let obj = outname ^ ".obj" in if Sys.file_exists obj then (Sys.remove obj); if !result then begin let conf = kprintf Cmd.expand "%s -config" compiler in let re = Str.regexp "ccomp_type: \\(.*\\)\n" in if Str.search_forward re conf 0 >= 0 then begin Some (Str.matched_group 1 conf) end else Some "<unknown ccomp_type>" end else None; | _ -> None ;; end
module Dep = struct open Printf exception Loop_found of string let trim = let re = Str.regexp "[ \n\r\n\t]+$" in Str.replace_first re "" let (^^) = Filename.check_suffix let (!$) = Filename.chop_extension let (//) = Filename.concat let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null" let re1 = Str.regexp " ?:\\( \\|$\\)" let re2 = Str.regexp " \\\\[\r\n]+" let re3 = Str.regexp " " let re_ss = Str.regexp "\\\\ " let re_00 = Str.regexp "\x00\x00" let split_nl = Str.split (Str.regexp "\n")  let replace_extension x = sprintf "%s.%s" (Filename.chop_extension x) (if x ^^ "cmi" then "mli" else if x ^^ "cmx" then "ml" else assert false);;  let find_dep ?pp ?includes ?(with_errors=true) ?(echo=true) target = let dir = Filename.dirname target in let anti_loop = ref [] in let table = Hashtbl.create 7 in let redirect_stderr = if with_errors then "" else (if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null") in let command = sprintf "%s%s %s -native -slash %s %s %s" (Ocaml_config.ocamldep()) (match pp with Some pp when pp <> "" -> " -pp " ^ pp | _ -> "" ) (Ocaml_config.expand_includes dir) (match dir with "." -> "*.mli" | _ -> dir // "*.mli *.mli") (match dir with "." -> "*.ml" | _ -> dir // "*.ml *.ml") redirect_stderr in if echo then (printf "%s\n%!" command); let ocamldep = Cmd.expand command in let ocamldep = Str.global_replace re2 " " ocamldep in let entries = split_nl ocamldep in List.iter begin fun entry -> match Str.split re1 entry with | key :: [] -> Hashtbl.add table key None | [key; deps] -> let deps = Str.global_replace re_ss "\x00\x00" deps in let deps = Str.split re3 deps in let deps = List.map (Str.global_replace re_00 "\\ ") deps in Hashtbl.add table key (Some deps) | _ -> eprintf "%s\n%s\n%!" command entry; assert false end entries; let target = (Filename.chop_extension target) ^ ".cmx" in let result = ref [] in let rec find_chain target = if (List.mem target !anti_loop) && (not (List.mem target !result)) then (raise (Loop_found (String.concat " " (List.map replace_extension (target :: !anti_loop))))); anti_loop := target :: (List.filter (fun x -> x <> target) !anti_loop); try if not (List.mem target !result) then begin match Hashtbl.find table target with | None -> result := target :: !result | Some deps -> List.iter find_chain deps; result := target :: !result; end with Not_found ->  (kprintf failwith "Dep: %s" target) in find_chain target; List.rev (List.map replace_extension !result)  let find ?pp ?includes ?with_errors ?(echo=true) targets = let deps = List.map (find_dep ?pp ?includes ?with_errors ~echo) targets in let deps = List.flatten deps in List.rev (List.fold_left begin fun acc x -> if not (List.mem x acc) then x :: acc else acc end [] deps)  let find_dependants = let re = Str.regexp "\\(.+\\.mli?\\) ?: ?\\(.*\\)" in let re1 = Str.regexp "\r?\n" in let re2 = Str.regexp " " in fun ~target ~modname -> let dir = Filename.dirname target in let dir = if dir = Filename.current_dir_name then "" else (dir ^ "/") in let cmd = sprintf "%s -modules -native %s*.ml %s*.mli%s" (Ocaml_config.ocamldep()) dir dir redirect_stderr in printf "%s (%s)\n%!" cmd modname; let ocamldep = Cmd.expand cmd in let entries = Str.split re1 ocamldep in let entries = List.map begin fun entry -> if Str.string_match re entry 0 then begin let filename = Str.matched_group 1 entry in let modules = Str.matched_group 2 entry in (filename, (Str.split re2 modules)) end else (assert false) end entries in let dependants = ref [] in let rec loop modname = List.iter begin fun (filename, modules) -> if List.mem modname modules then begin if not (List.mem filename !dependants) then begin dependants := filename :: !dependants; let prefix = Filename.chop_extension filename in let prefix_mli = prefix ^ ".mli" in if List.mem_assoc prefix_mli entries then (dependants := prefix_mli :: !dependants;); let mdep = String.capitalize prefix in ignore (loop mdep); end end end entries; !dependants in loop modname let find_dependants ~targets ~modname = let dependants = List.map (fun target -> find_dependants ~target ~modname) targets in List.flatten dependants ;; end
module Cmd_line_args = struct type state = StartArg | InUnquotedArg | InQuotedArg | InQuotedArgAfterQuote;; let format = String.concat " ";; let parse line = let args = ref [] in let buf = Buffer.create 10 in let state = ref StartArg in let start_arg () = state := StartArg; args := (Buffer.contents buf) :: !args; Buffer.clear buf; in String.iter begin function | (' ' as ch) when !state = InQuotedArg -> Buffer.add_char buf ch | ' ' when !state = StartArg -> () | ' ' when !state = InUnquotedArg -> start_arg (); | ' ' -> start_arg () | ('"' as ch) when !state = StartArg -> state := InQuotedArg; Buffer.add_char buf ch | ('"' as ch) when !state = InQuotedArg -> Buffer.add_char buf ch; start_arg (); | ('"' as ch) when !state = InQuotedArgAfterQuote -> Buffer.add_char buf ch; state := InQuotedArg; | ('"' as ch) when !state = InUnquotedArg -> start_arg (); Buffer.add_char buf ch; state := InQuotedArg; | ('\\' as ch) when !state = InQuotedArg -> state := InQuotedArgAfterQuote; Buffer.add_char buf ch | ch when !state = InQuotedArgAfterQuote -> state := InQuotedArg; Buffer.add_char buf ch; | ch when !state = StartArg -> state := InUnquotedArg; Buffer.add_char buf ch; | ch -> Buffer.add_char buf ch; end line; if Buffer.length buf > 0 then (start_arg ()); List.rev !args;;  end
module Task = struct type kind = [ `CLEAN | `CLEANALL | `ANNOT | `COMPILE | `RUN | `OTHER] type phase = Before_clean | Clean | After_clean | Before_compile | Compile | After_compile type t = { mutable et_name                  : string; mutable et_env                   : (bool * string) list; mutable et_env_replace           : bool;                                mutable et_dir                   : string;                                                                                            mutable et_cmd                   : string; mutable et_args                  : (bool * string) list; mutable et_phase                 : phase option; mutable et_always_run_in_project : bool; mutable et_always_run_in_script  : bool; } let string_of_phase = function | Before_clean -> "Before_clean" | Clean -> "Clean" | After_clean -> "After_clean" | Before_compile -> "Before_compile" | Compile -> "Compile" | After_compile -> "After_compile" let descr_of_phase = function | Before_clean -> "Pre-clean" | Clean -> "Clean" | After_clean -> "Post-clean" | Before_compile -> "Pre-build" | Compile -> "Build" | After_compile -> "Post-build" let phase_of_string = function | "Before_clean" -> Before_clean | "Clean" -> Clean | "After_clean" -> After_clean | "Before_compile" -> Before_compile | "Compile" -> Compile | "After_compile" -> After_compile | _ -> failwith "phase_of_string" let create ~name ~env ?(env_replace=false) ~dir ~cmd ~args ?phase () = { et_name                  = name; et_env                   = env; et_env_replace           = env_replace; et_dir                   = dir; et_cmd                   = cmd; et_args                  = args; et_phase                 = phase; et_always_run_in_project = false; et_always_run_in_script  = true; }  let handle f task = let tenv = Array.of_list task.et_env in let env = if task.et_env_replace then Array.concat [                        tenv] else (Array.concat [tenv                       ; (Array.map (fun e -> true, e) (Unix.environment()))]) in let env = List.filter (fun (e, _) -> e) (Array.to_list env) in let env = Array.of_list (List.map (fun (_, v) -> v) env) in let prog = task.et_cmd in let dir = if task.et_dir <> "" then task.et_dir else (Sys.getcwd ()) in let args = List.filter (fun (e, _) -> e) task.et_args in let args = List.flatten (List.map (fun (_, v) -> Cmd_line_args.parse v) args) in f ~env ~dir ~prog ~args;; end
module Build_script_command = struct open Printf type t = [`Show | `Build | `Install | `Uninstall | `Install_lib | `Clean | `Distclean] let commands = [`Show; `Build; `Install; `Uninstall; `Install_lib; `Clean; `Distclean] exception Unrecognized_command of string let string_of_command = function | `Show -> "show" | `Build -> "build" | `Install -> "install" | `Uninstall -> "uninstall" | `Install_lib -> "install-lib" | `Clean -> "clean" | `Distclean -> "distclean" let command_of_string = function | "show" -> `Show | "build" -> `Build | "install" -> `Install | "uninstall" -> `Uninstall | "install-lib" -> `Install_lib | "clean" -> `Clean | "distclean" -> `Distclean | x -> raise (Unrecognized_command (sprintf "`%s' is not a recognized command." x));; let code_of_command = function | `Show -> "`Show" | `Build -> "`Build" | `Install -> "`Install" | `Uninstall -> "`Uninstall" | `Install_lib -> "`Install_lib" | `Clean -> "`Clean" | `Distclean -> "`Distclean" end
module Oebuild_util = struct open Printf let (!$) = Filename.chop_extension let (//) = Filename.concat let (^^) = Filename.check_suffix let is_win32, win32 = (Sys.os_type = "Win32"), (fun a b -> match Sys.os_type with "Win32" -> a | _ -> b) let may opt f = match opt with Some x -> f x | _ -> () let re_spaces = Str.regexp " +"  let crono ?(label="Time") f x = let finally time = Printf.fprintf stdout "%s: %f sec." label (Unix.gettimeofday() -. time); print_newline(); in let time = Unix.gettimeofday() in let result = try f x with e -> begin finally time; raise e end in finally time; result  let remove_dupl l = List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)  let remove_file ?(verbose=false) filename = try if Sys.file_exists filename then (Sys.remove filename; if verbose then print_endline filename) with Sys_error ex -> eprintf "%s\n%!" ex  let command ?(echo=true) cmd = let cmd = Str.global_replace re_spaces " " cmd in if echo then (printf "%s\n%!" cmd); let exit_code = Sys.command cmd in Pervasives.flush stderr; Pervasives.flush stdout; exit_code  let iter_chan chan f = try while true do f chan done with End_of_file -> ()  let exec ?(env=Unix.environment()) ?(echo=true) ?(join=true) ?at_exit ?(process_err=(fun ~stderr -> prerr_endline (input_line stderr))) cmd = let cmd = Str.global_replace re_spaces " " cmd in if echo then (print_endline cmd); let (inchan, _, errchan) as channels = Unix.open_process_full cmd env in let close () = match Unix.close_process_full channels with 	| Unix.WEXITED code -> code 	| _ -> (-1) in let thi = Thread.create begin fun () -> iter_chan inchan (fun chan -> print_endline (input_line chan)); end () in let the = Thread.create begin fun () -> iter_chan errchan (fun chan -> process_err ~stderr:chan); Thread.join thi; (match at_exit with None -> () | Some f -> ignore (close()); f()); end () in if join then begin Thread.join thi; Thread.join the; end; if at_exit = None then (close()) else 0  let rm = win32 "DEL /F /Q" "rm -f"  let copy_file ic oc = let buff = String.create 0x1000 in let rec copy () = let n = input ic buff 0 0x1000 in if n = 0 then () else (output oc buff 0 n; copy()) in copy() let cp ?(echo=true) src dst = let ic = open_in_bin src in let oc = open_out_bin dst in if echo then (printf "%s -> %s\n%!" src dst); let finally () = close_out oc; close_in ic in try copy_file ic oc; finally() with ex -> (finally(); raise ex)  let rec mkdir_p ?(echo=true) d = if not (Sys.file_exists d) then begin mkdir_p (Filename.dirname d); printf "mkdir -p %s\n%!" d; (Unix.mkdir d 0o755) end end
module Oebuild_table = struct open Printf  type t = (string, float) Hashtbl.t let oebuild_times_filename = ".oebuild" let (^^) filename opt = filename ^ (if opt then ".opt" else ".byt") let find (table : t) filename opt = Hashtbl.find table (filename ^^ opt) let add (table : t) filename opt = Hashtbl.add table (filename ^^ opt) let remove (table : t) filename opt = Hashtbl.remove table (filename ^^ opt)  let read () = if not (Sys.file_exists oebuild_times_filename) then begin let ochan = open_out_bin oebuild_times_filename in Marshal.to_channel ochan (Hashtbl.create 7) []; close_out ochan end; let ichan = open_in_bin oebuild_times_filename in let times = Marshal.from_channel ichan in close_in ichan; (times : t)  let write (times : t) = if Hashtbl.length times > 0 then begin let ochan = open_out_bin oebuild_times_filename in Marshal.to_channel ochan times []; close_out ochan end  let update = let get_last_compiled_time ~opt cache filename = try let time = find cache filename opt in let ext = if opt then "cmx" else "cmo" in let cm = sprintf "%s.%s" (Filename.chop_extension filename) ext in if Sys.file_exists cm then time else begin remove cache filename opt; raise Not_found end with Not_found -> 0.0 in fun ~deps ~opt (cache : t) filename -> let ctime = get_last_compiled_time ~opt cache filename in if ctime > 0.0 && ((Unix.stat filename).Unix.st_mtime) >= ctime then begin remove cache filename opt; end ;; end
module Oebuild = struct open Printf open Oebuild_util module Table = Oebuild_table type compilation_type = Bytecode | Native | Unspecified type output_type = Executable | Library | Plugin | Pack type build_exit = Built_successfully | Build_failed of int type process_err_func = (stderr:in_channel -> unit) let string_of_compilation_type = function | Bytecode -> "Bytecode" | Native -> "Native" | Unspecified -> "Unspecified" let ocamlc = Ocaml_config.ocamlc() let ocamlopt = Ocaml_config.ocamlopt() let ocamllib = Ocaml_config.ocamllib()  let compile ?(times : Table.t option) ~opt ~compiler ~cflags ~package ~includes ~filename ?(process_err : process_err_func option) () = if Sys.file_exists filename then begin try begin match times with | Some times -> ignore (Table.find times filename opt); 0                 | _ -> raise Not_found end with Not_found -> begin let cmd = (sprintf "%s -c %s %s %s" compiler cflags includes filename) in let exit_code = match process_err with | None -> command cmd | Some process_err -> exec ~process_err cmd in may times (fun times -> Table.add times filename opt (Unix.gettimeofday())); exit_code end end else 0  let link ~compilation ~compiler ~outkind ~lflags ~package ~includes ~libs ~outname ~deps ?(process_err : process_err_func option) () = let opt = compilation = Native && ocamlopt <> None in let libs = if            outkind <> Executable then "" else let ext = if opt then "cmxa" else "cma" in let libs = List.map begin fun x -> if Filename.check_suffix x ".o" then begin let x = Filename.chop_extension x in let ext = if opt then "cmx" else "cmo" in sprintf "%s.%s" x ext end else if Filename.check_suffix x ".obj" then begin sprintf "%s" x end else (sprintf "%s.%s" x ext) end libs in String.concat " " libs in let use_findlib = package <> "" in let deps = String.concat " " deps in kprintf (exec             ?process_err) "%s%s %s %s -o %s %s %s %s" compiler (if use_findlib && outkind = Executable then " -linkpkg" else "") (match outkind with Library -> "-a" | Plugin -> "-shared" | Pack -> "-pack" | Executable -> "") lflags outname includes libs deps ;;  let get_output_name ~compilation ~outkind ~outname ~targets = match targets with | [] -> None | _ -> Some begin let o_ext = match outkind with | Library when compilation = Native -> ".cmxa" | Library -> ".cma" | Executable when compilation = Native -> ".opt" ^ (win32 ".exe" "") | Executable -> win32 ".exe" "" | Plugin -> ".cmxs" | Pack -> ".cmx" in let name = if outname = "" then begin match (List.rev targets) with | last :: _ -> Filename.chop_extension last | _ -> assert false end else outname in name ^ o_ext end ;;  let install ~compilation ~outkind ~outname ~deps ~path ~ccomp_type = let dest_outname = Filename.basename outname in match outkind with | Library -> let path = let path = ocamllib // path in mkdir_p path; path in cp outname (path // dest_outname); let deps_mod = List.map Filename.chop_extension deps in let deps_mod = remove_dupl deps_mod in let cmis = List.map (fun d -> sprintf "%s.cmi" d) deps_mod in let mlis = List.map (fun cmi -> sprintf "%s.mli" (Filename.chop_extension cmi)) cmis in let mlis = List.filter Sys.file_exists mlis in List.iter (fun x -> ignore (cp x (path // (Filename.basename x)))) cmis; List.iter (fun x -> ignore (cp x (path // (Filename.basename x)))) mlis; if compilation = Native then begin let ext = match ccomp_type with Some "msvc" -> ".lib" | Some _ ->  ".a" | None -> assert false in let basename = sprintf "%s%s" (Filename.chop_extension outname) ext in cp basename (path // (Filename.basename basename)); end; | Executable                                                                    | Plugin | Pack -> eprintf "\"Oebuild.install\" not implemented for Executable, Plugin or Pack." ;;  let run_output ~outname ~args = let args = List.rev args in if is_win32 then begin let cmd = Str.global_replace (Str.regexp "/") "\\\\" outname in let args = String.concat " " args in ignore (kprintf command "%s %s" cmd args) end else begin let cmd = Filename.current_dir_name // outname in  let args = cmd :: args in let args = Array.of_list args in Unix.execv cmd args end ;;  let sort_dependencies ~deps subset = let result = ref [] in List.iter begin fun x -> if List.mem x subset then (result := x :: !result) end deps; List.rev !result ;;  let filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~(cache : Table.t) ~opt = let re_inconsistent_assumptions = Str.regexp ".*make[ \t\r\n]+inconsistent[ \t\r\n]+assumptions[ \t\r\n]+over[ \t\r\n]+\\(interface\\|implementation\\)[ \t\r\n]+\\([^ \t\r\n]+\\)[ \t\r\n]+" in let re_error = Str.regexp "Error: " in ((fun ~stderr -> let line = input_line stderr in Buffer.add_string compiler_output (line ^ "\n"); let messages = Buffer.contents compiler_output in let len = String.length messages in try let pos = Str.search_backward re_error messages len in let last_error = String.sub messages pos (len - pos) in begin try let _ = Str.search_backward re_inconsistent_assumptions last_error (String.length last_error) in let modname = Str.matched_group 2 last_error in let dependants = Dep.find_dependants ~targets ~modname in let dependants = sort_dependencies ~deps dependants in let _                    = Buffer.contents compiler_output in eprintf "Warning (oebuild): the following files make inconsistent assumptions over interface/implementation %s: %s\n%!" modname (String.concat ", " dependants); List.iter begin fun filename -> Table.remove cache filename opt; let basename = Filename.chop_extension filename in let cmi = basename ^ ".cmi" in if Sys.file_exists cmi then (Sys.remove cmi); let cmo = basename ^ ".cmo" in if Sys.file_exists cmo then (Sys.remove cmo); let cmx = basename ^ ".cmx" in if Sys.file_exists cmx then (Sys.remove cmx); let obj = basename ^ (win32 ".obj" ".o") in if Sys.file_exists obj then (Sys.remove obj); end dependants; recompile := dependants; with Not_found -> () end with Not_found -> ()) : process_err_func) ;;  let build ~compilation ~package ~includes ~libs ~other_mods ~outkind ~compile_only ~thread ~vmthread ~annot ~pp ?inline ~cflags ~lflags ~outname ~deps ~ms_paths ~targets ?(prof=false) () = let split_space = Str.split (Str.regexp " +") in  let includes = ref includes in includes := Ocaml_config.expand_includes !includes;  let libs = split_space libs in  let cflags = ref cflags in let lflags = ref lflags in  if thread then (cflags := !cflags ^ " -thread"; lflags := !lflags ^ " -thread"); if vmthread then (cflags := !cflags ^ " -vmthread"; lflags := !lflags ^ " -vmthread"); if annot then (cflags := !cflags ^ " -annot"); if pp <> "" then (cflags := !cflags ^ " -pp " ^ pp); begin match inline with | Some inline when compilation = Native -> let inline = string_of_int inline in cflags := !cflags ^ " -inline " ^ inline; lflags := !lflags ^ " -inline " ^ inline; | _ -> () end;  let compiler = if prof then "ocamlcp -p a" else begin let compiler = if compilation = Native then (match ocamlopt with Some x -> x | _ -> failwith "ocamlopt was not found") else ocamlc in let use_findlib = package <> "" in if use_findlib then let cmp = try Filename.chop_extension compiler with Invalid_argument "Filename.chop_extension" -> compiler in sprintf "ocamlfind %s -package %s" cmp package else compiler end in  let mods = split_space other_mods in let mods = if compilation = Native then List.map (sprintf "%s.cmx") mods else List.map (sprintf "%s.cmo") mods in if compilation = Native && !ms_paths then begin lflags := !lflags ^ " -ccopt \"-LC:\\Programmi\\MIC977~1\\Lib -LC:\\Programmi\\MID05A~1\\VC\\lib -LC:\\GTK\\lib\"" end; let times = Table.read () in let build_exit = let compilation_exit = ref 0 in  begin try let opt = compilation = Native in let compiler_output = Buffer.create 100 in let rec try_compile filename = let recompile = ref [] in let compile_exit = Table.update ~deps ~opt times filename; let exit_code = compile ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~cache:times ~opt) ~times ~opt ~compiler ~cflags:!cflags ~package ~includes:!includes ~filename () in if exit_code <> 0 then (Table.remove times filename opt); exit_code in if List.length !recompile > 0 then begin List.iter begin fun filename -> compilation_exit := compile ~times ~opt ~compiler ~cflags:!cflags ~package ~includes:!includes ~filename (); if !compilation_exit <> 0 then (raise Exit) end !recompile; print_newline(); Buffer.clear compiler_output; try_compile filename; end else begin if Buffer.length compiler_output > 0 then (eprintf "%s\n%!" (Buffer.contents compiler_output)); compile_exit end in List.iter begin fun filename -> compilation_exit := try_compile filename; if !compilation_exit <> 0 then (raise Exit) end deps; with Exit -> () end;  if !compilation_exit = 0 then begin let opt = compilation = Native in let obj_deps = let ext = if compilation = Native then "cmx" else "cmo" in let deps = List.filter (fun x -> x ^^ "ml") deps in List.map (fun x -> sprintf "%s.%s" (Filename.chop_extension x) ext) deps in if compile_only then !compilation_exit else begin let obj_deps = mods @ obj_deps in let compiler_output = Buffer.create 100 in let rec try_link () = let recompile = ref [] in let link_exit = link ~compilation ~compiler ~outkind ~lflags:!lflags ~package ~includes:!includes ~libs ~deps:obj_deps ~outname ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~targets ~deps ~cache:times ~opt) () in if List.length !recompile > 0 then begin List.iter begin fun filename -> ignore (compile ~times ~opt ~compiler ~cflags:!cflags ~package ~includes:!includes ~filename ()) end !recompile; Buffer.clear compiler_output; try_link() end else begin eprintf "%s\n%!" (Buffer.contents compiler_output); link_exit end in try_link() end end else !compilation_exit in Table.write times; if build_exit = 0 then Built_successfully else (Build_failed build_exit) ;;  let obj_extensions = [".cmi"; ".cmo"; ".cmx"; ".o"; ".obj"; ".annot"; ".cmt"; ".cmti"] let lib_extensions = [".cma"; ".cmxa"; ".lib"; ".a"] let clean ~deps () = let files = List.map begin fun name -> let name = Filename.chop_extension name in List.map ((^) name) obj_extensions end deps in let files = List.flatten files in  let files = remove_dupl files in List.iter (fun file -> remove_file ~verbose:false file) files ;;  let distclean () = let cwd = Sys.getcwd() in let exists_suffix sufs name = List.exists (fun suf -> name ^^ suf) sufs in let rec clean_dir dir = if not ((Unix.lstat dir).Unix.st_kind = Unix.S_LNK) then begin let files = Sys.readdir dir in let files = Array.to_list files in let files = List.map (fun x -> dir // x) files in let directories, files = List.partition Sys.is_directory files in let files = List.filter (exists_suffix (obj_extensions @ lib_extensions)) files in List.iter (remove_file ~verbose:false) files; let oebuild_times_filename = dir // Table.oebuild_times_filename in remove_file ~verbose:false oebuild_times_filename; List.iter clean_dir directories; end in clean_dir cwd; ;;  let check_restrictions restr = List.for_all begin function | "IS_UNIX" -> Sys.os_type = "Unix" | "IS_WIN32" -> Sys.os_type = "Win32" | "IS_CYGWIN" -> Sys.os_type = "Cygwin" | "HAS_NATIVE" -> Ocaml_config.can_compile_native () <> None                        | _ -> false end restr;; end
module Build_script_util = struct open Arg open Printf open Oebuild open Oebuild_util open Task type target = { num : int; id : int; output_name : string; target_type : Oebuild.output_type; compilation_bytecode : bool; compilation_native : bool; toplevel_modules : string; package : string; search_path : string; required_libraries : string; compiler_flags : string; linker_flags : string; thread : bool; vmthread : bool; pp : string; inline : int option; library_install_dir : string; other_objects : string; external_tasks : int list; restrictions : string list; dependencies : int list; show : bool; } type target_map_entry = int * (string * target) exception Error let pushd, popd = let stack = Stack.create () in begin fun dir -> let cwd = Sys.getcwd () in Stack.push cwd stack; Sys.chdir dir end, (fun () -> Sys.chdir (Stack.pop stack));; let rpad txt c width = let result = txt ^ (String.make width c) in String.sub result 0 width;; let get_compilation_types native t = (if t.compilation_bytecode then [Bytecode] else []) @ (if t.compilation_native && native then [Native] else []) let string_of_compilation_type native t = let compilation = get_compilation_types native t in String.concat "/" (List.map string_of_compilation_type compilation) let create_target ?dir f x = (match dir with Some dir -> pushd dir | _ -> ()); f x; (match dir with Some _ -> popd() | _ -> ());; let create_target_func ?tg targets = match targets with | default_target :: _ -> (match tg with Some f -> create_target f | _ -> create_target default_target) | [] -> fun _ -> ();; let string_of_output_type = function | Executable -> "Executable" | Library -> "Library" | Plugin -> "Plugin" | Pack -> "Pack";;  let ccomp_type = Ocaml_config.can_compile_native () let system_config () = let ocaml_version = Cmd.expand ~first_line:true "ocamlc -v" in let std_lib = Cmd.expand ~first_line:true "ocamlc -where" in let properties = [ "OCaml", ocaml_version; "Standard library directory", std_lib; "OCAMLLIB", (try Sys.getenv "OCAMLLIB" with Not_found -> "<Not_found>"); "Native compilation supported", (match ccomp_type with Some ccomp_type -> ccomp_type | _ -> "No"); ] in let buf = Buffer.create 100 in Buffer.add_string buf "\nSystem configuration\n"; let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in List.iter (fun (n, v) -> bprintf buf "  %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties; Buffer.contents buf;;  module Option = struct let prefix = ref "" let change_dir = ref "src" end  module ETask = struct let filter tasks phase = List.filter begin fun task -> if task.et_always_run_in_script then match task.et_phase with | Some ph -> ph = phase | _ -> false else false end tasks;; let execute = Task.handle begin fun ~env ~dir ~prog ~args -> let cmd = sprintf "%s %s" prog (String.concat " " args) in let old_dir = Sys.getcwd () in Sys.chdir dir; let exit_code = Oebuild_util.exec ~env cmd in Sys.chdir old_dir; if exit_code > 0 then raise Error end end  let target : target_map_entry list ref = ref [] let add_target targets name = try begin try let num = int_of_string name in if num <= 0 then (raise Exit); let name_tg = try List.find (fun (_, tg) -> tg.num = num) targets with Not_found -> raise Exit in target := (num, name_tg) :: !target; with _ -> let tg = List.assoc name targets in if tg.num <= 0 then (raise Exit); target := (tg.num, (name, tg)) :: !target; end with Exit | Not_found -> (raise (Arg.Bad (sprintf "Invalid target `%s'" name)));;  let rec find_target_dependencies targets trg = remove_dupl (List.flatten (List.map begin fun id -> try let _, target = List.find (fun (_, tg) -> tg.id = id) targets in (find_target_dependencies targets target) @ [target] with Not_found -> [] end trg.dependencies));;  let show = fun targets -> function num, (name, t) -> let files = Str.split (Str.regexp " +") t.toplevel_modules in  let b_deps = find_target_dependencies targets t in let b_deps = List.map begin fun tg -> let name, _ = List.find (fun (_, t) -> t.id = tg.id) targets in name end b_deps in let compilation = (if t.compilation_bytecode then [Bytecode] else []) @ (if t.compilation_native && ccomp_type <> None then [Native] else []) in let outname = List.map begin fun compilation -> let oname = get_output_name ~compilation ~outkind:t.target_type ~outname:t.output_name ~targets:files in match oname with Some x -> x | _ -> "" end compilation in let outkind = string_of_output_type t.target_type in let compilation = string_of_compilation_type (ccomp_type <> None) t in let prop_1 = [ "Restrictions", (String.concat "," t.restrictions); "Output name", (String.concat ", " outname); ] in let prop_2 = [ "Findlib packages", t.package; "Search path", t.search_path; "Required libraries", t.required_libraries; "Compiler flags", t.compiler_flags; "Linker flags", t.linker_flags; "Toplevel modules", t.toplevel_modules; "Target dependencies", (String.concat ", " b_deps); ] in let properties = if t.target_type = Library then prop_1 @ [ "Install directory", (Oebuild.ocamllib // t.library_install_dir) ] @ prop_2 else prop_1 @ prop_2 in printf "%2d) %s (%s, %s)\n%!" num name outkind compilation; let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in List.iter (fun (n, v) -> printf "  %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;;  let install_lib ~compilation ~outname ~external_tasks ~deps target = match target.target_type with | Library -> let deps = deps() in Oebuild.install ~compilation ~outkind:target.target_type ~outname ~deps ~path:target.library_install_dir ~ccomp_type | Executable | Plugin | Pack -> eprintf "\"install\" not implemented for Executable, Plugin or Pack."; raise Exit;;  let rec execute_target ~external_tasks ~targets ~command target = if Oebuild.check_restrictions target.restrictions then let compilation = (if target.compilation_bytecode then [Bytecode] else []) @ (if target.compilation_native && (ccomp_type <> None) then [Native] else []) in let files = Str.split (Str.regexp " +") target.toplevel_modules in let deps () = Dep.find ~pp:target.pp ~with_errors:true ~echo:false files in let etasks = List.map begin fun index -> let mktask = try List.assoc index external_tasks with Not_found -> assert false in mktask command end target.external_tasks in try List.iter begin fun compilation -> match get_output_name ~compilation ~outkind:target.target_type ~outname:target.output_name ~targets:files with | Some outname -> begin match command with | `Build -> build ~targets ~external_tasks ~etasks ~deps ~compilation ~outname ~files target | `Install_lib -> install_lib ~compilation ~outname ~external_tasks ~deps target | `Clean -> List.iter ETask.execute (ETask.filter etasks Before_clean); let deps = deps() in Oebuild.clean ~deps (); List.iter ETask.execute (ETask.filter etasks After_clean); | `Distclean -> if files <> [] then (Oebuild_util.remove_file ~verbose:false outname); | `Show | `Install | `Uninstall -> assert false end | _ -> () end compilation with Exit -> ()  and build ~targets ~external_tasks ~etasks ~deps ~compilation ~outname ~files target = let b_deps = find_target_dependencies targets target in List.iter (execute_target ~external_tasks ~targets ~command:`Build) b_deps; List.iter ETask.execute (ETask.filter etasks Before_compile); let deps = deps() in  match Oebuild.build ~compilation ~package:target.package ~includes:target.search_path ~libs:target.required_libraries ~other_mods:target.other_objects ~outkind:target.target_type ~compile_only:false ~thread:target.thread ~vmthread:target.vmthread ~annot:false ~pp:target.pp ?inline:target.inline ~cflags:target.compiler_flags ~lflags:target.linker_flags ~outname ~deps ~ms_paths:(ref false) ~targets:files () with | Built_successfully -> List.iter ETask.execute (ETask.filter etasks After_compile); | Build_failed n -> popd(); exit n ;;  let main ~cmd_line_args ~external_tasks ~general_commands ~targets = let module Command = struct type t = Build_script_command.t let find_args tag = try List.assoc tag cmd_line_args with Not_found -> [] let command tag = try let descr = snd (List.assoc tag general_commands) in [tag, (find_args tag), descr, ""] with Not_found -> [];; let command_install = command `Install let command_uninstall = command `Uninstall let string_of_command = function | `Install as c when command_install <> [] -> Build_script_command.string_of_command c | `Uninstall as c when command_uninstall <> [] -> Build_script_command.string_of_command c | `Install | `Uninstall -> assert false | x -> Build_script_command.string_of_command x;; let command_of_string = function | "install" when command_install <> [] -> `Install | "uninstall" when command_uninstall <> [] -> `Uninstall | ("install" | "uninstall") as c -> raise (Build_script_command.Unrecognized_command c) | x -> Build_script_command.command_of_string x;; let options = List.map (fun (a, b, c, d) -> a, Arg.align b, c, d) ([ `Build,       (find_args `Build), "Build libraries and executables (default command)", ""; ] @ command_install @ command_uninstall @ [ `Clean,       (find_args `Clean), "Remove output files for the selected target",       ""; `Distclean,   (find_args `Distclean), "Remove all build output",                           ""; `Install_lib, (find_args `Install_lib), "Install libraries as subdirectories relative\n               to the standard library directory", ""; `Show,        (find_args `Show), "Show the build options of a target",                ""; ]);; let anon_fun = function | `Show -> add_target targets | `Build -> add_target targets | `Install_lib -> add_target targets | `Clean -> add_target targets | (`Install | `Uninstall | `Distclean) as x -> fun arg -> kprintf failwith "Invalid anonymous argument `%s' for command `%s'" arg (string_of_command x);;  let execute command = pushd !Option.change_dir; let target = List.rev !target in try begin let execute_general_command command = try let index, descr = List.assoc command general_commands in let task = List.assoc index external_tasks in ETask.execute (task command) with Not_found -> () in match command with | `Distclean -> List.iter (fun (_, t) -> execute_target ~external_tasks ~targets ~command t) targets; Oebuild.distclean(); execute_general_command `Distclean; | (`Install | `Uninstall) as command -> execute_general_command command; | `Show -> printf "%s\n%!" (system_config ()); Printf.printf "\n%!" ; if target = [] then (raise (Arg.Bad "show: no target specified")); List.iter begin fun t -> show targets t; print_newline(); print_newline(); end target; | _ -> if target = [] then (raise (Arg.Bad (sprintf "%s: no target specified" (string_of_command command)))); List.iter (fun (_, (_, t)) -> execute_target ~external_tasks ~targets ~command t) target end; popd(); with Arg.Bad _ as ex -> popd(); raise ex | ex -> popd(); Printf.eprintf "File \"oebuild_script_util.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());; end in let module Argc = Argc.Make (Command) in let global_options = [ ("-C",      Set_string Option.change_dir, "<dir> Change directory before running [default: src]"); ] in let global_options = Arg.align global_options in let command_name = Filename.basename Sys.argv.(0) in  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 targets in let targets_shown = List.filter (fun (_, tg) -> tg.show) targets in let help_of_targets = String.concat "\n" (List.map begin fun (name, tg) -> let name = rpad name ' ' maxlength in sprintf "  %2d) %s %s, %s" tg.num name (string_of_output_type tg.target_type) (string_of_compilation_type (ccomp_type <> None) tg) end targets_shown) in let usage_msg = sprintf "\nUSAGE\n  ocaml %s [global-options*] <command> [command-options*] [targets*]\n  ocaml %s <command> -help" command_name command_name in let help_string () = sprintf "%s\n\nGLOBAL OPTIONS%s\nCOMMANDS%s\n\nTARGETS\n%s" usage_msg (Arg.usage_string global_options "") Argc.help_of_commands help_of_targets in try Argc.parse ~usage_msg ~global_options ~default_command:`Build Command.execute with | Arg.Help _ -> print_endline (help_string ()) | Arg.Bad msg -> prerr_endline msg | Argc.Help_Command (cmd, (specs, descr, usage), msg) -> let name = Command.string_of_command cmd in begin match cmd with | `Distclean -> printf "%s %s - %s\n\nUSAGE\n  ocaml %s [global_options*] %s\n\nOPTIONS%s" command_name name descr command_name name (Arg.usage_string specs "") | _ -> printf "%s %s - %s\n\nUSAGE\n  ocaml %s [global_options*] %s [options*] [targets*]\n\nOPTIONS%s" command_name name descr command_name name (Arg.usage_string specs "") end; | Build_script_command.Unrecognized_command msg -> prerr_endline msg | ex -> prerr_endline (Printexc.to_string ex) ;; end

open Oebuild
open Build_script_util
open Arg
open Task
open Printf

let arg_0_use_modified_gtkThread = ref false
let arg_1_prefix = ref "/usr/local"
let arg_2_prefix = ref "/usr/local"

let cmd_line_args = [
  `Build, [
    "-use-modified-gtkThread", Set arg_0_use_modified_gtkThread,
      " Set this flag if you have Lablgtk-2.14.2 or earlier\n                           and you want to use the included modified version of \n                           gtkThread.ml to reduce CPU consumption [default: Not Set]";
  ];
  `Install, [
    "-prefix", Set_string arg_1_prefix,
      " Installation prefix (Unix only) [default: /usr/local]";
  ];
  `Uninstall, [
    "-prefix", Set_string arg_2_prefix,
      " Uninstallation prefix (Unix only) [default: /usr/local]";
  ];
]

let external_tasks = [
  
  0, (fun command -> {
    et_name                  = "mkicons";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/mkicons.ml"];
    et_phase                 = Some Before_compile;
    et_always_run_in_project = true;
    et_always_run_in_script  = true;
  });
  
  1, (fun command -> {
    et_name                  = "prepare-build";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/prepare_build.ml"];
    et_phase                 = Some Before_compile;
    et_always_run_in_project = false;
    et_always_run_in_script  = true;
  });
  
  2, (fun command -> {
    et_name                  = "prepare-build";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/prepare_build.ml"; command = `Build && !arg_0_use_modified_gtkThread, "-use-modified-gtkThread"];
    et_phase                 = Some Before_compile;
    et_always_run_in_project = true;
    et_always_run_in_script  = true;
  });
  
  3, (fun command -> {
    et_name                  = "<tools>";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "";
    et_cmd                   = "echo";
    et_args                  = [];
    et_phase                 = Some Compile;
    et_always_run_in_project = true;
    et_always_run_in_script  = false;
  });
  
  4, (fun command -> {
    et_name                  = "mkrelease";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/mkrelease.ml"];
    et_phase                 = Some Before_clean;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
  
  5, (fun command -> {
    et_name                  = "mkversion";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "../tools";
    et_cmd                   = "ocaml";
    et_args                  = [true,"mkversion.ml"; true,"1.8.0"];
    et_phase                 = Some Before_clean;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
  
  6, (fun command -> {
    et_name                  = "generate_oebuild_script";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/prepare_build.ml"; true,"-generate-oebuild-script"];
    et_phase                 = Some Before_clean;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
  
  7, (fun command -> {
    et_name                  = "Install OCamlEditor";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/install.ml"; command = `Install, (sprintf "-prefix %S" !arg_1_prefix)];
    et_phase                 = Some Before_clean;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
  
  8, (fun command -> {
    et_name                  = "Uninstall OCamlEditor";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/uninstall.ml"; command = `Uninstall, (sprintf "-prefix %S" !arg_2_prefix)];
    et_phase                 = Some Before_clean;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
  
  9, (fun command -> {
    et_name                  = "distclean";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/distclean.ml"];
    et_phase                 = None;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
];;


let general_commands = [
  `Distclean, (9, "distclean");
  `Install, (7, "Install OCamlEditor");
  `Uninstall, (8, "Uninstall OCamlEditor");
]


(* Targets ==================================================== *)

let targets = [
  
  (* 0 *)
  "common", {
    num                  = 0;
    id                   = 4;
    output_name          = "common/common";
    target_type          = Library;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "common/common.ml";
    package              = "";
    search_path          = "common"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-g";
    linker_flags         = "-g";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
  
  (* 0 *)
  "icons", {
    num                  = 0;
    id                   = 10;
    output_name          = "icons";
    target_type          = Library;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "icons/icons.ml";
    package              = "lablgtk2";
    search_path          = "common"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-g";
    linker_flags         = "-g";
    thread               = false;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [0];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
  
  (* 0 *)
  "oebuildlib", {
    num                  = 0;
    id                   = 7;
    output_name          = "oebuildlib";
    target_type          = Library;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "oebuild/oebuild.ml";
    package              = "";
    search_path          = "common oebuild"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-w y";
    linker_flags         = "";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
  
  (* 1 *)
  "oebuild", {
    num                  = 1;
    id                   = 5;
    output_name          = "oebuild/oebuild";
    target_type          = Executable;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "oebuild/oebuild_tool.ml";
    package              = "unix,str";
    search_path          = "common oebuild"; (* -I *)
    required_libraries   = "common";
    compiler_flags       = "-w y";
    linker_flags         = "";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [7];
    show                 = true;
  };
  
  (* 2 *)
  "oeproc", {
    num                  = 2;
    id                   = 6;
    output_name          = "oeproc/oeproc";
    target_type          = Executable;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "oeproc/oeproc.ml";
    package              = "unix,str";
    search_path          = "common oeproc"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-w s";
    linker_flags         = "";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = ["IS_WIN32"];
    dependencies         = [];
    show                 = true;
  };
  
  (* 2 *)
  "gmisclib", {
    num                  = 0;
    id                   = 8;
    output_name          = "gmisclib";
    target_type          = Library;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "gmisclib/gmisclib.ml";
    package              = "lablgtk2";
    search_path          = "gmisclib"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-g";
    linker_flags         = "-g";
    thread               = false;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    library_install_dir  = "gmisclib"; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
  
  (* 2 *)
  "otherwidgets", {
    num                  = 0;
    id                   = 9;
    output_name          = "otherwidgets";
    target_type          = Library;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "otherwidgets/otherwidgets.ml";
    package              = "lablgtk2";
    search_path          = "icons common otherwidgets"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-w sy -g";
    linker_flags         = "-w sy -g";
    thread               = false;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
  
  (* 3 *)
  "ocamleditor-bytecode", {
    num                  = 3;
    id                   = 0;
    output_name          = "ocamleditor";
    target_type          = Executable;
    compilation_bytecode = true;
    compilation_native   = false;
    toplevel_modules     = "ocamleditor.ml";
    package              = "str,lablgtk2,compiler-libs";
    search_path          = "+ocamldoc +xml-light gmisclib common icons otherwidgets oebuild "; (* -I *)
    required_libraries   = "ocamlcommon dynlink odoc_info xml-light gmisclib common icons otherwidgets oebuildlib";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [1];
    restrictions         = [];
    dependencies         = [4; 10; 7; 5; 6; 8; 9];
    show                 = true;
  };
  
  (* 4 *)
  "ocamleditor", {
    num                  = 4;
    id                   = 12;
    output_name          = "ocamleditor";
    target_type          = Executable;
    compilation_bytecode = false;
    compilation_native   = true;
    toplevel_modules     = "ocamleditor.ml";
    package              = "lablgtk2,str,compiler-libs";
    search_path          = "+ocamldoc +xml-light gmisclib common icons otherwidgets oebuild "; (* -I *)
    required_libraries   = "ocamlcommon dynlink odoc_info xml-light gmisclib common icons otherwidgets oebuildlib";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = Some 50;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [2];
    restrictions         = [];
    dependencies         = [4; 10; 7; 5; 6; 8; 9];
    show                 = true;
  };
  
  (* 4 *)
  "ocamleditor-native", {
    num                  = 0;
    id                   = 11;
    output_name          = "ocamleditor";
    target_type          = Executable;
    compilation_bytecode = false;
    compilation_native   = true;
    toplevel_modules     = "ocamleditor.ml";
    package              = "lablgtk2,str,compiler-libs";
    search_path          = "+ocamldoc +xml-light gmisclib common icons otherwidgets oebuild "; (* -I *)
    required_libraries   = "ocamlcommon dynlink odoc_info xml-light gmisclib common icons otherwidgets oebuildlib";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = Some 50;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [4; 10; 7; 5; 6; 8; 9];
    show                 = false;
  };
  
  (* 4 *)
  "tools", {
    num                  = 0;
    id                   = 13;
    output_name          = "";
    target_type          = Executable;
    compilation_bytecode = true;
    compilation_native   = false;
    toplevel_modules     = "";
    package              = "";
    search_path          = ""; (* -I *)
    required_libraries   = "";
    compiler_flags       = "";
    linker_flags         = "";
    thread               = false;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [3; 4; 5; 6; 7; 8; 9];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
];;

(* End of Targets ============================================= *)

let _ = main ~cmd_line_args ~external_tasks ~general_commands ~targets
