#directory "+threads" #load "str.cma" #load "unix.cma" #load "threads.cma"
let split re = Str.split (Str.regexp re)
module Quote = struct let path = if Sys.os_type = "Win32" then (fun x -> Filename.quote (Filename.quote x)) else (fun x -> x) let arg = if Sys.os_type = "Win32" then (fun x -> Filename.quote x) else (fun x -> x) end
module Argc = struct type command_descr = string type command_usage = string type speclist = (Arg.key * Arg.spec * Arg.doc) list module type COMMAND = sig type t val string_of_command : t -> string val command_of_string : string -> t val options : (t * speclist * command_descr * command_usage) list val anon_fun : t -> string -> unit end module Make (C : COMMAND) = struct open Printf exception Command_found exception Help_Command of C.t * (speclist * command_descr * command_usage) * string let command : C.t option ref = ref None;; let commands, cmd_map = List.fold_left (fun (x, y) (a, b, c, d) -> a::x, (a, (b, c, d))::y) ([], []) C.options let rpad txt c width = let result = txt ^ (String.make width c) in String.sub result 0 width;; let help_of_commands = let help_of_command maxlength cmd = let spec , descr, _ = List.assoc cmd cmd_map in (sprintf "  %s  %s" (rpad (C.string_of_command cmd) ' ' maxlength) descr) in let maxlength = List.fold_left (fun cand x -> max cand (String.length (C.string_of_command x))) 0 commands in "\n" ^ (String.concat "\n" (List.map (help_of_command maxlength) (List.rev commands))) let create_help_msg global_speclist usage_msg = sprintf "%s\n\nGLOBAL OPTIONS%s\nCOMMANDS%s\n" usage_msg (Arg.usage_string global_speclist "") help_of_commands;; let with_command f = match !command with Some cmd -> f cmd | _ -> assert false;; let parse_argv args ~(global_options : speclist) ?default_command ?(usage_msg=sprintf "\nUSAGE\n  %s [global_options*] <command> [options*] [args*]\n  %s <command> --help" (Filename.basename args.(0)) (Filename.basename args.(0))) execute_command = command := None; Arg.current := 0; let parse_anon arg = match !command with | None -> let cmd = try C.command_of_string arg with ex -> decr Arg.current; (match default_command with Some c -> c | _ -> raise ex) in command := Some cmd; raise Command_found | _ -> assert false in let help_string () = match !command with | Some cmd -> let spec, descr, _ = List.assoc cmd cmd_map in let cmd = C.string_of_command cmd in Arg.usage_string spec (sprintf "%s %s - %s\n\nUSAGE\n  %s [global_options*] %s [options*] [args*]\n\nOPTIONS" (Filename.basename args.(0)) cmd descr (Filename.basename args.(0)) cmd) | _ -> create_help_msg global_options usage_msg in if Array.length args = 1 then (raise (Arg.Help (help_string()))); try Arg.parse_argv args global_options parse_anon usage_msg; with | Command_found -> let len = Array.length args - !Arg.current in let command_args = Array.create len "" in Array.blit args !Arg.current command_args 0 len; let parse_anon = with_command (fun cmd -> C.anon_fun cmd) in begin try Arg.current := 0; let speclist = with_command (fun cmd -> let sp, _, _ = try List.assoc cmd cmd_map with Not_found -> assert false in sp) in Arg.parse_argv command_args speclist parse_anon usage_msg; let f = function | Some cmd -> execute_command cmd | None -> raise (Arg.Help (create_help_msg global_options usage_msg)) in f !command with | Arg.Help _ -> with_command begin fun cmd -> let cmd_specs, cmd_descr, cmd_usage = List.assoc cmd cmd_map in raise (Help_Command  (cmd, (cmd_specs, cmd_descr, cmd_usage), help_string ())) end | Arg.Bad msg as ex -> with_command begin fun cmd -> try let first_line = String.sub msg 0 (String.index msg '\n') in raise (Arg.Bad (sprintf "%s\n%s" first_line (help_string()))) with Not_found -> raise ex end end; | Arg.Bad _ -> raise (Arg.Bad (sprintf "unknown global option `%s'\n%s" (args.(!Arg.current)) (help_string()))) | Arg.Help _ -> raise (Arg.Help (help_string()));; let parse ~global_options ?default_command ?usage_msg f = parse_argv Sys.argv ~global_options ?default_command ?usage_msg f;; end;; end
module Cmd = struct open Printf let expand = let trimfunc = let replace = Str.global_replace (Str.regexp "\\(^[ \t\r\n]+\\)\\|\\([ \t\r\n]+$\\)") in fun str -> replace "" str in fun ?(trim=true) ?(first_line=false) ?filter command -> let ichan = Unix.open_process_in command in let finally () = ignore (Unix.close_process_in ichan) in let data = Buffer.create 100 in begin try let get_line ichan = if trim then trimfunc (input_line ichan) else input_line ichan in while true do let line = get_line ichan in if first_line && String.length line = 0 then begin end else if first_line then begin Buffer.add_string data line; raise End_of_file end else begin match filter with | None -> Buffer.add_string data line; Buffer.add_char data '\n' | Some f when (f line) -> Buffer.add_string data line; Buffer.add_char data '\n' | _ -> () end done with | End_of_file -> () | ex -> (finally(); raise ex) end; finally(); if Buffer.length data = 0 then (kprintf failwith "Cmd.expand: %s" command); Buffer.contents data;;  let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null"  let exec_lines command = let ch = Unix.open_process_in command in set_binary_mode_in ch false; let result = ref [] in try while true do result := (input_line ch) :: !result; done; assert false with End_of_file -> begin ignore (Unix.close_process_in ch); List.rev !result end | e -> begin ignore (Unix.close_process_in ch); raise e end end
module App_config = struct let (//) = Filename.concat let (!!) = Filename.dirname let application_param = try List.fold_left begin fun acc x -> match Str.split (Str.regexp "=") x with | n :: v :: [] -> (n, v) :: acc | n :: [] -> (n, "") :: acc | _ -> acc end [] (Str.split (Str.regexp ",") (Sys.getenv "OCAMLEDITORPARAM")) with Not_found -> [];; let application_debug = try (List.assoc "debug" application_param) = "2" with Not_found -> false;; let get_application_dir name = let is_app_in_cwd = !! Sys.executable_name = "." in let prefix = if is_app_in_cwd then ".." else !! (!! Sys.executable_name) in let path = prefix // name in if Sys.file_exists path && (Sys.is_directory path) then path else let install_path = prefix // "share" // "ocamleditor" // name in install_path let application_icons = get_application_dir "icons" let application_plugins = get_application_dir "plugins"  let get_locale () = try if Sys.win32 then begin let lines = Cmd.exec_lines "reg query \"hkcu\\Control Panel\\International\" /v LocaleName" in let lines = List.map String.trim lines in let locale = List.find (fun l -> Str.string_match (Str.regexp "LocaleName.+") l 0) lines in Str.string_match (Str.regexp ".*[\t ]\\([a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-]\\)") locale 0 |> ignore; Some (Str.matched_group 1 locale) end else begin let lines = Cmd.exec_lines "locale" in let locale = List.find (fun l -> Str.string_match (Str.regexp ".*=.+") l 0) lines in Str.string_match (Str.regexp ".*=\\(.*\\)") locale 0 |> ignore; Some (Str.matched_group 1 locale) end with ex -> Printf.eprintf "File \"miscellanea.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace()); None end
module Ocaml_config = struct open Printf let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null" let putenv_ocamllib value = match Sys.os_type with | "Win32" -> let value = match value with None -> "" | Some x -> x in Unix.putenv "OCAMLLIB" value | _ -> ignore (Sys.command "unset OCAMLLIB") let find_best_compiler compilers = try Some (List.find begin fun comp -> try ignore (kprintf Cmd.expand "%s -version%s" comp redirect_stderr); true with _ -> false end compilers) with Not_found -> None;; let find_tool which path = let commands = match which with | `BEST_OCAMLC -> ["ocamlc.opt"; "ocamlc"] | `BEST_OCAMLOPT -> ["ocamlopt.opt"; "ocamlopt"] | `BEST_OCAMLDEP -> ["ocamldep.opt"; "ocamldep"] | `BEST_OCAMLDOC -> ["ocamldoc.opt"; "ocamldoc"] | `OCAMLC -> ["ocamlc"] | `OCAML -> ["ocaml"] in let quote    = if path <> "" && Sys.os_type = "Win32" && String.contains path ' ' then Filename.quote else (fun x -> x) in let path     = if path <> "" then Filename.concat path "bin" else "" in find_best_compiler (List.map quote (List.map (Filename.concat path) commands)) let get_home () = try Sys.getenv "OCAML_HOME" with Not_found -> "" let expand_includes = let split = Str.split (Str.regexp " +") in fun compact -> if String.length compact > 0 then ("-I " ^ (String.concat " -I " (split compact))) else ""  let ocamlc ()   = match find_tool `BEST_OCAMLC (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocamlc'" let ocamlopt () = find_tool `BEST_OCAMLOPT (get_home ()) let ocamldep () = match find_tool `BEST_OCAMLDEP (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocamldep'" let ocamldoc () = match find_tool `BEST_OCAMLDOC (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocamldoc'" let ocaml ()    = match find_tool `OCAML (get_home ()) with Some x -> x | _ -> failwith "Cannot find 'ocaml'" let ocamllib () = Cmd.expand ~first_line:true ((ocamlc()) ^ " -where")  let ocaml_version ?(compiler=ocamlc()) () = Cmd.expand (compiler ^ " -v " ^ redirect_stderr)  let can_compile_native ?ocaml_home () = let result = ref false in let filename = Filename.temp_file "test_native" ".ml" in let ochan = open_out filename in begin try output_string ochan ("0"); close_out ochan with _ -> (close_out ochan) end; let outname = Filename.chop_extension filename in let exename = outname ^ (if Sys.os_type = "Win32" then ".exe" else "") in let compiler = match ocaml_home with | Some home -> find_tool `BEST_OCAMLOPT home | _ -> Some "ocamlopt" in match compiler with | Some compiler -> let cmd = sprintf "%s -o %s %s%s" compiler exename filename (if App_config.application_debug then redirect_stderr else "") in result := (Sys.command cmd) = 0; if Sys.file_exists filename then (Sys.remove filename); if Sys.file_exists exename then (Sys.remove exename); let cmi = outname ^ ".cmi" in if Sys.file_exists cmi then (Sys.remove cmi); let cmx = outname ^ ".cmx" in if Sys.file_exists cmx then (Sys.remove cmx); let obj = outname ^ ".o" in if Sys.file_exists obj then (Sys.remove obj); let obj = outname ^ ".obj" in if Sys.file_exists obj then (Sys.remove obj); if Sys.win32 then begin let manifest = exename ^ ".manifest" in if Sys.file_exists manifest then (Sys.remove manifest); end; if !result then begin let conf = kprintf Cmd.expand "%s -config" compiler in let re = Str.regexp "ccomp_type: \\(.*\\)\n" in if Str.search_forward re conf 0 >= 0 then begin Some (Str.matched_group 1 conf) end else Some "<unknown ccomp_type>" end else None; | _ -> None ;; end
module Cmd_line_args = struct type state = StartArg | InUnquotedArg | InQuotedArg | InQuotedArgAfterQuote;; let format = String.concat " ";; let parse line = let args = ref [] in let buf = Buffer.create 10 in let state = ref StartArg in let start_arg () = state := StartArg; args := (Buffer.contents buf) :: !args; Buffer.clear buf; in String.iter begin function | (' ' as ch) when !state = InQuotedArg -> Buffer.add_char buf ch | ' ' when !state = StartArg -> () | ' ' when !state = InUnquotedArg -> start_arg (); | ' ' -> start_arg () | ('"' as ch) when !state = StartArg -> state := InQuotedArg; Buffer.add_char buf ch | ('"' as ch) when !state = InQuotedArg -> Buffer.add_char buf ch; start_arg (); | ('"' as ch) when !state = InQuotedArgAfterQuote -> Buffer.add_char buf ch; state := InQuotedArg; | ('"' as ch) when !state = InUnquotedArg -> start_arg (); Buffer.add_char buf ch; state := InQuotedArg; | ('\\' as ch) when !state = InQuotedArg -> state := InQuotedArgAfterQuote; Buffer.add_char buf ch | ch when !state = InQuotedArgAfterQuote -> state := InQuotedArg; Buffer.add_char buf ch; | ch when !state = StartArg -> state := InUnquotedArg; Buffer.add_char buf ch; | ch -> Buffer.add_char buf ch; end line; if Buffer.length buf > 0 then (start_arg ()); List.rev !args;;  end
module Task = struct type kind = [ `CLEAN | `CLEANALL | `ANNOT | `COMPILE | `RUN | `OTHER] type phase = Before_clean | Clean | After_clean | Before_compile | Compile | After_compile type t = { mutable et_name                  : string; mutable et_env                   : (bool * string) list; mutable et_env_replace           : bool;                                mutable et_dir                   : string;                                                                                            mutable et_cmd                   : string; mutable et_args                  : (bool * string) list; mutable et_phase                 : phase option; mutable et_always_run_in_project : bool; mutable et_always_run_in_script  : bool; } let string_of_phase = function | Before_clean -> "Before_clean" | Clean -> "Clean" | After_clean -> "After_clean" | Before_compile -> "Before_compile" | Compile -> "Compile" | After_compile -> "After_compile" let descr_of_phase = function | Before_clean -> "Pre-clean" | Clean -> "Clean" | After_clean -> "Post-clean" | Before_compile -> "Pre-build" | Compile -> "Build" | After_compile -> "Post-build" let phase_of_string = function | "Before_clean" -> Before_clean | "Clean" -> Clean | "After_clean" -> After_clean | "Before_compile" -> Before_compile | "Compile" -> Compile | "After_compile" -> After_compile | _ -> failwith "phase_of_string" let create ~name ~env ?(env_replace=false) ~dir ~cmd ~args ?phase () = { et_name                  = name; et_env                   = env; et_env_replace           = env_replace; et_dir                   = dir; et_cmd                   = cmd; et_args                  = args; et_phase                 = phase; et_always_run_in_project = false; et_always_run_in_script  = true; }  let handle f task = let tenv = Array.of_list task.et_env in let env = if task.et_env_replace then Array.concat [                        tenv] else (Array.concat [tenv                       ; (Array.map (fun e -> true, e) (Unix.environment()))]) in let env = List.filter (fun (e, _) -> e) (Array.to_list env) in let env = Array.of_list (List.map (fun (_, v) -> v) env) in let prog = task.et_cmd in let dir = if task.et_dir <> "" then task.et_dir else (Sys.getcwd ()) in let args = List.filter (fun (e, _) -> e) task.et_args in let args = List.flatten (List.map (fun (_, v) -> Cmd_line_args.parse v) args) in f ~env ~dir ~prog ~args;; end
module Build_script_command = struct open Printf type t = [`Show | `Build | `Install | `Uninstall | `Install_lib | `Clean | `Distclean] let commands = [`Show; `Build; `Install; `Uninstall; `Install_lib; `Clean; `Distclean] exception Unrecognized_command of string let string_of_command = function | `Show -> "show" | `Build -> "build" | `Install -> "install" | `Uninstall -> "uninstall" | `Install_lib -> "install-lib" | `Clean -> "clean" | `Distclean -> "distclean" let command_of_string = function | "show" -> `Show | "build" -> `Build | "install" -> `Install | "uninstall" -> `Uninstall | "install-lib" -> `Install_lib | "clean" -> `Clean | "distclean" -> `Distclean | x -> raise (Unrecognized_command (sprintf "`%s' is not a recognized command." x));; let code_of_command = function | `Show -> "`Show" | `Build -> "`Build" | `Install -> "`Install" | `Uninstall -> "`Uninstall" | `Install_lib -> "`Install_lib" | `Clean -> "`Clean" | `Distclean -> "`Distclean" end
module Oebuild_util = struct open Printf let (!$) = Filename.chop_extension let (//) = Filename.concat let (^^) = Filename.check_suffix let (<@) = List.mem let is_win32, win32 = (Sys.os_type = "Win32"), (fun a b -> match Sys.os_type with "Win32" -> a | _ -> b) let may opt f = match opt with Some x -> f x | _ -> () let re_spaces = Str.regexp " +" let redirect_stderr_to_null = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null"  let crono ?(label="Time") f x = let finally time = Printf.fprintf stdout "[CRONO] %s: %f sec." label (Unix.gettimeofday() -. time); print_newline(); in let time = Unix.gettimeofday() in let result = try f x with e -> begin finally time; raise e end in finally time; result  let remove_dupl l = List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)  let remove_file ?(verbose=false) filename = try if Sys.file_exists filename then (Sys.remove filename; if verbose then print_endline filename) with Sys_error ex -> eprintf "%s\n%!" ex  let command ?(echo=true) cmd = let cmd = Str.global_replace re_spaces " " cmd in if echo then (printf "%s\n%!" cmd); let exit_code = Sys.command cmd in Pervasives.flush stderr; Pervasives.flush stdout; exit_code  let iter_chan chan f = try while true do f chan done with End_of_file -> ()  let exec ?(env=Unix.environment()) ?(verbose=true) ?(join=true) ?at_exit ?(process_in=(fun chan -> print_endline (input_line chan))) ?(process_err=(fun chan -> prerr_endline (input_line chan))) cmd = let cmd = Str.global_replace re_spaces " " cmd in if verbose then printf "%s\n%!" cmd; let exit_code = ref (-9998) in let (inchan, _, errchan) as channels = Unix.open_process_full cmd env in let close () = match Unix.close_process_full channels with 	| Unix.WEXITED code -> code 	| _ -> (-9997) in let thi = Thread.create begin fun () -> iter_chan inchan process_in; end () in let the = Thread.create begin fun () -> iter_chan errchan process_err; Thread.join thi; begin match at_exit with | None -> () | Some f -> exit_code := close(); f !exit_code end; end () in if join then begin Thread.join thi; Thread.join the; end; if at_exit = None then Some (close()) else None;;  let rm = win32 "DEL /F /Q" "rm -f"  let copy_file ic oc = let buff = String.create 0x1000 in let rec copy () = let n = input ic buff 0 0x1000 in if n = 0 then () else (output oc buff 0 n; copy()) in copy() let cp ?(echo=true) src dst = let ic = open_in_bin src in let oc = open_out_bin dst in if echo then (printf "%s -> %s\n%!" src dst); let finally () = close_out oc; close_in ic in try copy_file ic oc; finally() with ex -> (finally(); raise ex)  let rec mkdir_p ?(echo=true) d = if not (Sys.file_exists d) then begin mkdir_p (Filename.dirname d); printf "mkdir -p %s\n%!" d; (Unix.mkdir d 0o755) end   let replace_extension_to_ml filename = if Filename.check_suffix filename ".cmx" then (Filename.chop_extension filename) ^ ".ml" else if Filename.check_suffix filename ".cmi" then (Filename.chop_extension filename) ^ ".mli" else filename ;;  let get_effective_command = let re_verbose = Str.regexp " -verbose" in fun ?(linkpkg=false) ocamlfind -> try let cmd = sprintf "%s%s -verbose %s" ocamlfind (if linkpkg then " -linkpkg" else "") redirect_stderr_to_null in let lines = Cmd.exec_lines cmd in let effective_compiler = List.find (fun line -> String.sub line 0 2 = "+ ") lines in let effective_compiler = Str.string_after effective_compiler 2  in let effective_compiler = Str.replace_first re_verbose "" effective_compiler in effective_compiler with Not_found -> ocamlfind ;;  type parfold_entry = { pf_cmd         : string; pf_out         : Buffer.t; pf_err         : Buffer.t; pf_process_in  : (in_channel -> unit); pf_process_err : (in_channel -> unit); }  let parfold_command ~command ~args ?verbose () = let finished = Condition.create() in let mx_nargs = Mutex.create () in let mx_finished = Mutex.create () in let nargs = ref (List.length args) in let write buf chan = Buffer.add_string buf (input_line chan); Buffer.add_char buf '\n'; in let entries = List.map begin fun arg -> let out = Buffer.create 10 in let err = Buffer.create 10 in { pf_cmd         = sprintf "%s %s" command arg; pf_out         = out; pf_err         = err; pf_process_in  = write out; pf_process_err = write err; } end args in let at_exit exit_code = Mutex.lock mx_nargs; decr nargs; Mutex.unlock mx_nargs; Mutex.lock mx_finished; if !nargs = 0 then Condition.signal finished; Mutex.unlock mx_finished; in List.iter begin fun entry -> exec ?env:None ?verbose ~join:false ~at_exit ~process_in:entry.pf_process_in ~process_err:entry.pf_process_err entry.pf_cmd |> ignore end entries; Mutex.lock mx_finished; while !nargs > 0 do Condition.wait finished mx_finished done; Mutex.unlock mx_finished; entries ;; end
module Oebuild_table = struct open Printf  type t = (string, float) Hashtbl.t let oebuild_times_filename = ".oebuild" let (^^) filename opt = filename ^ (if opt then ".opt" else ".byt") let find (table : t) filename opt = Hashtbl.find table (filename ^^ opt) let add (table : t) filename opt = Hashtbl.add table (filename ^^ opt) let remove (table : t) filename opt = Hashtbl.remove table (filename ^^ opt)  let read () = if not (Sys.file_exists oebuild_times_filename) then begin let ochan = open_out_bin oebuild_times_filename in Marshal.to_channel ochan (Hashtbl.create 7) []; close_out ochan end; let ichan = open_in_bin oebuild_times_filename in let times = Marshal.from_channel ichan in close_in ichan; (times : t)  let write (times : t) = if Hashtbl.length times > 0 then begin let ochan = open_out_bin oebuild_times_filename in Marshal.to_channel ochan times []; close_out ochan end  let update = let get_last_compiled_time ~opt cache filename = try let time = find cache filename opt in let ext = if opt then "cmx" else "cmo" in let cm = sprintf "%s.%s" (Filename.chop_extension filename) ext in if Sys.file_exists cm then time else begin remove cache filename opt; raise Not_found end with Not_found -> 0.0 in fun ~opt (cache : t) filename -> let ctime = get_last_compiled_time ~opt cache filename in if ctime > 0.0 && ((Unix.stat filename).Unix.st_mtime) >= ctime then begin remove cache filename opt; true end else ctime = 0.0 ;; end
module Oebuild_dag = struct open Printf module type ENTRY = sig type key type t val equal : t -> t -> bool val hash : t -> int val to_string : t -> string end module Make (Entry : ENTRY) = struct type t = (Entry.key, entry) Hashtbl.t and entry = { key                  : Entry.key; node                 : Entry.t; mutable dependants   : entry list; mutable dependencies : entry list; } let length = Hashtbl.length let set_dependants (dag : t) = Hashtbl.iter begin fun _ entry -> List.iter begin fun node -> node.dependants <- entry :: node.dependants end entry.dependencies end dag let get_leaves : t -> entry list = fun dag -> Hashtbl.fold begin fun _ entry acc -> if entry.dependencies = [] then entry :: acc else acc end dag [];; let remove_leaf : t -> entry -> unit = fun dag leaf -> if Hashtbl.mem dag leaf.key then if leaf.dependencies <> [] then failwith "Not a leaf" else begin Hashtbl.iter begin fun _ entry -> entry.dependencies <- List.filter (fun d -> d.key <> leaf.key) entry.dependencies; end dag; Hashtbl.remove dag leaf.key; end;; end end
module Oebuild_dep = struct open Printf type ocamldeps = (string, bool * string list) Hashtbl.t exception Loop_found of string let re1 = Str.regexp " ?:\\( \\|$\\)" let re3 = Str.regexp " " let split_nl = Str.split (Str.regexp "\n")  let ocamldep_command ?pp ?(slash=true) ?(search_path="") () = sprintf "%s%s %s -native -one-line %s" (Ocaml_config.ocamldep()) (match pp with Some pp when pp <> "" -> " -pp " ^ pp | _ -> "" ) search_path (if slash then "-slash" else "");;  let ocamldep ?times ?pp ?(ignore_stderr=false) ?(verbose=false) ?slash ?search_path filenames = let table : ocamldeps = Hashtbl.create 7 in if String.trim filenames <> "" then begin let redirect_stderr = if ignore_stderr then Oebuild_util.redirect_stderr_to_null else "" in let cmd = ocamldep_command ?pp ?slash ?search_path () in let cmd = sprintf "%s %s %s" cmd filenames redirect_stderr in if verbose then (printf "%s\n%!" cmd); let text = Cmd.expand cmd in let entries = split_nl text in let replace = match times with | Some (times, opt) ->  fun table key data -> let ml = Oebuild_util.replace_extension_to_ml key in let changed = Oebuild_table.update ~opt times ml in Hashtbl.replace table key (changed, data) | _ -> fun table key data -> Hashtbl.replace table key (true, data) in let open! Oebuild_util in List.iter begin fun entry -> match Str.split re1 entry with | key :: _ when key ^^ ".cmo" -> () | key :: [] -> replace table key [] | [key; deps] -> let deps = Str.split re3 deps in replace table key deps | _ -> eprintf "%s\n%s\n%!" cmd entry; assert false end entries; end; table;;  let ocamldep_toplevels ?times ?pp ?ignore_stderr ?verbose ?slash ?(search_path="") toplevel_modules = let search_path, filenames = List.fold_left begin fun (sp, fn) x -> let dir = Filename.dirname x in if dir = "." then sp, ("*.ml *.mli" :: fn) else (" -I " ^ dir) :: sp, (sprintf "%s/*.ml %s/*.mli" dir dir) :: fn end ([search_path], []) toplevel_modules in let search_path = String.concat "" (Oebuild_util.remove_dupl search_path) in let filenames = String.concat " " (Oebuild_util.remove_dupl filenames) in ocamldep ?times ?pp ?ignore_stderr ~search_path ?verbose ?slash filenames  let ocamldep_recursive ?times ?pp ?(ignore_stderr=false) ?(verbose=false) ?slash ?search_path toplevel_modules = let dag : ocamldeps = Hashtbl.create 17 in let rec loop ~toplevel_modules = let filenames = String.concat " " toplevel_modules in let ocamldeps = ocamldep ?times ?pp ~ignore_stderr ~verbose ?slash ?search_path filenames in let new_tops = Hashtbl.fold begin fun key (changed, deps) acc -> Hashtbl.add dag key (changed, deps); List.rev_append deps acc end ocamldeps [] in let new_tops = List.filter (fun tl -> not (Hashtbl.mem dag tl)) new_tops in let new_tops = Oebuild_util.remove_dupl new_tops in let new_tops = List.map Oebuild_util.replace_extension_to_ml new_tops in if new_tops <> [] then loop ~toplevel_modules:new_tops in loop ~toplevel_modules; dag  let sort_dependencies (dag : ocamldeps) = let dag = Hashtbl.copy dag in let get_leaves dag = Hashtbl.fold begin fun key (_            , deps) acc -> let deps = List.filter (Hashtbl.mem dag) deps in if deps = [] then key :: acc else acc end dag [] in let rec loop res = match get_leaves dag with | [] -> res | leaves -> List.iter (Hashtbl.remove dag) leaves; loop (List.rev_append leaves res); in List.rev (Oebuild_util.remove_dupl (loop [])) ;;  let find_dependants = let re = Str.regexp "\\(.+\\.mli?\\) ?: ?\\(.*\\)" in let re1 = Str.regexp "\r?\n" in let re2 = Str.regexp " " in fun ~dirname ~modname ->  let dir = if dirname = Filename.current_dir_name then "" else (dirname ^ "/") in let cmd = sprintf "%s -modules -native %s*.ml %s*.mli%s" (Ocaml_config.ocamldep()) dir dir Oebuild_util.redirect_stderr_to_null in printf "%s (%s)\n%!" cmd modname; let ocamldep = Cmd.expand cmd in let entries = Str.split re1 ocamldep in let entries = List.map begin fun entry -> if Str.string_match re entry 0 then begin let filename = Str.matched_group 1 entry in let modules = Str.matched_group 2 entry in (filename, (Str.split re2 modules)) end else (assert false) end entries in let dependants = ref [] in let rec loop modname = List.iter begin fun (filename, modules) -> if List.mem modname modules then begin if not (List.mem filename !dependants) then begin dependants := filename :: !dependants; let prefix = Filename.chop_extension filename in let prefix_mli = prefix ^ ".mli" in if List.mem_assoc prefix_mli entries then (dependants := prefix_mli :: !dependants;); let mdep = String.capitalize prefix in ignore (loop mdep); end end end entries; !dependants in loop modname  let find_dependants ~path ~modname = let dependants = List.map (fun dirname -> find_dependants ~dirname ~modname) path in List.flatten dependants ;;   let find_dep ?pp ?(ignore_stderr=false) ?(echo=true) target = let dir = Filename.dirname target in let filenames = (match dir with "." -> "*.mli" | _ -> dir ^ "/" ^ "*.mli *.mli") ^ " " ^ (match dir with "." -> "*.ml" | _ -> dir ^ "/" ^ "*.ml *.ml") in let search_path = Ocaml_config.expand_includes dir in let table = ocamldep ?pp ~ignore_stderr ~verbose:echo ~search_path filenames in let target = (Filename.chop_extension target) ^ ".cmx" in let anti_loop = ref [] in let result = ref [] in let rec find_chain target = if (List.mem target !anti_loop) && (not (List.mem target !result)) then (raise (Loop_found (String.concat " " (List.map Oebuild_util.replace_extension_to_ml (target :: !anti_loop))))); anti_loop := target :: (List.filter ((<>) target) !anti_loop); try if not (List.mem target !result) then begin match Hashtbl.find table target with | (_, []) -> result := target :: !result; | (_, deps) -> List.iter find_chain deps; result := target :: !result; end with Not_found ->  (kprintf failwith "Dep: %s" target) in find_chain target; List.rev (                                     !result) ;;  let find ?pp ?ignore_stderr ?(echo=true) targets = let deps = List.map (find_dep ?pp ?ignore_stderr ~echo) targets in let deps = List.flatten deps in List.rev (List.fold_left begin fun acc x -> if not (List.mem x acc) then x :: acc else acc end [] deps) ;;   end
module Oebuild_dep_dag = struct                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 open Oebuild_util open Printf type t = (string, string list) Hashtbl.t type dag_option = Dag of t * Oebuild_dep.ocamldeps | Cycle of string list exception Cycle_exception of string list  let array_exists from p a = try for i = from to Array.length a - 1 do if p a.(i) then raise Exit done; false with Exit -> true  let reduce : t -> unit = function table -> let rec (<-?-) x y = let deps = try Hashtbl.find table y with Not_found -> [] in (List.mem x deps) || (List.exists ((<-?-) x) deps) in let is_descendant =                              (<-?-) in let reduce ll = let stop = ref "" in let rec reduce' ll = let len = Array.length ll in if len <= 1 then ll else let fst = ll.(0) in if fst = !stop then ll else begin let len = len - 1 in if array_exists 1 (is_descendant fst) ll then begin let tail = Array.make len "" in Array.blit ll 1 tail 0 len; reduce' tail end else begin if !stop = "" then (stop := fst); Array.blit ll 1 ll 0 len; ll.(len) <- fst; reduce' ll end end in Array.to_list (reduce' (Array.of_list ll)) in Hashtbl.iter (fun key deps -> Hashtbl.replace table key (reduce deps)) table ;;  let dot_of_dag (dag : t) = let buf = Buffer.create 1000 in Buffer.add_string buf "digraph {\n"; Hashtbl.iter begin fun key -> List.iter (kprintf (Buffer.add_string buf) "%S -> %S;\n" key) end dag; Buffer.add_string buf "}\n"; Buffer.contents buf;;   let find_toplevels ocamldeps = let all_deps = Hashtbl.fold begin fun key (_, deps) acc -> Printf.printf "OCAMLDEPS: %-30s -> [%s]\n%!" key (String.concat ", " deps); List.rev_append deps acc end ocamldeps [] in let toplevels = Hashtbl.fold begin fun key _ acc -> if List.mem key all_deps then acc else key :: acc end ocamldeps []; in Printf.printf "\nTOPLEVELS: %s\n\n%!" (String.concat ", " toplevels); toplevels ;;  let create_dag ?times ~toplevel_modules ~verbose () = let crono = if verbose >= 4 then crono else fun ?label f x -> f x in let dirs = List.map Filename.dirname toplevel_modules in let dirs = List.filter ((<>) ".") dirs in let dirs = remove_dupl dirs in let search_path = List.map Ocaml_config.expand_includes dirs in let search_path = String.concat " " search_path in  let ocamldeps = let mode =          `Recursive in match mode with  | `Recursive -> let ocamldeps = crono ~label:"Oebuild_dep_dag.create_dag, ocamldep(`Recursive)" (Oebuild_dep.ocamldep_recursive ?times ~search_path ~verbose:false) toplevel_modules in ocamldeps in if verbose >= 4 then Printf.printf "OCAMLDEPS LENGTH: %d\n%!" (Hashtbl.length ocamldeps);  try let table = Hashtbl.create 17 in let rec add path node = if List.mem node path then raise (Cycle_exception (node :: path)) else begin let changed, children = try Hashtbl.find ocamldeps node with Not_found -> false, [] in if not (Hashtbl.mem table node) then begin if changed then Hashtbl.add table node children; List.iter (add (node :: path)) children; end end in let toplevel_modules_cmx = List.map (fun filename -> (Filename.chop_extension filename) ^ ".cmx") toplevel_modules in let need_find_tl = List.exists (fun tl -> not (Hashtbl.mem ocamldeps tl)) toplevel_modules_cmx in  let toplevel_modules_cmx = if need_find_tl then crono ~label:"Oebuild_dep_dag.create_dag, find_toplevels" find_toplevels ocamldeps else toplevel_modules_cmx in crono ~label:"Oebuild_dep_dag.create_dag, add" (List.iter (add [])) toplevel_modules_cmx; crono ~label:"Oebuild_dep_dag.create_dag, reduce" reduce table; if verbose >= 4 then begin Printf.printf "DAG: number of nodes = %d\n%!" (Hashtbl.length table); Hashtbl.iter begin fun node children -> Printf.printf "DAG: %-40s : [%s]\n%!" node (String.concat "; " children); end table; end; Dag ((table : t), ocamldeps) with Cycle_exception cycle -> match cycle with | hd :: _ -> let cycle = let found = ref false in List.filter begin fun x -> found := !found || x = hd; !found end (List.rev cycle) in Cycle (List.rev cycle) | [] -> assert false ;; end
module Oebuild_parallel = struct                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 open Printf module Dep_dag = Oebuild_dep_dag type process_output = { command           : string; filename          : string; mutable exit_code : int; mutable err       : Buffer.t; mutable out       : Buffer.t; } module NODE = struct type key = string type t = { nd_create_command     : (string -> string option); nd_at_exit            : (process_output -> unit); nd_filename           : string; mutable nd_processing : bool; } let equal a b = a.nd_filename = b.nd_filename let hash x = Hashtbl.hash x.nd_filename let to_string x = x.nd_filename end module Dag = Oebuild_dag.Make(NODE) type t = (NODE.key, Dag.entry) Hashtbl.t type dag = { graph     : t; ocamldeps : (string, bool * string list) Hashtbl.t; mutex     : Mutex.t; }  let print_results err_outputs ok_outputs = flush_all();  let sep = "\n" in List.iter begin fun process_output -> let has_out = Buffer.length process_output.out > 0 in let has_err = Buffer.length process_output.err > 0 in if has_out then printf "%s\n%s\n%s%!" process_output.command (Buffer.contents process_output.out) sep; if has_err then eprintf "%s\n%s\n%s%!" process_output.command (Buffer.contents process_output.err) sep; end ok_outputs; flush_all(); List.iter begin fun process_output -> let has_out = Buffer.length process_output.out > 0 in let has_err = Buffer.length process_output.err > 0 in let cmd = sprintf "%s\n(exit code = %d)" process_output.command process_output.exit_code in if has_out then printf "%s\n%s\n%s%!" cmd (Buffer.contents process_output.out) sep; if has_err then eprintf "%s\n%s\n%s%!" cmd (Buffer.contents process_output.err) sep; end err_outputs; flush_all() ;;  let create_dag ?times ~cb_create_command ~cb_at_exit ~toplevel_modules ~verbose () = let open Dag in match Dep_dag.create_dag ?times ~toplevel_modules ~verbose () with | Dep_dag.Cycle cycle -> kprintf failwith "Cycle: %s" (String.concat "->" cycle) | Dep_dag.Dag (dag', ocamldeps) -> let dag = Hashtbl.create 17 in Hashtbl.iter begin fun filename deps -> let node = { NODE.nd_create_command = cb_create_command; nd_at_exit        = cb_at_exit; nd_filename       = filename; nd_processing     = false } in Hashtbl.add dag filename { key          = filename; node         = node; dependencies = []; dependants   = [] } end dag'; Hashtbl.iter begin fun node deps -> try let node = Hashtbl.find dag node in List.iter begin fun dep -> try let e = Hashtbl.find dag dep in node.dependencies <- e :: node.dependencies; with Not_found -> () end deps; with Not_found -> assert false end dag'; set_dependants dag; { graph = dag; ocamldeps; mutex = Mutex.create() } ;; let job_counter = ref 1 let job_mutex = Mutex.create ()  let create_process ?(jobs=0) ~verbose cb_create_command cb_at_exit dag leaf errors messages = leaf.Dag.node.NODE.nd_processing <- true; let filename = Oebuild_util.replace_extension_to_ml leaf.Dag.node.NODE.nd_filename in let command = cb_create_command filename in match command with | Some command when jobs = 0 || !job_counter <= jobs -> if verbose >= 4 then Printf.printf "Oebuild_parallel.create_process [%d/%d]: %s\n%!" !job_counter jobs                                command else if verbose >= 2 then print_endline command; let output = { command; filename; exit_code  = 0; err        = Buffer.create 10; out        = Buffer.create 10 } in let at_exit exit_code = output.exit_code <- exit_code; if exit_code <> 0 then (errors := output :: !errors) else messages := output :: !messages; Mutex.lock dag.mutex; Dag.remove_leaf dag.graph leaf; Mutex.unlock dag.mutex;  Mutex.lock job_mutex; decr job_counter; Mutex.unlock job_mutex;  cb_at_exit output in let process_in stdin = Buffer.add_string output.out (input_line stdin); Buffer.add_char output.out '\n'; in let process_err stderr = Buffer.add_string output.err (input_line stderr); Buffer.add_char output.err '\n'; in  Mutex.lock job_mutex; incr job_counter; Mutex.unlock job_mutex;  Oebuild_util.exec ~verbose:false ~join:false ~at_exit ~process_in ~process_err command | None -> if verbose >= 4 then Printf.printf "Oebuild_parallel.create_process: %30s (No command)\n%!" filename; Mutex.lock dag.mutex; Dag.remove_leaf dag.graph leaf; Mutex.unlock dag.mutex; None | _ -> leaf.Dag.node.NODE.nd_processing <- false; None ;;  let process_parallel ?jobs ~verbose dag = let open NODE in let errors = ref [] in let messages = ref [] in  let leaves = ref [] in begin try while leaves := Dag.get_leaves dag.graph; !leaves <> [] do List.iter begin fun leaf -> if not leaf.Dag.node.nd_processing then (create_process ?jobs ~verbose leaf.Dag.node.nd_create_command leaf.Dag.node.nd_at_exit dag leaf errors messages |> ignore) end !leaves;  if !errors <> [] then raise Exit; Thread.delay 0.005; done; with Exit -> () end; let errors = List.rev !errors in let messages = List.rev !messages in  print_results errors messages;  ;; end
module Oebuild = struct open Printf open Oebuild_util module Table = Oebuild_table type compilation_type = Bytecode | Native | Unspecified type output_type = Executable | Library | Plugin | Pack | External type build_exit = Built_successfully | Build_failed of int type process_err_func = (in_channel -> unit) let string_of_compilation_type = function | Bytecode -> "Bytecode" | Native -> "Native" | Unspecified -> "Unspecified" let string_of_output_type = function | Executable -> "Executable" | Library -> "Library" | Plugin -> "Plugin" | Pack -> "Pack" | External -> "External";; let ocamlc = Ocaml_config.ocamlc() let ocamlopt = Ocaml_config.ocamlopt() let ocamllib = Ocaml_config.ocamllib()  let check_package_list = let redirect_stderr = if Sys.os_type = "Win32" then " 1>NUL 2>NUL" else " 1>/dev/null 2>/dev/null" in fun package_list -> let package_list = Str.split (Str.regexp "[, ]") package_list in let available, unavailable = List.partition begin fun package -> kprintf (Oebuild_util.command ~echo:false) "ocamlfind query %s %s" package redirect_stderr = 0 end package_list in if unavailable <> [] then begin eprintf "Warning (oebuild): the following packages are not found: %s\n" (String.concat ", " unavailable); end; String.concat "," available;;  let get_compiler_command ?(times : Table.t option) ~opt ~compiler ~cflags ~includes ~filename ~verbose () = if Sys.file_exists filename then begin try begin match times with | Some times -> ignore (Table.find times filename opt); None | _ -> raise Not_found end with Not_found -> begin let verbose_opt = if verbose >= 5 then " -verbose" else "" in Some (sprintf "%s -c %s %s%s %s" compiler cflags includes verbose_opt filename) end end else None  let compile ?(times : Table.t option) ~opt ~compiler ~cflags ~includes ~filename ?(process_err : process_err_func option) ~verbose () = let command = get_compiler_command ?times ~opt ~compiler ~cflags ~includes ~filename ~verbose () in match command with | Some cmd -> let exit_code = match process_err with | None -> Oebuild_util.command cmd | Some process_err -> begin match exec ~process_err ~verbose:(verbose>=2) cmd with | Some n -> n | None -> -9998 end; in may times (fun times -> Table.add times filename opt (Unix.gettimeofday())); exit_code | _ -> 0   let link ~compilation ~compiler ~outkind ~lflags ~includes ~libs ~outname ~deps ?(process_err : process_err_func option) ~verbose () = let opt = compilation = Native && ocamlopt <> None in let libs = if            outkind <> Executable then "" else let ext = if opt then "cmxa" else "cma" in let libs = List.map begin fun x -> if Filename.check_suffix x ".o" then begin let x = Filename.chop_extension x in let ext = if opt then "cmx" else "cmo" in sprintf "%s.%s" x ext end else if Filename.check_suffix x ".obj" then begin sprintf "%s" x end else (sprintf "%s.%s" x ext) end libs in String.concat " " libs in let deps = String.concat " " deps in let process_exit = kprintf (exec             ?process_err ~verbose:(verbose>=2)) "%s %s %s -o %s %s %s %s %s" compiler (match outkind with Library -> "-a" | Plugin when opt -> "-shared" | Plugin -> "" | Pack -> "-pack" | Executable | External -> "") lflags outname includes libs deps (if verbose >= 5 then "-verbose" else "") in match process_exit with | Some n -> n | None -> -9997 ;;  let get_output_name ~compilation ~outkind ~outname ~toplevel_modules = match toplevel_modules with | [] -> None | _ -> Some begin let o_ext = match outkind with | Library when compilation = Native -> ".cmxa" | Library -> ".cma" | Executable when compilation = Native -> ".opt" ^ (win32 ".exe" "") | Executable -> win32 ".exe" "" | Plugin when compilation = Native -> ".cmxs" | Plugin -> ".cma" | Pack -> ".cmx" | External -> "" in let name = if outname = "" then begin match (List.rev toplevel_modules) with | last :: _ -> Filename.chop_extension last | _ -> assert false end else outname in name ^ o_ext end ;;  let install ~compilation ~outkind ~outname ~deps ~path ~ccomp_type = let dest_outname = Filename.basename outname in match outkind with | Library -> let path = let path = ocamllib // path in mkdir_p path; path in cp outname (path // dest_outname); let deps_mod = List.map Filename.chop_extension deps in let deps_mod = remove_dupl deps_mod in let cmis = List.map (fun d -> sprintf "%s.cmi" d) deps_mod in let mlis = List.map (fun cmi -> sprintf "%s.mli" (Filename.chop_extension cmi)) cmis in let mlis = List.filter Sys.file_exists mlis in List.iter (fun x -> ignore (cp x (path // (Filename.basename x)))) cmis; List.iter (fun x -> ignore (cp x (path // (Filename.basename x)))) mlis; if compilation = Native then begin let ext = match ccomp_type with Some "msvc" -> ".lib" | Some _ ->  ".a" | None -> assert false in let basename = sprintf "%s%s" (Filename.chop_extension outname) ext in cp basename (path // (Filename.basename basename)); end; | Executable                                                                    | Plugin | Pack | External -> eprintf "\"Oebuild.install\" not implemented for Executable, Plugin, Pack or External." ;;  let run_output ~outname ~args = let args = List.rev args in if is_win32 then begin let cmd = Str.global_replace (Str.regexp "/") "\\\\" outname in let args = String.concat " " args in ignore (kprintf command "%s %s" cmd args) end else begin let cmd = Filename.current_dir_name // outname in  let args = cmd :: args in let args = Array.of_list args in Unix.execv cmd args end ;;  let sort_dependencies ~deps subset = let result = ref [] in List.iter begin fun x -> if List.mem x subset then (result := x :: !result) end deps; List.rev !result ;;  let filter_inconsistent_assumptions_error ~compiler_output ~recompile ~toplevel_modules ~deps ~(cache : Table.t) ~opt = let re_inconsistent_assumptions = Str.regexp ".*make[ \t\r\n]+inconsistent[ \t\r\n]+assumptions[ \t\r\n]+over[ \t\r\n]+\\(interface\\|implementation\\)[ \t\r\n]+\\([^ \t\r\n]+\\)[ \t\r\n]+" in let re_error = Str.regexp "Error: " in ((fun stderr -> let line = input_line stderr in Buffer.add_string compiler_output (line ^ "\n"); let messages = Buffer.contents compiler_output in let len = String.length messages in try let pos = Str.search_backward re_error messages len in let last_error = String.sub messages pos (len - pos) in begin try let _ = Str.search_backward re_inconsistent_assumptions last_error (String.length last_error) in let modname = Str.matched_group 2 last_error in let dependants = Oebuild_dep.find_dependants ~path:(List.map Filename.dirname toplevel_modules) ~modname in let dependants = sort_dependencies ~deps dependants in let _                    = Buffer.contents compiler_output in eprintf "Warning (oebuild): the following files make inconsistent assumptions over interface/implementation %s: [%s]\n%!" modname (String.concat "; " dependants); List.iter begin fun filename -> Table.remove cache filename opt; let basename = Filename.chop_extension filename in let cmi = basename ^ ".cmi" in if Sys.file_exists cmi then (Sys.remove cmi); let cmo = basename ^ ".cmo" in if Sys.file_exists cmo then (Sys.remove cmo); let cmx = basename ^ ".cmx" in if Sys.file_exists cmx then (Sys.remove cmx); let obj = basename ^ (win32 ".obj" ".o") in if Sys.file_exists obj then (Sys.remove obj); end dependants; recompile := dependants; with Not_found -> () end with Not_found -> ()) : process_err_func) ;;  let serial_compile ~compilation ~times ~compiler ~cflags ~includes ~toplevel_modules ~deps ~verbose = let crono = if verbose >= 3 then crono else fun ?label f x -> f x in let compilation_exit = ref 0 in begin try let opt = compilation = Native in let compiler_output = Buffer.create 100 in let rec try_compile filename = let recompile = ref [] in let compile_exit = Table.update ~opt times filename |> ignore; let exit_code = compile ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~toplevel_modules ~deps ~cache:times ~opt) ~times ~opt ~compiler ~cflags ~includes ~filename ~verbose () in if exit_code <> 0 then (Table.remove times filename opt); exit_code in if List.length !recompile > 0 then begin List.iter begin fun filename -> compilation_exit := compile ~times ~opt ~compiler ~cflags ~includes ~filename ~verbose (); if !compilation_exit <> 0 then (raise Exit) end !recompile; print_newline(); Buffer.clear compiler_output; try_compile filename; end else begin if Buffer.length compiler_output > 0 then (eprintf "%s\n%!" (Buffer.contents compiler_output)); compile_exit end in crono ~label:"Serial compilation" (List.iter begin fun filename -> compilation_exit := try_compile filename; if !compilation_exit <> 0 then (raise Exit) end) deps; with Exit -> () end; !compilation_exit  let parallel_compile ~compilation ?times ~compiler ~cflags ~includes ~toplevel_modules ~verbose ?jobs () = let crono = if verbose >= 3 then crono else fun ?label f x -> f x in let crono4 = if verbose >= 4 then crono else fun ?label f x -> f x in let open Oebuild_parallel in let opt = compilation = Native in let may_update_times = match times with Some times -> fun filename -> Table.update ~opt times filename |> ignore | _ -> ignore in let cb_create_command filename = may_update_times filename; get_compiler_command ?times ~opt ~compiler ~cflags ~includes ~filename ~verbose () in let cb_at_exit out = may times begin fun times -> if out.exit_code = 0 then Table.add times out.filename opt (Unix.gettimeofday()) else Table.remove times out.filename opt; end in let times = match times with Some t -> Some (t, opt) | _ -> None in let dag = crono4 ~label:"Oebuild_parallel.create_dag (ocamldep+add+reduce)" (fun () -> Oebuild_parallel.create_dag ?times ~cb_create_command ~cb_at_exit ~toplevel_modules ~verbose ()) () in crono ~label:"Parallel compilation" (Oebuild_parallel.process_parallel ?jobs ~verbose) dag; let sorted_deps = Oebuild_dep.sort_dependencies dag.ocamldeps in let deps_ml = List.map replace_extension_to_ml sorted_deps in sorted_deps, deps_ml, 0  let build ~compilation ~package ~includes ~libs ~other_mods ~outkind ~compile_only ~thread ~vmthread ~annot ~bin_annot ~pp ?inline ~cflags ~lflags ~outname ~deps ~dontlinkdep               ~toplevel_modules ?(jobs=0) ?(serial=false) ?(prof=false) ?(verbose=2) () = let crono = if verbose >= 3 then crono else fun ?label f x -> f x in let crono4 = if verbose >= 4 then crono else fun ?label f x -> f x in if verbose >= 1 then begin printf "\n%s %s" (string_of_compilation_type compilation) (string_of_output_type outkind); if outname <> ""    then (printf " %s\n" outname); if dontlinkdep      then (printf "  ~dontlinkdep\n"); if compile_only     then (printf "  ~compile_only\n"); if thread           then (printf "  ~thread\n"); if vmthread         then (printf "  ~vmthread\n"); if bin_annot        then (printf "  ~binannot\n"); if package <> ""    then (printf "  ~package ........ : %s\n" package); if includes <> ""   then (printf "  ~search_path .... : %s\n" includes); if libs <> ""       then (printf "  ~libs ........... : %s\n" libs); if other_mods <> "" then (printf "  ~other_mods ..... : %s\n" other_mods); (match inline with Some n -> printf "  ~inline ......... : %d\n" n | _ -> ()); if cflags <> ""     then (printf "  ~cflags ......... : %s\n" cflags); if lflags <> ""     then (printf "  ~lflags ......... : %s\n" cflags); if toplevel_modules <> [] then (printf "  ~toplevel_modules : %s\n" (String.concat "," toplevel_modules)); if serial           then (printf "  ~serial ......... : %b\n" serial) else (printf "  ~jobs ........... : %d\n" jobs); printf "  ~verbose ........ : %d\n" verbose; if verbose >= 2 then (printf "\n"); printf "%!"; end; let split_space = Str.split re_spaces in  let includes = ref includes in includes := Ocaml_config.expand_includes !includes;  let libs = split_space libs in  let cflags = ref cflags in let lflags = ref lflags in  if thread then (cflags := !cflags ^ " -thread"; lflags := !lflags ^ " -thread"); if vmthread then (cflags := !cflags ^ " -vmthread"; lflags := !lflags ^ " -vmthread"); if annot then (cflags := !cflags ^ " -annot"); if bin_annot then (cflags := !cflags ^ " -bin-annot"); if pp <> "" then (cflags := !cflags ^ " -pp " ^ pp);  begin match inline with | Some inline when compilation = Native -> let inline = string_of_int inline in cflags := !cflags ^ " -inline " ^ inline; lflags := !lflags ^ " -inline " ^ inline; | _ -> () end;  let package = check_package_list package in let compiler, linker = if prof then "ocamlcp -p a", "ocamlcp -p a" else begin let ocaml_c_opt = if compilation = Native then (match ocamlopt with Some x -> x | _ -> failwith "ocamlopt was not found") else ocamlc in let use_findlib = package <> "" in if use_findlib then let thread = if thread then "-thread" else if vmthread then "-vmthread" else "" in let ocaml_c_opt = try Filename.chop_extension ocaml_c_opt with Invalid_argument "Filename.chop_extension" -> ocaml_c_opt in let ocamlfind = sprintf "ocamlfind %s -package %s %s" ocaml_c_opt package thread in let compiler = crono4 ~label:"Oebuild, get_effective_command(compiler)" get_effective_command ocamlfind in let linker = if compile_only then "" else let linkpkg = outkind <@ [Executable] in if linkpkg then ocamlfind ^ " -linkpkg" else ocamlfind  in compiler, linker else ocaml_c_opt, ocaml_c_opt end in  let times = Table.read () in let build_exit =  let deps, deps_ml, compilation_exit = if serial then let deps_ml = List.map replace_extension_to_ml deps in deps, deps_ml, serial_compile ~compilation ~times ~compiler ~cflags:!cflags ~includes:!includes ~toplevel_modules ~deps:deps_ml ~verbose else parallel_compile ~compilation ~times ~compiler ~cflags:!cflags ~includes:!includes ~toplevel_modules ~verbose ~jobs () in   if compilation_exit = 0 then begin let opt = compilation = Native in let find_objs filenames = let objs = List.filter (fun x -> x ^^ ".cmx") filenames in if opt then objs else List.map (fun x -> (Filename.chop_extension x) ^ ".cmo") objs in let mods = split_space other_mods in let mods = if compilation = Native then List.map (sprintf "%s.cmx") mods else List.map (sprintf "%s.cmo") mods in let obj_deps = if dontlinkdep then find_objs (List.map (fun ml -> if ml ^^ ".ml" then (Filename.chop_extension ml) ^ ".cmx" else if ml ^^ ".mli" then (Filename.chop_extension ml) ^ ".cmi" else ml) toplevel_modules) else mods @ (find_objs deps) in if compile_only then compilation_exit else begin let compiler_output = Buffer.create 100 in let rec try_link () = let recompile = ref [] in let link_exit = crono ~label:"Linking phase" (link ~compilation ~compiler:linker ~outkind ~lflags:!lflags ~includes:!includes ~libs ~deps:obj_deps ~outname ~process_err:(filter_inconsistent_assumptions_error ~compiler_output ~recompile ~toplevel_modules ~deps:deps_ml ~cache:times ~opt) ~verbose) () in if List.length !recompile > 0 then begin if serial then begin crono ~label:"Serial compilation" (List.iter begin fun filename -> ignore (compile ~times ~opt ~compiler ~cflags:!cflags ~includes:!includes ~filename ~verbose ()) end) !recompile; end else begin parallel_compile ~compilation ~times ~compiler ~cflags:!cflags ~includes:!includes ~toplevel_modules ~verbose ~jobs () |> ignore end; Buffer.clear compiler_output; try_link() end else begin eprintf "%s\n%!" (Buffer.contents compiler_output); link_exit end in try_link() end end else compilation_exit in Table.write times; if build_exit = 0 then Built_successfully else (Build_failed build_exit) ;;  let obj_extensions = [".cmi"; ".cmo"; ".cmx"; ".o"; ".obj"; ".annot"; ".cmt"; ".cmti"] let lib_extensions = [".cma"; ".cmxa"; ".lib"; ".a"] let clean ~deps () = let files = List.map begin fun name -> let name = Filename.chop_extension name in List.map ((^) name) obj_extensions end deps in let files = List.flatten files in  let files = remove_dupl files in List.iter (fun file -> remove_file ~verbose:false file) files ;;  let distclean () = let cwd = Sys.getcwd() in let exists_suffix sufs name = List.exists (fun suf -> name ^^ suf) sufs in let rec clean_dir dir = if not ((Unix.lstat dir).Unix.st_kind = Unix.S_LNK) then begin let files = Sys.readdir dir in let files = Array.to_list files in let files = List.map (fun x -> dir // x) files in let directories, files = List.partition Sys.is_directory files in let files = List.filter (exists_suffix (obj_extensions @ lib_extensions)) files in List.iter (remove_file ~verbose:false) files; let oebuild_times_filename = dir // Table.oebuild_times_filename in remove_file ~verbose:false oebuild_times_filename; List.iter clean_dir directories; end in clean_dir cwd; ;; let re_fl_pkg_exist = Str.regexp "HAVE_FL_PKG(\\([-A-Za-z0-9_., ]+\\))" let re_comma = Str.regexp " *, *"  let check_restrictions restr = List.for_all begin function | "IS_UNIX" -> Sys.os_type = "Unix" | "IS_WIN32" -> Sys.os_type = "Win32" | "IS_CYGWIN" -> Sys.os_type = "Cygwin" | "HAVE_NATIVE" | "HAS_NATIVE" -> Ocaml_config.can_compile_native () <> None                        | res when Str.string_match re_fl_pkg_exist res 0 -> let packages = Str.matched_group 1 res in let packages = Str.split re_comma packages in let redirect_stderr = if Sys.os_type = "Win32" then " 1>NUL 2>NUL" else " 1>/dev/null 2>/dev/null" in packages = [] || List.for_all begin fun package -> kprintf (Oebuild_util.command ~echo:false) "ocamlfind query %s %s" package redirect_stderr = 0 end packages | _ -> false end restr;; end
module Build_script_util = struct open Arg open Printf open Oebuild open Oebuild_util open Task type target = { descr : string; num : int; id : int; output_name : string; target_type : Oebuild.output_type; compilation_bytecode : bool; compilation_native : bool; toplevel_modules : string; mutable package : string; search_path : string; required_libraries : string; compiler_flags : string; linker_flags : string; thread : bool; vmthread : bool; pp : string; inline : int option; nodep : bool; dontlinkdep : bool; library_install_dir : string; other_objects : string; external_tasks : int list; restrictions : string list; dependencies : int list; show : bool; } type target_map_entry = int * (string * target) exception Error let pushd, popd = let stack = Stack.create () in begin fun dir -> let cwd = Sys.getcwd () in Stack.push cwd stack; Sys.chdir dir end, (fun () -> Sys.chdir (Stack.pop stack));; let rpad txt c width = let result = txt ^ (String.make width c) in String.sub result 0 width;; let get_compilation_types native t = (if t.compilation_bytecode then [Bytecode] else []) @ (if t.compilation_native && native then [Native] else []) let string_of_compilation_type native t = let compilation = get_compilation_types native t in String.concat "/" (List.map string_of_compilation_type compilation) let create_target ?dir f x = (match dir with Some dir -> pushd dir | _ -> ()); f x; (match dir with Some _ -> popd() | _ -> ());; let create_target_func ?tg targets = match targets with | default_target :: _ -> (match tg with Some f -> create_target f | _ -> create_target default_target) | [] -> fun _ -> ();;  let ccomp_type = Ocaml_config.can_compile_native () let system_config () = let ocaml_version = Cmd.expand ~first_line:true "ocamlc -v" in let std_lib = Cmd.expand ~first_line:true "ocamlc -where" in let properties = [ "OCaml", ocaml_version; "Standard library directory", std_lib; "OCAMLLIB", (try Sys.getenv "OCAMLLIB" with Not_found -> "<Not_found>"); "Native compilation supported", (match ccomp_type with Some ccomp_type -> ccomp_type | _ -> "No"); ] in let buf = Buffer.create 100 in Buffer.add_string buf "\nSystem configuration\n"; let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in List.iter (fun (n, v) -> bprintf buf "  %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties; Buffer.contents buf;;  module Option = struct let prefix = ref "" let change_dir = ref "src" let verbosity = ref 2 end  module ETask = struct let filter tasks phase = List.filter begin fun task -> if task.et_always_run_in_script then match task.et_phase with | Some ph -> ph = phase | _ -> false else false end tasks;; let execute = Task.handle begin fun ~env ~dir ~prog ~args -> let cmd = sprintf "%s %s" prog (String.concat " " args) in let old_dir = Sys.getcwd () in Sys.chdir dir; let exit_code = Oebuild_util.exec ~env cmd in Sys.chdir old_dir; match exit_code with | Some 0 -> () | _ -> raise Error end end  let targets_selected : target_map_entry list ref = ref [] let add_target targets name = try begin try let num = int_of_string name in if num <= 0 then (raise Exit); let name_tg = try List.find (fun (_, tg) -> tg.num = num) targets with Not_found -> raise Exit in targets_selected := (num, name_tg) :: !targets_selected; with _ -> let tg = List.assoc name targets in if tg.num <= 0 then (raise Exit); targets_selected := (tg.num, (name, tg)) :: !targets_selected; end with Exit | Not_found -> (raise (Arg.Bad (sprintf "Invalid target `%s'" name)));;  let rec find_target_dependencies targets trg = remove_dupl (List.flatten (List.map begin fun id -> try let _, target = List.find (fun (_, tg) -> tg.id = id) targets in (find_target_dependencies targets target) @ [target] with Not_found -> [] end trg.dependencies));;  let show = fun targets -> function num, (name, t) -> let files = Str.split (Str.regexp " +") t.toplevel_modules in  let b_deps = find_target_dependencies targets t in let b_deps = List.map begin fun tg -> let name, _ = List.find (fun (_, t) -> t.id = tg.id) targets in name end b_deps in let compilation = (if t.compilation_bytecode then [Bytecode] else []) @ (if t.compilation_native && ccomp_type <> None then [Native] else []) in let outname = List.map begin fun compilation -> let oname = get_output_name ~compilation ~outkind:t.target_type ~outname:t.output_name ~toplevel_modules:files in match oname with Some x -> x | _ -> "" end compilation in let outkind = string_of_output_type t.target_type in let compilation = string_of_compilation_type (ccomp_type <> None) t in let prop_1 = [ "Restrictions", (String.concat " " t.restrictions); "Output name", (String.concat ", " outname); ] in let prop_2 = [ "Findlib packages", t.package; "Search path", t.search_path; "Required libraries", t.required_libraries; "Compiler flags", t.compiler_flags; "Linker flags", t.linker_flags; "Toplevel modules", t.toplevel_modules; "Target dependencies", (String.concat ", " b_deps); ] in let properties = if t.target_type = Library then prop_1 @ [ "Install directory", (Oebuild.ocamllib // t.library_install_dir) ] @ prop_2 else prop_1 @ prop_2 in printf "%2d) %s (%s, %s)%s\n\n%!" num name outkind compilation (if t.descr <> "" then "\n    " ^ t.descr else ""); let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in List.iter (fun (n, v) -> printf "    %s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;;  let install_lib ~compilation ~outname ~external_tasks ~deps target = match target.target_type with | Library -> let deps = deps() in Oebuild.install ~compilation ~outkind:target.target_type ~outname ~deps ~path:target.library_install_dir ~ccomp_type | Executable | Plugin | Pack | External -> eprintf "\"install\" not implemented for Executable, Plugin, Pack or External."; raise Exit;;  let rec execute_target ~external_tasks ~targets:avail_targets ~command ?target_deps target = if Oebuild.check_restrictions target.restrictions then begin let compilation = (if target.compilation_bytecode then [Bytecode] else []) @ (if target.compilation_native && (ccomp_type <> None) then [Native] else []) in let files = Str.split (Str.regexp " +") target.toplevel_modules in let deps () = let verbose = !Option.verbosity >= 4 in Oebuild_dep.ocamldep_toplevels ~verbose ~pp:target.pp ~ignore_stderr:false files |> Oebuild_dep.sort_dependencies in let etasks = List.map begin fun index -> let mktask = try List.assoc index external_tasks with Not_found -> assert false in mktask command end target.external_tasks in try match target.target_type with | External when command = `Build -> build ~targets:avail_targets ~external_tasks ~etasks ~deps ~compilation:Unspecified ~outname:target.output_name ~files ?target_deps ~verbose:!Option.verbosity target | External -> () | Executable | Library | Pack | Plugin -> List.iter begin fun compilation -> match get_output_name ~compilation ~outkind:target.target_type ~outname:target.output_name ~toplevel_modules:files with | Some outname -> begin match command with | `Build -> build ~targets:avail_targets ~external_tasks ~etasks ~deps ~compilation ~outname ~files ?target_deps ~verbose:!Option.verbosity target | `Install_lib -> install_lib ~compilation ~outname ~external_tasks ~deps target | `Clean -> List.iter ETask.execute (ETask.filter etasks Before_clean); let deps = deps() in Oebuild.clean ~deps (); List.iter ETask.execute (ETask.filter etasks After_clean); | `Distclean -> if files <> [] then (Oebuild_util.remove_file ~verbose:false outname); | `Show | `Install | `Uninstall -> assert false end | _ -> () end compilation with Exit -> () end else begin let target_name, _ = try List.find (fun (_, t) -> t.id = target.id) avail_targets with Not_found -> kprintf failwith "Target not found (id=%d)" target.id in if !Option.verbosity >= 1 then begin Printf.printf "=== %s ===\n%!" target_name; Printf.printf "Skipped: %s failed\n\n%!" (String.concat " AND " target.restrictions); end end  and build ~targets:avail_targets ~external_tasks ~etasks ~deps ~compilation ~outname ~files ?target_deps ~verbose target = let target_deps = match target_deps with | None -> []                                                   | Some x -> x in List.iter (execute_target ~external_tasks ~targets:avail_targets ~command:`Build) target_deps; let target_name, _ = try List.find (fun (_, t) -> t.id = target.id) avail_targets with Not_found -> kprintf failwith "Target not found (id=%d)" target.id in if !Option.verbosity >= 1 then Printf.printf "=== %s ===\n%!" target_name; List.iter ETask.execute (ETask.filter etasks Before_compile); let deps = if target.nodep then files else deps() in let tasks_compile = ETask.filter etasks Compile in if tasks_compile <> [] then List.iter ETask.execute (tasks_compile) else let crono = if !Option.verbosity >= 3 then Oebuild_util.crono else fun ?label f x -> f x in match crono ~label:"Build time" (Oebuild.build ~compilation ~package:target.package ~includes:target.search_path ~libs:target.required_libraries ~other_mods:target.other_objects ~outkind:target.target_type ~compile_only:false ~thread:target.thread ~vmthread:target.vmthread ~annot:false ~bin_annot:false ~pp:target.pp ?inline:target.inline ~cflags:target.compiler_flags ~lflags:target.linker_flags ~outname ~deps ~dontlinkdep:target.dontlinkdep ~verbose ~toplevel_modules:files) () with | Built_successfully -> List.iter ETask.execute (ETask.filter etasks After_compile); | Build_failed n -> popd(); exit n ;;  let main ~cmd_line_args ~external_tasks ~general_commands ~targets:avail_targets = let module Command = struct type t = Build_script_command.t let find_args tag = try List.assoc tag cmd_line_args with Not_found -> [] let command tag = try let descr = snd (List.assoc tag general_commands) in [tag, (find_args tag), descr, ""] with Not_found -> [];; let command_install = command `Install let command_uninstall = command `Uninstall let string_of_command = function | `Install as c when command_install <> [] -> Build_script_command.string_of_command c | `Uninstall as c when command_uninstall <> [] -> Build_script_command.string_of_command c | `Install | `Uninstall -> assert false | x -> Build_script_command.string_of_command x;; let command_of_string = function | "install" when command_install <> [] -> `Install | "uninstall" when command_uninstall <> [] -> `Uninstall | ("install" | "uninstall") as c -> raise (Build_script_command.Unrecognized_command c) | x -> Build_script_command.command_of_string x;; let options = List.map (fun (a, b, c, d) -> a, Arg.align b, c, d) ([ `Build,       (find_args `Build), "Build libraries and executables (default command)", ""; ] @ command_install @ command_uninstall @ [ `Clean,       (find_args `Clean), "Remove output files for the selected target",       ""; `Distclean,   (find_args `Distclean), "Remove all build output",                           ""; `Install_lib, (find_args `Install_lib), "Install libraries as subdirectories relative\n               to the standard library directory", ""; `Show,        (find_args `Show), "Show the build options of a target",                ""; ]);; let anon_fun = function | `Show -> add_target avail_targets | `Build -> add_target avail_targets | `Install_lib -> add_target avail_targets | `Clean -> add_target avail_targets | (`Install | `Uninstall | `Distclean) as x -> fun arg -> kprintf failwith "Invalid anonymous argument `%s' for command `%s'" arg (string_of_command x);;  let execute command = pushd !Option.change_dir; let targets = List.rev !targets_selected in try begin let execute_general_command command = try let index, descr = List.assoc command general_commands in let task = List.assoc index external_tasks in ETask.execute (task command) with Not_found -> () in match command with | `Distclean -> List.iter (fun (_, t) -> execute_target ~external_tasks ~targets:avail_targets ~command t) avail_targets; Oebuild.distclean(); execute_general_command `Distclean; | (`Install | `Uninstall) as command -> execute_general_command command; | `Show -> printf "%s\n%!" (system_config ()); Printf.printf "\n%!" ; if targets = [] then (raise (Arg.Bad "show: no target specified")); List.iter begin fun t -> show avail_targets t; print_newline(); print_newline(); end targets; | _ -> if targets = [] then (raise (Arg.Bad (sprintf "%s: no target specified" (string_of_command command)))); List.iter begin fun (_, (name, tg)) -> let target_deps = remove_dupl (find_target_dependencies avail_targets tg) in  execute_target ~external_tasks ~targets:avail_targets ~command ~target_deps tg end targets end; popd(); with Arg.Bad _ as ex -> popd(); raise ex | ex -> popd(); Printf.eprintf "File \"build_script_util.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());; end in let module Argc = Argc.Make (Command) in let global_options = [ ("-C",       Set_string Option.change_dir, "<dir> Change directory before running [default: src]"); ("-verbose", Set_int Option.verbosity, " Verbosity level (0..5)"); ] in let global_options = Arg.align global_options in let command_name = Filename.basename Sys.argv.(0) in  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 avail_targets in let targets_shown = List.filter (fun (_, tg) -> tg.show) avail_targets in let help_of_targets = String.concat "\n" (List.map begin fun (name, tg) -> let name = rpad name ' ' maxlength in sprintf "  %2d) %s (%s, %s)%s" tg.num name (string_of_output_type tg.target_type) (string_of_compilation_type (ccomp_type <> None) tg) 	(if tg.descr <> "" then "\n      " ^ tg.descr else "") end targets_shown) in let usage_msg = sprintf "\nUSAGE\n  ocaml %s [global-options*] <command> [command-options*] [targets*]\n  ocaml %s <command> -help" command_name command_name in let help_string () = sprintf "%s\n\nGLOBAL OPTIONS%s\nCOMMANDS%s\n\nTARGETS\n%s" usage_msg (Arg.usage_string global_options "") Argc.help_of_commands help_of_targets in try Argc.parse ~usage_msg ~global_options ~default_command:`Build Command.execute with | Arg.Help _ -> print_endline (help_string ()) | Arg.Bad msg -> prerr_endline msg | Argc.Help_Command (cmd, (specs, descr, usage), msg) -> let name = Command.string_of_command cmd in begin match cmd with | `Distclean -> printf "%s %s - %s\n\nUSAGE\n  ocaml %s [global_options*] %s\n\nOPTIONS%s" command_name name descr command_name name (Arg.usage_string specs "") | _ -> printf "%s %s - %s\n\nUSAGE\n  ocaml %s [global_options*] %s [options*] [targets*]\n\nOPTIONS%s" command_name name descr command_name name (Arg.usage_string specs "") end; | Build_script_command.Unrecognized_command msg -> prerr_endline msg | Error -> exit 2 | ex -> prerr_endline (Printexc.to_string ex) ;; end

open Oebuild
open Build_script_util
open Arg
open Task
open Printf

let arg_0_record_backtrace = ref true
let arg_1_use_modified_gtkThread = ref false
let arg_2_prefix = ref "/usr/local"
let arg_3_gmisclib = ref false
let arg_4_nsis = ref false
let arg_5_prefix = ref "/usr/local"
let arg_6_ver_1_8_0 = ref false

let cmd_line_args = [
  `Uninstall, [
    "-prefix", Set_string arg_5_prefix,
      (" Uninstallation prefix (Unix only) [default: " ^ !arg_5_prefix ^ "]");
    "-ver-1.8.0", Set arg_6_ver_1_8_0,
      (" Uninstall OCamlEditor ver. 1.8.0 or earlier [default: " ^ (sprintf "%s" (if !arg_6_ver_1_8_0 then "Set" else "Not Set")) ^ "]");
  ];
  `Install, [
    "-prefix", Set_string arg_2_prefix,
      (" Installation prefix (Unix only) [default: " ^ !arg_2_prefix ^ "]");
    "-gmisclib", Set arg_3_gmisclib,
      (" Install the gmisclib library (miscellaneous widgets \n             based on LablGtk2) [default: " ^ (sprintf "%s" (if !arg_3_gmisclib then "Set" else "Not Set")) ^ "]");
    "-nsis", Set arg_4_nsis,
      (" Create a Win32 installer with NSIS [default: " ^ (sprintf "%s" (if !arg_4_nsis then "Set" else "Not Set")) ^ "]");
  ];
  `Build, [
    "-record-backtrace", Bool (fun x -> arg_0_record_backtrace := x),
      (" Turn recording of exception backtraces on or off [default: " ^ (string_of_bool !arg_0_record_backtrace) ^ "]");
    "-use-modified-gtkThread", Set arg_1_use_modified_gtkThread,
      (" Set this flag if you have Lablgtk-2.14.2 or earlier\n                           and you want to use the included modified version of \n                           gtkThread.ml to reduce CPU consumption [default: " ^ (sprintf "%s" (if !arg_1_use_modified_gtkThread then "Set" else "Not Set")) ^ "]");
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
    et_phase                 = Some Compile;
    et_always_run_in_project = true;
    et_always_run_in_script  = true;
  });
  
  2, (fun command -> {
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
  
  3, (fun command -> {
    et_name                  = "mkversion";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "../tools";
    et_cmd                   = "ocaml";
    et_args                  = [true,"mkversion.ml"; true,"1.11.0"];
    et_phase                 = Some Before_clean;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
  
  4, (fun command -> {
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
  
  5, (fun command -> {
    et_name                  = "Install OCamlEditor";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/install.ml"; 
                                command = `Install && !arg_4_nsis, "-nsis"; 
                                command = `Install && !arg_3_gmisclib, "-gmisclib"; 
                                command = `Install, (sprintf "-prefix %S" !arg_2_prefix)];
    et_phase                 = Some Before_clean;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
  
  6, (fun command -> {
    et_name                  = "Uninstall OCamlEditor";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/uninstall.ml"; 
                                command = `Uninstall && !arg_6_ver_1_8_0, "-ver-1.8.0"; 
                                command = `Uninstall, (sprintf "-prefix %S" !arg_5_prefix)];
    et_phase                 = Some Before_clean;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
  
  7, (fun command -> {
    et_name                  = "distclean";
    et_env                   = [];
    et_env_replace           = false;
    et_dir                   = "..";
    et_cmd                   = "ocaml";
    et_args                  = [true,"tools/distclean.ml"];
    et_phase                 = Some Before_clean;
    et_always_run_in_project = false;
    et_always_run_in_script  = false;
  });
];;


let general_commands = [
  `Distclean, (7, "distclean");
  `Install, (5, "Install OCamlEditor");
  `Uninstall, (6, "Uninstall OCamlEditor");
]


(* Targets ==================================================== *)

let targets = [
  
  (* 0 *)
  "common", {
    descr                = "";
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
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
  
  (* 0 *)
  "icons", {
    descr                = "";
    num                  = 0;
    id                   = 10;
    output_name          = "icons/icons";
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
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [0];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
  
  (* 0 *)
  "oebuildlib", {
    descr                = "";
    num                  = 0;
    id                   = 7;
    output_name          = "oebuildlib";
    target_type          = Library;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "oebuild/oebuild.ml oebuild/oebuild_dep_ext.ml";
    package              = "";
    search_path          = "common oebuild"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-w y";
    linker_flags         = "";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [4];
    show                 = false;
  };
  
  (* 1 *)
  "oebuild", {
    descr                = "";
    num                  = 1;
    id                   = 5;
    output_name          = "oebuild/oebuild";
    target_type          = Executable;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "oebuild/oebuild_tool.ml";
    package              = "str,unix";
    search_path          = "common oebuild"; (* -I *)
    required_libraries   = "common";
    compiler_flags       = "-w y";
    linker_flags         = "";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [7];
    show                 = true;
  };
  
  (* 2 *)
  "oeproc", {
    descr                = "";
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
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = ["IS_WIN32"];
    dependencies         = [];
    show                 = true;
  };
  
  (* 3 *)
  "gmisclib", {
    descr                = "Miscellaneous widgets based on LablGtk2.";
    num                  = 3;
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
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = "gmisclib"; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [];
    show                 = true;
  };
  
  (* 3 *)
  "otherwidgets", {
    descr                = "";
    num                  = 0;
    id                   = 9;
    output_name          = "otherwidgets";
    target_type          = Library;
    compilation_bytecode = true;
    compilation_native   = true;
    toplevel_modules     = "otherwidgets/otherwidgets.ml";
    package              = "lablgtk2";
    search_path          = "icons common otherwidgets gmisclib"; (* -I *)
    required_libraries   = "gmisclib";
    compiler_flags       = "-w sy -g";
    linker_flags         = "-w sy -g";
    thread               = false;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [8];
    show                 = false;
  };
  
  (* 4 *)
  "ocamleditor", {
    descr                = "";
    num                  = 4;
    id                   = 12;
    output_name          = "ocamleditor";
    target_type          = Executable;
    compilation_bytecode = false;
    compilation_native   = true;
    toplevel_modules     = "ocamleditor.ml";
    package              = "compiler-libs.common,dynlink,lablgtk2,str,unix,xml-light";
    search_path          = "+ocamldoc gmisclib common icons otherwidgets oebuild "; (* -I *)
    required_libraries   = "odoc_info gmisclib common icons otherwidgets oebuildlib ocamleditor_lib";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = Some 50;
    nodep                = true;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [14; 19; 16];
    show                 = true;
  };
  
  (* 5 *)
  "ocamleditor-bytecode", {
    descr                = "";
    num                  = 5;
    id                   = 0;
    output_name          = "ocamleditor";
    target_type          = Executable;
    compilation_bytecode = true;
    compilation_native   = false;
    toplevel_modules     = "ocamleditor.ml";
    package              = "compiler-libs.common,dynlink,lablgtk2,str,unix,xml-light";
    search_path          = "+ocamldoc gmisclib common icons otherwidgets oebuild "; (* -I *)
    required_libraries   = "odoc_info gmisclib common icons otherwidgets oebuildlib";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [4; 10; 7; 5; 6; 8; 9; 20; 17; 18];
    show                 = true;
  };
  
  (* 6 *)
  "ocamleditor-msvc", {
    descr                = "";
    num                  = 6;
    id                   = 15;
    output_name          = "ocamleditor";
    target_type          = Executable;
    compilation_bytecode = false;
    compilation_native   = true;
    toplevel_modules     = "ocamleditor.ml";
    package              = "compiler-libs.common,dynlink,lablgtk2,str,unix,xml-light";
    search_path          = "+ocamldoc gmisclib common icons otherwidgets oebuild "; (* -I *)
    required_libraries   = "resource.obj odoc_info gmisclib common icons otherwidgets oebuildlib ocamleditor_lib";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = Some 50;
    nodep                = true;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = ["IS_WIN32"];
    dependencies         = [14; 19; 16];
    show                 = true;
  };
  
  (* 6 *)
  "ocamleditor-native", {
    descr                = "";
    num                  = 0;
    id                   = 11;
    output_name          = "ocamleditor";
    target_type          = Executable;
    compilation_bytecode = false;
    compilation_native   = true;
    toplevel_modules     = "ocamleditor.ml";
    package              = "compiler-libs.common,dynlink,lablgtk2,str,unix,xml-light";
    search_path          = "+ocamldoc gmisclib common icons otherwidgets oebuild "; (* -I *)
    required_libraries   = "odoc_info gmisclib common icons otherwidgets oebuildlib";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = Some 50;
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [14; 19; 16];
    show                 = false;
  };
  
  (* 7 *)
  "ocamleditor-lib", {
    descr                = "";
    num                  = 7;
    id                   = 14;
    output_name          = "ocamleditor_lib";
    target_type          = Library;
    compilation_bytecode = false;
    compilation_native   = true;
    toplevel_modules     = "ocamleditor_lib.ml";
    package              = "compiler-libs.common,dynlink,lablgtk2,str,unix,xml-light";
    search_path          = "+ocamldoc gmisclib common icons otherwidgets oebuild "; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = Some 50;
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = [];
    dependencies         = [4; 10; 5; 6; 8; 9; 20];
    show                 = true;
  };
  
  (* 7 *)
  "plugin-remote-bytecode", {
    descr                = "";
    num                  = 0;
    id                   = 17;
    output_name          = "../plugins/remote";
    target_type          = Library;
    compilation_bytecode = true;
    compilation_native   = false;
    toplevel_modules     = "remote.ml";
    package              = "curl,lablgtk2";
    search_path          = "common icons otherwidgets gmisclib"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-g -w -10";
    linker_flags         = "-g curl.cma";
    thread               = false;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    nodep                = false;
    dontlinkdep          = true;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = ["HAVE_FL_PKG(curl)"];
    dependencies         = [];
    show                 = false;
  };
  
  (* 7 *)
  "plugin-remote-native", {
    descr                = "";
    num                  = 0;
    id                   = 16;
    output_name          = "../plugins/remote";
    target_type          = Plugin;
    compilation_bytecode = false;
    compilation_native   = true;
    toplevel_modules     = "remote.ml";
    package              = "curl,lablgtk2";
    search_path          = "common icons otherwidgets gmisclib"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-g -w -10";
    linker_flags         = "-g curl.cmxa";
    thread               = false;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    nodep                = false;
    dontlinkdep          = true;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = ["HAVE_FL_PKG(curl)"];
    dependencies         = [];
    show                 = false;
  };
  
  (* 7 *)
  "plugin-dotviewer-byetcode", {
    descr                = "";
    num                  = 0;
    id                   = 18;
    output_name          = "../plugins/dot_viewer_svg";
    target_type          = Library;
    compilation_bytecode = true;
    compilation_native   = false;
    toplevel_modules     = "dot_viewer_svg.ml";
    package              = "lablgtk2.rsvg";
    search_path          = "common gmisclib"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g lablrsvg.cma";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    nodep                = false;
    dontlinkdep          = true;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = ["HAVE_FL_PKG(lablgtk2.rsvg)"];
    dependencies         = [];
    show                 = false;
  };
  
  (* 7 *)
  "plugin-dotviewer-native", {
    descr                = "";
    num                  = 0;
    id                   = 19;
    output_name          = "../plugins/dot_viewer_svg";
    target_type          = Plugin;
    compilation_bytecode = false;
    compilation_native   = true;
    toplevel_modules     = "dot_viewer_svg.ml";
    package              = "lablgtk2.rsvg";
    search_path          = "common gmisclib"; (* -I *)
    required_libraries   = "";
    compiler_flags       = "-w syxm -g";
    linker_flags         = "-g lablrsvg.cmxa";
    thread               = true;
    vmthread             = false;
    pp                   = "";
    inline               = None;
    nodep                = false;
    dontlinkdep          = true;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [];
    restrictions         = ["HAVE_FL_PKG(lablgtk2.rsvg)"];
    dependencies         = [];
    show                 = false;
  };
  
  (* 7 *)
  "prepare-build", {
    descr                = "";
    num                  = 0;
    id                   = 20;
    output_name          = "";
    target_type          = External;
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
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [1];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
  
  (* 7 *)
  "tools", {
    descr                = "";
    num                  = 0;
    id                   = 13;
    output_name          = "";
    target_type          = External;
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
    nodep                = false;
    dontlinkdep          = false;
    library_install_dir  = ""; (* Relative to the Standard Library Directory *)
    other_objects        = "";
    external_tasks       = [2; 3; 4; 5; 6; 7];
    restrictions         = [];
    dependencies         = [];
    show                 = false;
  };
];;

(* End of Targets ============================================= *)

let _ = main ~cmd_line_args ~external_tasks ~general_commands ~targets
