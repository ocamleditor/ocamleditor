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

#load "unix.cma"
#load "str.cma"
#use "src/common/cmd.ml"
#use "src/common/miscellanea.ml"


(** Configuration Section =================================================== *)

(* Search path (-I)
   Please set the right search path for lablgtk2 and xml-light.
*)
let lablgtk2  = "+lablgtk2"
let xml_light = ""  (* +xml-light *)
let ocamldoc  = "+ocamldoc"


(* If you have Microsoft-based native Win32 port, set ccopt to your
   "Microsoft Platform SDK\Lib" and "Microsoft Visual Studio 8\VC\lib"
   directories using short format filenames.
   Otherwise set ccopt to empty string. *)
let ccopt =
  match Sys.os_type with
    | "Win32" -> ""
      (* "-ccopt \"-LC:\\Programmi\\MIC977~1\\Lib -LC:\\Programmi\\MID05A~1\\VC\\lib\"" *)
    | _ -> ""


(* If you have Lablgtk-2.14.2 or earlier, use the modified version of gtkThread.ml
   to reduce CPU consumption, as proposed in
   http://yquem.inria.fr/pipermail/lablgtk/2009-December/000335.html *)
let use_modified_gtkThread = true


(** End of Configuration Section ============================================ *)



open Printf
open Arg

exception Build_error of int

let can_compile_native = ref true
let force_bytecode = ref false
let (//) = Filename.concat
let is_win32 = Sys.os_type = "Win32"
let ext = if is_win32 then ".exe" else ""

let prefix = ref "/usr/local"
let debug = " -g"
let annot = ref false
let prof = ref false

let ccopt = try Unix.getenv "OCAMLEDITOR_CCOPT" with Not_found -> ccopt
let use_modified_gtkThread =
  try ignore (Unix.getenv "OCAMLEDITOR_GTKTHREAD"); true
  with Not_found -> use_modified_gtkThread

let search_path = String.concat " " ["+compiler-libs"; lablgtk2; xml_light; ocamldoc]
let search_path = sprintf "%s gmisclib common icons otherwidgets oebuild" search_path
let libs = "unix str threads dynlink odoc_info lablgtk gtkThread.o xml-light gmisclib common icons otherwidgets oebuildlib"

let oebuild_name = sprintf "oebuild%s" ext
let oebuild_command = "oebuild" // oebuild_name


(** run *)
let run cmd =
  printf "%s\n%!" cmd;
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then (raise (Build_error exit_code))

(** mkdir *)
let mkdir d = printf "mkdir %s\n%!" d; Unix.mkdir d 0o755
let rec mkdir_p d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    mkdir d
  end

(** Copy file *)
let copy_file ic oc =
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let cp src dst =
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  let finally () = close_out oc; close_in ic in
  try copy_file ic oc; finally() with ex -> (finally(); raise ex)

(** Remove file *)
let remove_file ?(verbose=false) filename =
  if Sys.file_exists filename && not (Sys.is_directory filename)
  then (if verbose then (print_endline filename); Sys.remove filename)

let rm = if is_win32 then "DEL /F /Q" else "rm -f"
let rmr = if is_win32 then "DEL /F /Q /S" else "rm -fr"


(** substitute *)
let (!!) = sprintf "(*%s*)";;

let substitute ~filename repl =
  let ichan = open_in_bin filename in
  let tmp, ochan = Filename.open_temp_file (Filename.basename filename) "" in
  Pervasives.set_binary_mode_out ochan true;
  let finally () =
    close_in_noerr ichan;
    close_out_noerr ochan;
    let new_filename = filename in
    remove_file new_filename;
    cp tmp new_filename;
    remove_file tmp;
  in
  begin
    try
      while true do
        let line = input_line ichan in
        let line = replace_all ~regexp:false repl line in
        fprintf ochan "%s\n" line;
      done;
      assert false;
    with
      | End_of_file -> finally()
      | ex -> finally(); raise ex
  end;;




(** Targets ----------------------------------------------------------------- *)

let clean_parsers () =
  remove_file "annot_lexer.ml";
  remove_file "annot_parser.ml";
  remove_file "annot_parser.mli";
  remove_file "err_lexer.ml";
  remove_file "err_parser.ml";
  remove_file "err_parser.mli"

(** clean *)
let clean ?(all=false) () =
  let all = if all then "-all" else "" in
  kprintf run "%s ocamleditor.ml -clean%s" oebuild_command all;
  kprintf run "%s common/common.ml -a -byt -opt -clean%s" oebuild_command all;
  kprintf run "%s gmisclib/gmisclib.ml -a -byt -opt -clean%s" oebuild_command all;
  kprintf run "%s otherwidgets/otherwidgets.ml -a -byt -opt -clean%s" oebuild_command all;
  kprintf run "%s icons/icons.ml -a -byt -opt -clean%s" oebuild_command all;
  kprintf run "%s oebuild/oebuild_tool.ml -a -byt -opt -o oebuild/oebuild -clean%s" oebuild_command all;
  kprintf run "%s oeproc/oeproc.ml -byt -opt -o oeproc/oeproc -clean%s" oebuild_command all;
  clean_parsers()

(** cleanall *)
let cleanall () =
  clean_parsers();
  clean ~all:true ();
  kprintf run "%s *.exe *.bak *.annot *~" rm;
  List.iter remove_file ["oebuild"//"oebuild.exe"; "oebuild"//"oebuild.opt.exe";
    "geometry"; "Thumbs.db"; ".." // "pixmaps" // "Thumbs.db"];
  kprintf run "%s %s" rmr (Filename.parent_dir_name // "bak");
  if Sys.file_exists (Filename.parent_dir_name // "tmp") then
    (kprintf run "%s %s" rmr (Filename.parent_dir_name // "tmp" // "*"))

(** common *)
let common () =
  pushd "common";
  kprintf run
    "ocamlc -a -o common.cma -annot -thread -w syumx \
cmd.ml miscellanea.ml file.ml quote.ml ocaml_config.ml cmd_line_args.ml dep.mli dep.ml list_opt.ml common.ml";
  if !can_compile_native then begin
    kprintf run
      "ocamlopt -a -o common.cmxa -annot -thread -w syumx cmd.ml miscellanea.ml file.ml quote.ml \
ocaml_config.ml cmd_line_args.ml dep.mli dep.ml list_opt.ml common.ml";
  end;
  popd()

(** icons *)
let icons () =
  common();
  pushd "icons";
  kprintf run "%s icons.ml -a -byt %s -cflags \"%s%s\" -I \"%s ../common\""
    (".." // oebuild_command) (if !can_compile_native then "-opt" else "") debug (if !annot then " -annot" else "") lablgtk2;
  popd()

(** gmisclib *)
let gmisclib () =
  pushd "gmisclib";
  kprintf run "%s gmisclib.ml -a -byt %s -cflags \"%s%s\" -I \"%s\""
    (".." // oebuild_command) (if !can_compile_native then "-opt" else "") debug (if !annot then " -annot" else "") lablgtk2;
  popd()

(** otherwidgets *)
let otherwidgets () =
  pushd "otherwidgets";
  kprintf run "%s otherwidgets.ml -a -byt %s -cflags \"-w sy %s%s\" -I \"%s ../icons ../common\""
    (".." // oebuild_command) (if !can_compile_native then "-opt" else "") debug (if !annot then " -annot" else "") lablgtk2;
  popd()

(** oebuild *)
let oebuild () =
  common();
  pushd "oebuild";
  kprintf run "ocamlc -a -o oebuildlib.cma -thread -w syumx -annot -I ../common oebuild_util.ml oebuild_table.mli oebuild_table.ml oebuild.ml";
  kprintf run "ocamlc -o %s -thread -w syumx -I ../common str.cma unix.cma threads.cma common.cma oebuildlib.cma oebuild_tool.ml"
    oebuild_name;
  if !can_compile_native then begin
    kprintf run "ocamlopt -a -o oebuildlib.cmxa -thread -w syumx -I ../common oebuild_util.ml oebuild_table.mli oebuild_table.ml oebuild.ml";
    kprintf run "ocamlopt -o oebuild.opt%s %s -thread -w syumx -I ../common str.cmxa unix.cmxa threads.cmxa common.cmxa oebuildlib.cmxa oebuild_tool.ml"
      ext ccopt;
  end;
  popd()

(** oeproc *)
let oeproc () =
  if is_win32 then begin
    pushd "oeproc";
    let ccopt_quoted = Str.global_replace (Str.regexp "\"") "\\\"" ccopt in
    kprintf run "..\\%s oeproc.ml -thread -cflags \"-w yumx\" -lflags \"%s\" -l \"str unix threads\""
      oebuild_command ccopt_quoted;
    if !can_compile_native then begin
      kprintf run "..\\%s oeproc.ml -opt -thread -cflags \"-w yumx\" -lflags \"%s\" -l \"str unix threads\""
        oebuild_command ccopt_quoted;
    end;
    popd()
  end

(** lexyacc *)
let lexyacc () =
  run "ocamllex annot_lexer.mll";
  run "ocamlyacc annot_parser.mly";
  run "ocamllex err_lexer.mll";
  run "ocamlyacc err_parser.mly"

(** generate_oebuild_script *)
let generate_oebuild_script () =
  run "ocaml -I common str.cma unix.cma common.cma generate_oebuild_script.ml"

(** ocamleditor *)
let ocamleditor () =
  cp (if use_modified_gtkThread then "gtkThread3.ml" else "gtkThread4.ml") "gtkThread2.ml";
  if Sys.ocaml_version >= "3.12.0" then begin
    substitute ~filename:((Sys.getcwd()) // "odoc.ml") [!!"@REPL_1@", "| Target _ -> ()"; !!"@REPL_2@", ", _"];
  end;
  icons();
  gmisclib();
  otherwidgets();
  lexyacc();
  kprintf run
    "%s ocamleditor.ml%s %s -thread -lflags \"-linkall%s%s\" -cflags \"-w syumx%s%s%s\" -I \"%s\" -l \"%s\""
      oebuild_command
      (if !prof then " -prof" else "")
      (if !can_compile_native then "-opt" else "-byt")
      debug
      (if !can_compile_native then " -inline 50" else "")
      debug
      (if !annot then " -annot" else "")
      (if !can_compile_native then " -inline 50" else "")
      search_path
      libs;;

(** install *)
let install () =
  if not is_win32 then begin
    let pixmaps = sprintf "%s/share/pixmaps/ocamleditor" !prefix in
    mkdir_p pixmaps;
    kprintf run "cp -vru ../pixmaps/* %s" pixmaps;
    let bin = sprintf "%s/bin" !prefix in
    mkdir_p bin;
    kprintf run "cp -v ocamleditor%s %s/ocamleditor" (if !can_compile_native then ".opt" else "") bin;
    kprintf run "cp -v oebuild/oebuild%s %s" ext bin;
    if !can_compile_native then begin
      kprintf run "cp -v oebuild/oebuild%s.opt %s" ext bin;
    end;
  end

(** uninstall *)
let uninstall () =
  if not is_win32 then begin
    printf "prefix is \"%s\", continue (yes/no)? %!" !prefix;
    let line = input_line stdin in
    if line = "yes" then begin
      kprintf run "rm -vIr %s/share/pixmaps/ocamleditor" !prefix;
      kprintf run "rm -vi %s/bin/ocamleditor" !prefix;
      kprintf run "rm -vi %s/bin/oebuild%s" !prefix ext;
      kprintf run "rm -vi %s/bin/oebuild%s.opt" !prefix ext;
    end
  end

(** all *)
let all () =
  annot := true;
  oebuild ();
  oeproc ();
  ocamleditor()


(** Main -------------------------------------------------------------------- *)
let _ = begin
  try
    let mktarget f x =
      fun () ->
        let cwd = Sys.getcwd () in
        Sys.chdir "src";
        can_compile_native :=
          if !force_bytecode then false
          else if is_win32 then Sys.command "ML /? 1>NUL" = 0 || Sys.command "as -version" = 0
          else !can_compile_native;
        let ocaml_version = Str.global_replace
          (Str.regexp "\n") " - " (Str.global_replace (Str.regexp "\n$") "" (expand "ocamlc -v")) in
        if not is_win32 then
          (printf "Installation prefix .. : %s\n%!" !prefix);
        printf "OCaml ................ : %s\n%!" ocaml_version;
        printf "Native ............... : %b\n%!" !can_compile_native;
        printf "use_modified_gtkThread : %b\n%!" use_modified_gtkThread;
        printf "ccopt ................ : %s\n%!" ccopt;
        f x;
        Sys.chdir cwd
    in
    let target_func = ref (mktarget all ()) in
    let target f x = target_func := mktarget f x in
    let speclist = [
      ("-all",                Unit (target all),                " Build OCamlEditor (default)");
      ("-install",            Unit (target install),            " Install OCamlEditor (Unix only, you may need superuser permissions)");
      ("-prefix",             Set_string prefix,                (sprintf " Installation prefix (Unix only, default is %s)" !prefix));
      ("-clean",              Unit (target clean),              " Remove object files");
      ("-cleanall",           Unit (target cleanall),           " Remove all the build output");
      ("-byt",                Set force_bytecode,               " Force bytecode compilation");
      ("-oebuild",            Unit (target oebuild),            " (undocumented)");
      ("-common",             Unit (target common),             " (undocumented)");
      ("-icons",              Unit (target icons),              " (undocumented)");
      ("-gmisclib",           Unit (target gmisclib),           " (undocumented)");
      ("-oeproc",             Unit (target oeproc),             " (undocumented)");
      ("-lexyacc",            Unit (target lexyacc),            " (undocumented)");
      ("-ocamleditor",        Unit (target ocamleditor),        " (undocumented)");
      ("-uninstall",          Unit (target uninstall),          " (undocumented)");
      ("-annot",              Set annot,                        " (undocumented)");
      ("-prof",               Set prof,                         " (undocumented)");
    ] in
    let speclist = Arg.align speclist in
    let command_name = Filename.basename Sys.argv.(0) in
    let help_message = sprintf "\nUsage:\n  ocaml %s [options]\n\nOptions:" command_name in
    Arg.parse speclist (fun x -> raise (Bad x)) help_message;
    !target_func ();
    exit 0;
  with Build_error _ -> (exit 2)
end










