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

#load "unix.cma"
#load "str.cma"

(** Configuration Section --------------------------------------------------- *)

let includes = "+lablgtk2 +xml-light"

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

(** End of Configuration Section -------------------------------------------- *)

open Printf
open Arg

exception Build_error of int

let (//) = Filename.concat
let is_win32 = Sys.os_type = "Win32"
let ext = if is_win32 then ".exe" else ""

let prefix = ref "/usr/local"
let debug = " -g"
let annot = ref false
let prof = ref false
let can_compile_native = ref true

let ccopt = try Unix.getenv "OCAMLEDITOR_CCOPT" with Not_found -> ccopt
let use_modified_gtkThread =
  try ignore (Unix.getenv "OCAMLEDITOR_GTKTHREAD"); true
  with Not_found -> use_modified_gtkThread

let ocaml_src = "ocaml-src"
let parsing = ocaml_src ^ "/parsing"
let typing = ocaml_src ^ "/typing"
let utils = ocaml_src ^ "/utils"
let includes = sprintf "%s %s %s %s common oebuild" parsing typing utils includes
let libs = "toplevellib unix str threads lablgtk gtkThread.o xml-light common oebuildlib"

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

(** pushd and popd *)
let pushd, popd =
  let stack = Stack.create () in
  begin fun dir ->
    let cwd = Sys.getcwd () in
    Stack.push cwd stack;
    Sys.chdir dir
  end, (fun () -> Sys.chdir (Stack.pop stack))

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
    "ocamlc -a -o common.cma -thread -w syumx str.cma unix.cma threads.cma \
miscellanea.ml quote.ml ocaml_config.ml cmd_line_args.ml dep.mli dep.ml common.ml";
  if !can_compile_native then begin
    kprintf run
      "ocamlopt -a -o common.cmxa -thread -w syumx miscellanea.ml quote.ml \
ocaml_config.ml cmd_line_args.ml dep.mli dep.ml common.ml";
  end;
  popd()

(** oebuild *)
let oebuild () =
  common();
  pushd "oebuild";
  kprintf run "ocamlc -a -o oebuildlib.cma -thread -w syumx -I ../common \
str.cma unix.cma threads.cma common.cma oebuild_util.ml oebuild.ml";
  kprintf run "ocamlc -o %s -thread -w syumx -I ../common \
str.cma unix.cma threads.cma common.cma oebuildlib.cma oebuild_tool.ml"
    oebuild_name;
  if !can_compile_native then begin
    kprintf run "ocamlopt -a -o oebuildlib.cmxa -thread -w syumx -I ../common oebuild_util.ml oebuild.ml";
    kprintf run "ocamlopt -o oebuild.opt%s %s -thread -w syumx -I ../common \
str.cmxa unix.cmxa threads.cmxa common.cmxa oebuildlib.cmxa oebuild_tool.ml"
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

(** ocamleditor *)
let ocamleditor () =
  cp (if use_modified_gtkThread then "gtkThread3.ml" else "gtkThread4.ml") "gtkThread2.ml";
  lexyacc();
  kprintf run
    "%s ocamleditor.ml%s -thread -lflags \"-linkall%s\" -cflags \"-w syumx%s%s\" -I \"%s\" -l \"%s\""
      oebuild_command
      (if !prof then " -prof" else "")
      debug
      debug
      (if !annot then " -annot" else "")
      includes
      libs

(** compiler_libs *)
let compiler_libs () =
  let ocaml_src_path = (Sys.getcwd()) // ocaml_src in
  if (Unix.lstat ocaml_src_path).Unix.st_kind = Unix.S_LNK then
    (kprintf failwith "%s is a symbolic link" ocaml_src_path);
  oebuild();
  let write files =
    let ochan = open_out "top.ml" in
    let finally () = close_out ochan in
    try
      output_string ochan (String.concat "\n" (List.map (fun x -> sprintf "open %s"
        (String.capitalize (Filename.chop_extension x))) files));
      finally()
    with ex -> (finally (); raise ex)
  in
  let compile ?(clean=false) root path =
    Sys.chdir path;
    let files = List.filter (fun x -> Filename.check_suffix x ".mli")
      (Array.to_list (Sys.readdir ".")) in
    if clean then begin
      List.iter begin fun x ->
        let cmi = sprintf "%s.cmi" (Filename.chop_extension x) in
        if Sys.file_exists cmi then (Sys.remove cmi);
        let cmo = sprintf "%s.cmo" (Filename.chop_extension x) in
        if Sys.file_exists cmo then (Sys.remove cmo);
      end files;
    end;
    if Sys.file_exists "lexer.mll" then (run "ocamllex lexer.mll");
    if Sys.file_exists "parser.mly" then (run "ocamlyacc parser.mly");
    write files;
    run ((".."//".."//"oebuild"//"oebuild") ^ " -I \"../utils ../parsing ../typing\" top.ml -c");
    Sys.remove "top.ml";
    Sys.remove ".oebuild";
    Sys.chdir root
  in
  let root = Sys.getcwd () in
  compile ~clean:true root (ocaml_src // "utils");
  compile ~clean:true root (ocaml_src // "parsing");
  compile ~clean:true root (ocaml_src // "typing")

(** install *)
let install () =
  if not is_win32 then begin
    let pixmaps = sprintf "%s/share/pixmaps/ocamleditor" !prefix in
    mkdir_p pixmaps;
    kprintf run "cp -vru ../pixmaps/* %s" pixmaps;
    let bin = sprintf "%s/bin" !prefix in
    mkdir_p bin;
    kprintf run "cp -v ocamleditor %s" bin;
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

(** with_compiler_libs *)
let with_compiler_libs () =
  annot := true;
  compiler_libs ();
  oeproc ();
  ocamleditor()

(** release *)
let release () =
  cleanall();
  if Sys.file_exists "ocaml-src" then (kprintf run "RMDIR /S /Q ocaml-src");
  Sys.chdir "..";
  let name = Filename.basename (Sys.getcwd ()) in
  Sys.chdir "..";
  kprintf remove_file "%s.tar.gz" name;
  kprintf run "mv -f %s/%s.xml %s/%s.tmp.xml" name name name name;
  kprintf run "cp %s/%s.release.xml %s/%s.xml" name name name name;
  kprintf run "tar --mode=755 -cf %s.tar %s/src %s/pixmaps" name name name;
  kprintf run "tar --mode=655 -rf %s.tar %s/README %s/NEWS %s/COPYING %s/%s.xml %s/ocamleditor.nsi %s/build.ml %s/header"
    name name name name name name name name name;
  kprintf run "gzip -c %s.tar > %s.tar.gz" name name;
  kprintf Sys.remove "%s.tar" name;
  kprintf run "mv -f %s/%s.tmp.xml %s/%s.xml" name name name name;
  kprintf Sys.chdir "%s/src" name;
  kprintf run "mkdir ocaml-src";
  kprintf run "cp -ru ..\\..\\ocaml-src .";
  with_compiler_libs()

(** all *)
let all () =
  annot := true;
  oebuild ();
  oeproc ();
  ocamleditor()

(** icons *)
let icons () =
  let pixmaps = ".." // "pixmaps" in
  let files = Array.to_list (Sys.readdir pixmaps) in
  let files = List.filter (fun x -> Filename.check_suffix x ".png") files in
  ignore (Sys.command "cat ../header > icons.ml");
  let filename = "icons.ml" in
  let ochan = open_out_gen [Open_append; Open_binary] 0o644 filename in
  try
    fprintf ochan "let (//) = Filename.concat\n\nlet create pixbuf = GMisc.image ~pixbuf ()\n\n";
    List.iter begin fun file ->
      let new_name = Str.global_replace (Str.regexp "-") "_" file in
      Sys.rename (pixmaps // file) (pixmaps // new_name);
      let icon_name = Filename.basename (Filename.chop_extension new_name) in
      fprintf ochan "let %s = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // \"%s\")\n" icon_name new_name
    end files;
    close_out_noerr ochan;
  with _ -> close_out_noerr ochan


(** Main -------------------------------------------------------------------- *)
let _ = begin
  try
    let mktarget f x =
      fun () ->
        let cwd = Sys.getcwd () in
        Sys.chdir "src";
        can_compile_native := if is_win32 then Sys.command "ML" = 0 else !can_compile_native;
        printf "Installation prefix ..................... : %s\n%!" !prefix;
        printf "Native .................................. : %b\n%!" !can_compile_native;
        printf "use_modified_gtkThread .................. : %b\n%!" use_modified_gtkThread;
        printf "ccopt ................................... : %s\n%!" ccopt;
        f x;
        Sys.chdir cwd
    in
    let target_func = ref (mktarget all ()) in
    let target f x = target_func := mktarget f x in
    let speclist = [
      ("-all",                Unit (target all),                " Build OCamlEditor (default)");
      ("-with-compiler-libs", Unit (target with_compiler_libs), " Compile the OCaml compiler libraries and build OCamlEditor");
      ("-install",            Unit (target install),            " Install OCamlEditor (Unix only, you may need superuser permissions)");
      ("-prefix",             Set_string prefix,                (sprintf " Installation prefix (default is %s)" !prefix));
      ("-clean",              Unit (target clean),              " Remove object files");
      ("-cleanall",           Unit (target cleanall),           " Remove all the build output");
      ("-oebuild",            Unit (target oebuild),            " (undocumented)");
      ("-oeproc",             Unit (target oeproc),             " (undocumented)");
      ("-compiler-libs",      Unit (target compiler_libs),      " (undocumented)");
      ("-lexyacc",            Unit (target lexyacc),            " (undocumented)");
      ("-icons",              Unit (target icons),              " (undocumented)");
      ("-ocamleditor",        Unit (target ocamleditor),        " (undocumented)");
      ("-uninstall",          Unit (target uninstall),          " (undocumented)");
      ("-release",            Unit (target release),            " (undocumented)");
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










