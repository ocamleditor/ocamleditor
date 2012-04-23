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


#use "scripting.ml"

open Printf
open Arg

let has_native             = ref false
let ccopt                  = ref (try Unix.getenv "OCAMLEDITOR_CCOPT" with Not_found -> "")
let use_modified_gtkThread = ref
  (try ignore (Unix.getenv "OCAMLEDITOR_GTKTHREAD"); true with Not_found -> true)
let ocaml_src              = "ocaml-src"
let ext                    = if is_win32 then ".exe" else ""
let oebuild_name           = sprintf "oebuild%s" ext
let oebuild_command        = "oebuild" // oebuild_name
let (!!)                   = sprintf "(*%s*)";;

(** common *)
let common () =
  pushd "common";
  kprintf run
    "ocamlc -a -o common.cma -annot -thread -w syumx str.cma unix.cma threads.cma \
cmd.ml miscellanea.ml file.ml quote.ml ocaml_config.ml cmd_line_args.ml dep.mli dep.ml list_opt.ml common.ml";
  if !has_native then begin
    kprintf run
      "ocamlopt -a -o common.cmxa -annot -thread -w syumx cmd.ml miscellanea.ml file.ml quote.ml \
ocaml_config.ml cmd_line_args.ml dep.mli dep.ml list_opt.ml common.ml";
  end;
  popd();;

(** oebuild *)
let oebuild () =
  common();
  pushd "oebuild";
  kprintf run "ocamlc -a -o oebuildlib.cma -thread -w syumx -annot -I ../common str.cma unix.cma threads.cma common.cma oebuild_util.ml oebuild_table.mli oebuild_table.ml oebuild.ml";
  kprintf run "ocamlc -o %s -thread -w syumx -I ../common str.cma unix.cma threads.cma common.cma oebuildlib.cma oebuild_tool.ml"
    oebuild_name;
  if !has_native then begin
    kprintf run "ocamlopt -a -o oebuildlib.cmxa -thread -w syumx -I ../common oebuild_util.ml oebuild_table.mli oebuild_table.ml oebuild.ml";
    kprintf run "ocamlopt -o oebuild.opt%s %s -thread -w syumx -I ../common str.cmxa unix.cmxa threads.cmxa common.cmxa oebuildlib.cmxa oebuild_tool.ml"
      ext !ccopt;
  end;
  popd();;

(** compiler_libs *)
let compiler_libs () =
  oebuild();
  let ocaml_src_path = (Sys.getcwd()) // ocaml_src in
  if (Unix.lstat ocaml_src_path).Unix.st_kind <> Unix.S_LNK then begin
    let write top files =
      let ochan = open_out top in
      let finally () = close_out ochan in
      try
        output_string ochan (String.concat "\n" (List.map (fun x -> sprintf "open %s"
          (String.capitalize (Filename.chop_extension x))) files));
        finally()
      with ex -> (finally (); raise ex)
    in
    let compile ?(clean=false) root path =
      Sys.chdir path;
      let files = List.filter (fun x -> Filename.check_suffix x ".mli" || Filename.check_suffix x ".ml")
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
      if Sys.file_exists "linenum.mll" then (run "ocamllex linenum.mll");
      if Sys.file_exists "parser.mly" then (run "ocamlyacc parser.mly");
      let top = sprintf "top_%s.ml" (Filename.basename path) in
      write top files;
      run ((".."//".."//"oebuild"//"oebuild") ^ " -I \"../utils ../parsing ../typing\" " ^ top ^ " -c");
      Sys.remove top;
      Sys.remove ".oebuild";
      Sys.chdir root;
    in
    let root = Sys.getcwd () in
    compile ~clean:true root (ocaml_src // "utils");
    compile ~clean:true root (ocaml_src // "parsing");
    compile ~clean:true root (ocaml_src // "typing");
  end;;

(** lex_yacc *)
let lex_yacc () =
  run "ocamllex annot_lexer.mll";
  run "ocamlyacc annot_parser.mly";
  run "ocamllex err_lexer.mll";
  run "ocamlyacc err_parser.mly";;

(** substitute *)
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

(** prepare_build *)
let prepare_build () =
  compiler_libs();
  if Sys.ocaml_version >= "3.12.0" then begin
    substitute ~filename:((Sys.getcwd()) // "odoc.ml") [!!"@REPL_1@", "| Target _ -> ()"; !!"@REPL_2@", ", _"];
  end;
  lex_yacc();;

(** mkicons *)
let mkicons () =
  let pixmaps = ".." // "pixmaps" in
  let files = Array.to_list (Sys.readdir pixmaps) in
  let files = List.filter (fun x -> Filename.check_suffix x ".png") files in
  ignore (Sys.command "cat ../header > icons/icons.ml");
  let filename = "icons/icons.ml" in
  let ochan = open_out_gen [Open_append; Open_binary] 0o644 filename in
  begin
    try
      fprintf ochan "let (//) = Filename.concat\n\nlet create pixbuf = GMisc.image ~pixbuf ()\n\n";
      List.iter begin fun file ->
        let new_name = Str.global_replace (Str.regexp "-") "_" file in
        Sys.rename (pixmaps // file) (pixmaps // new_name);
        let icon_name = Filename.basename (Filename.chop_extension new_name) in
        fprintf ochan "let %s = GdkPixbuf.from_file (Common.application_pixmaps // \"%s\")\n" icon_name new_name
      end files;
      close_out_noerr ochan;
    with _ -> close_out_noerr ochan;
  end;;

(** clean_lex_yacc *)
let clean_lex_yacc () =
  remove_file "annot_lexer.ml";
  remove_file "annot_parser.ml";
  remove_file "annot_parser.mli";
  remove_file "err_lexer.ml";
  remove_file "err_parser.ml";
  remove_file "err_parser.mli";;

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
  clean_lex_yacc();;

(** distclean *)
let distclean () =
  clean_lex_yacc();
  clean ~all:true ();
  kprintf run "%s *.exe *.bak *.annot *~" rm;
  List.iter remove_file [
    "geometry";
    "Thumbs.db";
    ".." // "pixmaps" // "Thumbs.db";
    "oebuild"//"oebuild.exe";
    "oebuild"//"oebuild.opt.exe";
    "oebuild"//"oebuild";
    "oebuild"//"oebuild.opt";
    "oeproc"//"oeproc";
    "oeproc"//"oeproc.opt";
  ];
  kprintf run "%s %s" rmr (Filename.parent_dir_name // "bak");
  if Sys.file_exists (Filename.parent_dir_name // ".tmp") then
    (kprintf run "%s %s" rmr (Filename.parent_dir_name // ".tmp" // "*"));;

(** generate_oebuild_script *)
let generate_oebuild_script () =
  run "ocaml -I common str.cma unix.cma common.cma generate_oebuild_script.ml";;

(** mkrelease *)
let mkrelease () =
  generate_oebuild_script();
  distclean();
  if Sys.file_exists ocaml_src then (kprintf run "RMDIR /S /Q %s" ocaml_src);
  Sys.chdir "..";
  let name = Filename.basename (Sys.getcwd ()) in
  Sys.chdir "..";
  kprintf remove_file "%s.tar.gz" name;
  kprintf run "mv -f %s/%s.xml %s/%s.tmp.xml" name name name name;
  kprintf run "cp %s/%s.release.xml %s/%s.xml" name name name name;
  kprintf run "tar --mode=755 -cf %s.tar %s/src %s/pixmaps %s/build" name name name name;
  kprintf run "tar --mode=655 -rf %s.tar %s/README %s/NEWS %s/COPYING %s/%s.xml %s/ocamleditor.nsi %s/build.ml %s/header"
    name name name name name name name name name;
  kprintf run "gzip -c %s.tar > %s.tar.gz" name name;
  kprintf Sys.remove "%s.tar" name;
  kprintf run "mv -f %s/%s.tmp.xml %s/%s.xml" name name name name;
  kprintf Sys.chdir "%s/src" name;
  kprintf run "mkdir %s" ocaml_src;
  kprintf run "cp -ru ..\\..\\%s ." ocaml_src;
  pushd "..";
  run "ocaml build.ml -with-compiler-libs";
  popd();;

(** Main *)
let _ = main ~dir:"../src" ~targets:[
  "-prepare-build",           prepare_build,           " (undocumented)";
  "-compiler-libs",           compiler_libs,           " (undocumented)";
  "-lex-yacc",                lex_yacc,                " (undocumented)";
  "-mkicons",                 mkicons,                 " (undocumented)";
  "-mkrelease",               mkrelease,               " (undocumented)";
  "-generate-oebuild-script", generate_oebuild_script, " (undocumented)";
  "-clean-lex-yacc",          clean_lex_yacc,          " (undocumented)";
  "-clean",                   (clean ?all:None),       " (undocumented)";
  "-distclean",               distclean,               " (undocumented)";
] ~options:[
  "-dont-use-modified-gtkThread",  Clear use_modified_gtkThread,    "";
  "-has-native",              Bool (fun x -> has_native := x), "{true|false} Whether native compilation is supported (default: false)";
  "-ccopt",                   Set_string ccopt,                " (default: \"\")";
] ()
