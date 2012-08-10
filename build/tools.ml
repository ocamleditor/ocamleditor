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

let required_ocaml_version = "4.00.0"
let has_native             = ref false
let ccopt                  = ref (try Unix.getenv "OCAMLEDITOR_CCOPT" with Not_found -> "")
let use_modified_gtkThread = ref true
let ext                    = if is_win32 then ".exe" else ""
let oebuild_name           = sprintf "oebuild%s" ext
let oebuild_command        = "oebuild" // oebuild_name
let (!!)                   = sprintf "(*%s*)";;

(** lex_yacc *)
let lex_yacc () =
  run "ocamllex annot_lexer.mll";
  run "ocamlyacc annot_parser.mly";
  run "ocamllex err_lexer.mll";
  run "ocamlyacc err_parser.mly";;

(** generate_oebuild_script *)
let generate_oebuild_script () =
  run "ocaml -I common str.cma unix.cma common.cma generate_oebuild_script.ml";;

(** prepare_build *)
let prepare_build () =
  if Sys.ocaml_version < required_ocaml_version then
    eprintf "You are using OCaml-%s but version %s is required." Sys.ocaml_version required_ocaml_version;
  cp ~echo:true (if !use_modified_gtkThread then "gtkThread3.ml" else "gtkThread4.ml") "gtkThread2.ml";
  lex_yacc();
  generate_oebuild_script();;

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

(** mkrelease *)
let mkrelease () =
  generate_oebuild_script();
  distclean();
  Sys.chdir "..";
  let name = Filename.basename (Sys.getcwd ()) in
  let version = get_line_from_file ~filename:"VERSION" 1 in
  let package = sprintf "%s-%s" name version in
  Sys.chdir "..";
  kprintf remove_file "%s.tar.gz" package;
  kprintf run "mklink /d %s %s" package name;
  kprintf run "mv -f %s/%s.project %s/%s.tmp.project" name name name name;
  kprintf run "cp %s/%s.project.release %s/%s.project" name name name name;
  kprintf run "tar --mode=755 -cf %s.tar %s/src %s/pixmaps %s/build" package package package package;
  kprintf run "tar --mode=655 -rf %s.tar %s/README %s/NEWS %s/COPYING %s/%s.project %s/ocamleditor.nsi %s/build.ml %s/header %s/VERSION"
    package package package package package name package package package package;
  kprintf run "gzip -c %s.tar > %s.tar.gz" package package;
  kprintf Sys.remove "%s.tar" package;
  kprintf run "mv -f %s/%s.tmp.project %s/%s.project" name name name name;
  kprintf run "rmdir %s" package;
  kprintf Sys.chdir "%s/src" name;
  pushd "..";
  run "ocaml build.ml -annot";
  popd();;

(** Main *)
let _ = main ~dir:"../src" ~targets:[
  "-prepare-build",           prepare_build,           " (undocumented)";
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
