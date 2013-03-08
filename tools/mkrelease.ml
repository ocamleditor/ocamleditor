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


#cd "tools"
#use "scripting.ml"

open Printf

let mkrelease () =
  if is_win32 then begin
    pushd "..";
    run "ocaml tools/prepare_build.ml -generate-oebuild-script";
    run "ocaml build.ml distclean";
    popd();
    Sys.chdir "..";
    let name = Filename.basename (Sys.getcwd ()) in
    let version = match get_lines_from_file ~filename:"VERSION" [1] with (_, x) :: [] -> x | _ -> assert false in
    let package = sprintf "%s-%s" name version in
    Sys.chdir "..";
    kprintf remove_file "%s.tar.gz" package;
    kprintf run "mklink /d %s %s" package name;
    kprintf run "tar --mode=755 -cf %s.tar %s/src %s/icons %s/tools" package package package package;
    kprintf run "tar --mode=655 -rf %s.tar %s/README %s/NEWS %s/COPYING %s/%s.project %s/ocamleditor.nsi %s/build.ml %s/header %s/VERSION"
      package package package package package name package package package package;
    kprintf run "gzip -c %s.tar > %s.tar.gz" package package;
    kprintf Sys.remove "%s.tar" package;
    kprintf run "rmdir %s" package;
    kprintf Sys.chdir "%s/src" name;
    pushd "..";
    run "ocaml build.ml build ocamleditor";
    popd()
  end;;

let _ = main ~default_target:mkrelease ~options:[] ()
