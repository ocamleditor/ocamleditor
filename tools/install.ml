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

let prefix     = ref "/usr/local"
let ext        = if is_win32 then ".exe" else ""
let has_native = ref false

let install () =
  if not is_win32 then begin
    let pixmaps = sprintf "%s/share/pixmaps/ocamleditor" !prefix in
    mkdir_p pixmaps;
    kprintf run "cp -vru ../pixmaps/* %s" pixmaps;
    let bin = sprintf "%s/bin" !prefix in
    mkdir_p bin;
    let filename = if Sys.file_exists "ocamleditor.opt" then "ocamleditor.opt" else "ocamleditor" in
    kprintf run "cp -v %s %s/ocamleditor" filename bin;
    kprintf run "cp -v oebuild/oebuild%s %s" ext bin;
    if !has_native then begin
      kprintf run "cp -v oebuild/oebuild%s.opt %s" ext bin;
    end;
  end else begin
    let exit_code = kprintf Sys.command "\"%s\" ..\\ocamleditor.nsi" (Filename.quote "%ProgramFiles(x86)%\\NSIS\\makensis") in
    match exit_code with
      | 0 ->
        let version = get_line_from_file ~filename:"../VERSION" 1 in
        let cmd = sprintf "..\\ocamleditor-%s" version in
        ignore (Sys.command cmd)
      | _ -> prerr_endline "This script is not available under Windows.
To install OCamlEditor, please use the included ocamleditor.nsi script.
You will need the free NSIS install system (http://nsis.sourceforge.net).";
  end;;

let _ = main ~default_target:install ~options:[
  "-prefix", Set_string prefix, (sprintf " Installation prefix (Unix only, default is %s)" !prefix);
  "-has-native", Bool (fun x -> has_native := x), "{true|false} Whether native compilation is supported (default: false)";
] ()
