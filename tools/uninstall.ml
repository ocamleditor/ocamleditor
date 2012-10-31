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
  end else prerr_endline "This script is not available under Windows";;

let _ = main ~default_target:uninstall ~options:[
  "-prefix", Set_string prefix, (sprintf " Installation prefix (Unix only, default is %s)" !prefix);
] ()
