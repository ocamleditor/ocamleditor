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


(** create *)
let create task =
  Task.handle begin fun ~env ~dir ~prog ~args ->
    let proc = Process.create ~env ~prog ~args () in (* 2>&1 *)
    proc, fun () ->
      let cwd =
        if task.Task.et_dir <> "" then
          let old = Sys.getcwd () in
          Sys.chdir task.Task.et_dir;
          Some old
        else None
      in
      Process.start proc;
      match cwd with Some old -> Sys.chdir old | _ -> ()
  end task;;
