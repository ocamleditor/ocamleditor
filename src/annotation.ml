(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

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

open Miscellanea

(** preload *)
let preload ~project =
  let name = "Parsing \xC2\xAB.annot\xC2\xBB files..." in
  let finally () = GtkThread2.async Activity.remove name in
  ignore (Thread.create begin fun () ->
      GtkThread2.async (Activity.add Activity.Annot) name;
      try
        let src_path = Project.path_src project in
        let files = File_util.readdirs (Some (fun x -> x ^^^ ".ml")) src_path in
        List.iter (fun filename -> Binannot_ident_scan.scan ~project ~filename ()) files;
        finally()
      with ex -> begin
          Printf.eprintf "File \"annotation.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
          finally()
        end
    end ())
;;






