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

type status =
  { mutable added : int
  ; mutable modified : int
  ; mutable deleted : int
  }

module Log = Common.Log.Make (struct
  let prefix = "Git"
end)

let _ = Log.set_verbosity `ERROR

let string_of_status status =
  match status with
  | Some s ->
      Printf.sprintf " [+%d ~%d -%d]" s.added s.modified s.deleted
  | _ ->
      ""


let with_status f =
  match Oe_config.git_version with
  | None ->
      f None
  | _ ->
      let status = { added = 0; modified = 0; deleted = 0 } in
      let process_in =
        let re = Str.regexp "\\([ MADRCU?!]\\)\\([ MDU?!]\\) +\\(.*\\)?" in
        Spawn.iter_chan (fun ic ->
            let line = input_line ic in
            if Str.string_match re line 0
            then
              let x = Str.matched_group 1 line in
              let y = Str.matched_group 2 line in
              match (x, y) with
              | _, "M" | "M", _ ->
                  status.modified <- status.modified + 1
              | "?", _ ->
                  status.added <- status.added + 1
              | "D", _ ->
                  status.deleted <- status.deleted + 1
              | _ ->
                  Log.println `ERROR "Unparseable git message: %s%s" x y)
      in
      let has_errors = ref false in
      let process_err =
        Spawn.iter_chan (fun ic ->
            let _ = input_line ic in
            has_errors := true)
      in
      Spawn.async
        ~verbose:false
        ~process_in
        ~process_err
        ~at_exit:(fun _ ->
          if !has_errors
          then GtkThread.async f None
          else GtkThread.async f (Some status))
        "git status --porcelain=1"
      |> ignore
