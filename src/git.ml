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


open Printf

type status = {
  mutable added    : int;
  mutable modified : int;
  mutable deleted  : int
}

let string_of_status status = match status with Some s -> sprintf " [+%d ~%d -%d]" s.added s.modified s.deleted | _ -> ""

let with_status =
  let last_check = ref 0.0 in
  fun  f ->
    match Oe_config.git_version with
      | None -> f None
      | _ ->
        let now = Unix.gettimeofday() in
        if now -. !last_check > 3.0 then begin
          last_check := now;
          let status = { added = 0; modified = 0; deleted = 0 } in
          let process_in =
            let re = Str.regexp "\\([ MADRCU?!]\\)\\([ MDU?!]\\) +\\(.*\\)?" in
            Spawn.loop begin fun ic ->
              let line = input_line ic in
              if Str.string_match re line 0 then begin
                let y = Str.matched_group 2 line in
                begin
                  match y with
                    | "M" -> status.modified <- status.modified + 1
                    | "?" -> status.added <- status.added + 1
                    | "D" -> status.deleted <- status.deleted + 1
                    | _ -> assert false
                end;
              end;
            end
          in
          let has_errors = ref false in
          let process_err =
            Spawn.loop begin fun ic ->
              let _ = input_line ic in
              has_errors := true;
            end
          in
          Spawn.async ~process_in ~process_err
            ~at_exit:begin fun _ ->
              if !has_errors then GtkThread.async f None
              else GtkThread.async f (Some status)
            end "git" [|"status"; "--porcelain"|] |> ignore;
          last_check := now
        end
