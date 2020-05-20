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

let re_tmp = Miscellanea.regexp (Printf.sprintf "File \"..[\\/]%s[\\/]" Prj.default_dir_tmp)
let (!!) = Filename.quote

(** compile_buffer *)
let compile_buffer ~project ~editor ~page ?(join=false) () =
  let activity_name = "Compiling " ^ page#get_filename ^ "..." in
  Activity.add Activity.Compile_buffer activity_name;
  try
    match page#buffer#save_buffer ?filename:None () with
      | _, None -> ()
      | _, Some (_, relpath) ->
        (* Compile *)
        let args =
          let tmp = "../" ^ Prj.default_dir_tmp in
          Array.concat [
            project.Prj.autocomp_dflags;
            (Array.of_list (Miscellanea.split " +" project.Prj.autocomp_cflags));
            [| "-error-style"; "short"; "-I"; tmp |];
            (Array.of_list (Miscellanea.split " +" (Project.get_search_path_i_format project)));
            [|tmp ^ "/" ^ relpath|];
          ]
        in
        let compiler_output = Buffer.create 101 in
        let process_err stderr =
          let line = input_line stderr in
          let line = Str.replace_first re_tmp "File \"" line in
          Buffer.add_string compiler_output (Miscellanea.rtrim line);
          Buffer.add_char compiler_output '\n';
        in
        let at_exit _ =
          let errors = Error.parse_string (Buffer.contents compiler_output) in
          GtkThread2.async page#error_indication#apply_tag errors;
          (* Outline *)
          let no_errors = errors.Oe.er_errors = [] in
          if editor#show_outline then begin
            Gmisclib.Idle.add ~prio:100 begin fun () ->
              match page#outline with
                | None ->
                  let ol = new Cmt_view.widget ~editor ~page () in
                  ol#load ();
                  Gaux.may page#outline ~f:(fun x -> x#destroy());
                  page#set_outline (Some ol);
                  editor#with_current_page begin fun current_page ->
                    if current_page#get_oid = page#get_oid then (editor#pack_outline ol#coerce)
                  end;
                | Some ol -> if no_errors then ol#load ()
            end
          end;
          Activity.remove activity_name;
        in
        let process_err = Spawn.loop process_err in
        if join then
          Spawn.sync ~at_exit ~process_err project.Prj.autocomp_compiler args |> ignore
        else
          Spawn.async ~at_exit ~process_err project.Prj.autocomp_compiler args |> ignore
    (*end ()*)
  with ex -> begin
    Printf.eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ());
    ()
  end
