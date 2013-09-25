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


open Printf
open Miscellanea
open Oe_config

let _ = if true || App_config.application_debug then begin
  let copyright = Str.replace_first (Str.regexp_string "©") "(c)" About.copyright in
  Printf.printf "\n%s %s\n\n%!" About.program_name About.version (*copyright*);
  let ocaml_version = Str.global_replace
    (Str.regexp "\n") " - " (Str.global_replace (Str.regexp "\n$") "" (Cmd.expand "ocamlc -v")) in
  let a, b, c = GMain.Main.version in
  Printf.printf "---------------------------------------------------------------\n%!" ;
  let properties = [
    "OCaml Version", ocaml_version;
    "OCamlEditor User Home", ocamleditor_user_home;
    "icons", App_config.application_icons;
    "plugins", App_config.application_plugins;
    "oebuild", oebuild_command;
  ] @
  (if is_win32 then [
     "oeproc", oeproc_command
   ] else [
     "xdg-open", (Opt.default xdg_open_version "<Not Found>");
   ]) @ [
    "dot", (Opt.default dot_version "<Not Found>");
    "ocp-indent", (Opt.default ocp_indent_version "<Not Found>");
    "GTK Version", (sprintf "%d.%d.%d" a b c);
    (*"plink", (Opt.default plink_version "<Not Found>");*)
    (*"Glib Charset", (sprintf "%b, %s" Convert.glib_is_utf8 Convert.glib_charset);
    "Locale Charset", (sprintf "%b, %s" Convert.locale_is_utf8 Convert.locale_charset);
    "Default Charset", (sprintf "%b, %s" Convert.is_utf8 Convert.default_charset);*)
    "Backtrace status", (sprintf "%b" (Printexc.backtrace_status ()))
  ] in
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
  List.iter (fun (n, v) -> printf "%s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;
  Printf.printf "---------------------------------------------------------------\n%!" ;
  print_newline();
end

(** main *)
let main () = begin
  ignore (Plugin.load "dot_viewer_svg.cma");
  Browser.browser#window#present();


(*  let window = Browser.browser#window in
  begin
    match window#misc#get_property "screen" with
      | `OBJECT (Some screen) ->
        let screen = Gobject.try_cast screen "GdkScreen" in
        Printf.printf "w=%d; h=%d\n%!" (Gdk.Screen.width ~screen ()) (Gdk.Screen.height ~screen ());
        (*window#move ~x:0 ~y:0;
        window#resize ~width:(Gdk.Screen.width ~screen ()) ~height:(Gdk.Screen.height ~screen () - 1);*)
      | _ -> Printf.printf "***\n%!" ;
  end;*)


  let stats() =
    let parse inchan =
      try
        while true do
          (*print_endline*) (input_line inchan) |> ignore
        done
      with End_of_file -> (flush_all ())
    in
    try
      let locale =
        try
          if Sys.win32 then begin
            let lines = Miscellanea.exec_lines "reg query \"hkcu\\Control Panel\\International\" /v LocaleName" in
            let lines = List.map String.trim lines in
            let locale =
              List.find (fun l -> Str.string_match (Str.regexp "LocaleName.+") l 0) lines
            in
            Str.string_match (Str.regexp ".*[\t ]\\([a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-]\\)") locale 0 |> ignore;
            Str.matched_group 1 locale
          end else begin
            let lines = Miscellanea.exec_lines "locale" in
            let locale =
              List.find (fun l -> Str.string_match (Str.regexp ".*=.+") l 0) lines
            in
            Str.string_match (Str.regexp ".*=\\(.*\\)") locale 0 |> ignore;
            Str.matched_group 1 locale
          end
        with ex -> "<Locale_not_found>"
      in
      let terms = [
        Glib.get_user_name();
        About.build_id;
        (sprintf "%s-%s" About.program_name About.version);
        Sys.os_type;
        locale
      ] in
      let address, port = Check_for_updates.website_name, 80 in
      let (sock, inchan, outchan) = Check_for_updates.init_socket address port in
      let path = sprintf "%s?utm_source=%s&utm_medium=app_medium&utm_term=%s&utm_campaign=stat"
          Check_for_updates.website About.program_name (String.concat "+" terms)
      in
      (*Printf.printf "%s\n%!" path;*)
      ignore (Check_for_updates.submit_request ~close:true "GET" path outchan);
      ignore (parse inchan);
      Unix.shutdown sock Unix.SHUTDOWN_ALL;
    with (Sys_error _) as ex -> begin
        Printf.fprintf stderr "%s\n%!" (Printexc.to_string ex)
      end
  in
  if Oe_config.stats_enabled && not App_config.application_debug then stats();

  GtkThread2.main ();
end

