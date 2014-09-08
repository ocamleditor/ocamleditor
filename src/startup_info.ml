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
open Printf

(** get_locale *)
let get_locale () =
  try
    if Sys.win32 then begin
      let lines = Cmd.exec_lines "reg query \"hkcu\\Control Panel\\International\" /v LocaleName" in
      let lines = List.map String.trim lines in
      let locale =
        List.find (fun l -> Str.string_match (Str.regexp "LocaleName.+") l 0) lines
      in
      Str.string_match (Str.regexp ".*[\t ]\\([a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-]\\)") locale 0 |> ignore;
      Some (Str.matched_group 1 locale)
    end else begin
      let lines = Cmd.exec_lines "locale" in
      let locale =
        List.find (fun l -> Str.string_match (Str.regexp ".*=.+") l 0) lines
      in
      Str.string_match (Str.regexp ".*=\\(.*\\)") locale 0 |> ignore;
      Some (Str.matched_group 1 locale)
    end
  with ex ->
    Printf.eprintf "File \"miscellanea.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    None

(** print *)
let print () = if true || App_config.application_debug then begin
  let copyright = Str.replace_first (Str.regexp_string "©") "(c)" About.copyright in
  Printf.printf "\n%s %s\n\n%!" About.program_name About.version (*copyright*);
  let ocaml_version = Str.global_replace
    (Str.regexp "\n") " - " (Str.global_replace (Str.regexp "\n$") "" (Cmd.expand "ocamlc -v")) in
  let a, b, c = GMain.Main.version in
  Printf.printf "---------------------------------------------------------------\n%!" ;
  let properties = [
    "OCaml Version", ocaml_version;
    "OCamlEditor User Home", App_config.ocamleditor_user_home;
    "icons", App_config.application_icons;
    "plugins", App_config.application_plugins;
    "oebuild", Oe_config.oebuild_command;
  ] @
  (if Sys.win32 then [
     "oeproc", Oe_config.oeproc_command
   ] else [
     "xdg-open", (Opt.default Oe_config.xdg_open_version "<Not Found>");
   ]) @ [
    "dot", (Opt.default Oe_config.dot_version "<Not Found>");
    "ocp-indent", (Opt.default Oe_config.ocp_indent_version "<Not Found>");
    "git", (Opt.default Oe_config.git_version "<Not Found>");
    "rc", (Opt.default Oe_config.rc "<Not Found>");
    "cvtres", (Opt.default Oe_config.cvtres "<Not Found>");
    "GTK Version", (sprintf "%d.%d.%d" a b c);
    "Locale", (Opt.default (get_locale ()) "<Not Found>");
    "Charset", (let x, charset = Glib.Convert.get_charset () in sprintf "%b, %s" x charset);
    "Backtrace status", (sprintf "%b" (Printexc.backtrace_status ()));
  ] in
  List.iter (printf "%s\n") (Text_util.dot_leaders properties);
  Printf.printf "---------------------------------------------------------------\n%!" ;
  print_newline();
end
