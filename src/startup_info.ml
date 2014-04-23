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

module Config = Oe_config

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
    "OCamlEditor User Home", Config.ocamleditor_user_home;
    "icons", App_config.application_icons;
    "plugins", App_config.application_plugins;
    "oebuild", Config.oebuild_command;
  ] @
  (if Config.is_win32 then [
     "oeproc", Config.oeproc_command
   ] else [
     "xdg-open", (Opt.default Config.xdg_open_version "<Not Found>");
   ]) @ [
    "dot", (Opt.default Config.dot_version "<Not Found>");
    "ocp-indent", (Opt.default Config.ocp_indent_version "<Not Found>");
    "git", (Opt.default Config.git_version "<Not Found>");
    "GTK Version", (sprintf "%d.%d.%d" a b c);
    "Locale", (Opt.default (App_config.get_locale ()) "<Not Found>");
    "Charset", (let x, charset = Glib.Convert.get_charset () in sprintf "%b, %s" x charset);
    "Backtrace status", (sprintf "%b" (Printexc.backtrace_status ()));
  ] in
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
  List.iter (fun (n, v) -> printf "%s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;
  Printf.printf "---------------------------------------------------------------\n%!" ;
  print_newline();
end
