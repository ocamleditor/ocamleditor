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
  Printf.printf "\n%s %s\n%s\n\n%!" About.program_name About.version copyright;
  let ocaml_version = Str.global_replace
    (Str.regexp "\n") " - " (Str.global_replace (Str.regexp "\n$") "" (Cmd.expand "ocamlc -v")) in
  let a, b, c = GMain.Main.version in
  Printf.printf "---------------------------------------------------------------\n%!" ;
  let properties = [
    "OCaml Version", ocaml_version;
    "OCamlEditor User Home", ocamleditor_user_home;
    "pixmaps", App_config.application_pixmaps;
    "oebuild", oebuild_command;
  ] @
  (if is_win32 then ["oeproc", oeproc_command] else []) @ [
    "dot", (Opt.default dot_version "<Not Found>");
    "GTK Version", (sprintf "%d.%d.%d" a b c);
    "Glib Charset", (sprintf "%b, %s" Convert.glib_is_utf8 Convert.glib_charset);
    "Locale Charset", (sprintf "%b, %s" Convert.locale_is_utf8 Convert.locale_charset);
    "Default Charset", (sprintf "%b, %s" Convert.is_utf8 Convert.default_charset);
  ] in
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
  List.iter (fun (n, v) -> printf "%s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;
  Printf.printf "---------------------------------------------------------------\n%!" ;
  print_newline();
end

let main () = begin
  Browser.browser#window#present();
  (*
    THE FOLLOWING LINE IS PROCESSED BY "tools/prepare_build", DO NOT EDIT.
  *)
  (*Dot_viewer.device := (module Dot_viewer_svg.SVG : Dot_viewer.DEVICE);*)

  GtkThread2.main ();
end

