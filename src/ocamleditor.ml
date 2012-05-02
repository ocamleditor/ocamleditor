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

open Unix
open Printf
open Miscellanea

let _ = if true || Common.application_debug then begin
  let ocaml_version = Str.global_replace
    (Str.regexp "\n") " - " (Str.global_replace (Str.regexp "\n$") "" (Cmd.expand "ocamlc -v")) in
  let a, b, c = GMain.Main.version in
  let properties = [
    "OCaml Version", ocaml_version;
    "OCamlEditor User Home", Oe_config.ocamleditor_user_home;
    "oebuild", Oe_config.oebuild_command;
  ] @
  (if Oe_config.is_win32 then ["oeproc", Oe_config.oeproc_command] else []) @ [
    "pixmaps", Common.application_pixmaps;
    "GTK Version", (sprintf "%d.%d.%d" a b c);
    "Glib Charset", (sprintf "%b, %s" Convert.glib_is_utf8 Convert.glib_charset);
    "Locale Charset", (sprintf "%b, %s" Convert.locale_is_utf8 Convert.locale_charset);
    "Default Charset", (sprintf "%b, %s" Convert.is_utf8 Convert.default_charset);
  ] in
  let maxlength = List.fold_left (fun cand (x, _) -> let len = String.length x in max cand len) 0 properties in
  List.iter (fun (n, v) -> printf "%s : %s\n" (rpad (n ^ " ") '.' maxlength) v) properties;
  print_newline();
end

let main () = begin
  Browser.browser#window#present();
  GtkThread2.main ();
end

let _ =
  Printexc.print main ()
























