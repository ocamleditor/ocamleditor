(*

  OCamlEditor
  Copyright (C) 2010, 2011 Francesco Tovagliari

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

let _ = if true || Oe_config.ocamleditor_debug then begin
  let a, b, c = GMain.Main.version in
  let rpad s = Miscellanea.rpad (s ^ " ") '.' 40 in
  printf "%-40s : %d.%d.%d\n" (rpad "GTK version") a b c;
  printf "%-40s : %s\n" (rpad "OCamlEditor User Home") Oe_config.ocamleditor_user_home;
(*  printf "%-40s : %s\n" (rpad "OCaml Std Library") Oe_config.ocamllib;
  printf "%-40s : %s\n" (rpad "OCaml Toplevel") Oe_config.ocaml_command;
  printf "%-40s : %s\n" (rpad "ocamldep") Dep.ocamldep;*)
  printf "%-40s : %s\n" (rpad "oebuild") Oe_config.oebuild_command;
  if Oe_config.is_win32 then (printf "%-40s : %s\n" (rpad "oeproc") Oe_config.oeproc_command);
  printf "%-40s : %s\n" (rpad "pixmaps") Oe_config.ocamleditor_pixmaps;
  printf "%-40s : %b, %s\n" (rpad "Glib Charset") Convert.glib_is_utf8 Convert.glib_charset;
  printf "%-40s : %b, %s\n" (rpad "Locale Charset") Convert.locale_is_utf8 Convert.locale_charset;
  printf "%-40s : %b, %s\n" (rpad "Default Charset") Convert.is_utf8 Convert.default_charset;
  print_newline();
end

let main () = begin
  Browser.browser#window#present();
  GtkThread2.main ();
end

let _ = Printexc.print main ()
























