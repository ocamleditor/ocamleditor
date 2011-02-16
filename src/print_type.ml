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

open Printf


let replace_greek descr =
  if !Preferences.preferences.Preferences.pref_compl_greek then begin
    Miscellanea.replace_all ~regexp:false [
      "-&gt;", " &#8594; ";
(*      "-&gt;", " <big>&#8594;</big> ";*)
      " *", " &#215;";
      "&apos;a", "&#945;";
      "&apos;b", "&#946;";
      "&apos;c", "&#947;";
      "&apos;d", "&#948;";
      "&apos;e", "&#949;";
      "&apos;f", "&#950;";
      "&apos;g", "&#951;";
      "&apos;h", "&#952;";
      "&apos;i", "&#953;";
      "&apos;j", "&#954;";
      "&apos;k", "&#955;";
      "&apos;l", "&#956;";
      "&apos;m", "&#957;";
      "&apos;n", "&#958;";
      "&apos;o", "&#959;";
      "&apos;p", "&#960;";
      "&apos;q", "&#961;";
      "&apos;r", "&#962;";
      "&apos;s", "&#963;";
      "&apos;t", "&#964;";
      "&apos;u", "&#965;";
      "&apos;v", "&#966;";
      "&apos;w", "&#967;";
      "&apos;x", "&#968;";
      "&apos;y", "&#969;";
      (*"&apos;z", "&#970;";*)
    ] descr
  end else descr

let markup descr =
  let descr = replace_greek (Glib.Markup.escape_text descr) in
  Miscellanea.replace_all
    ["\\([ \n(]\\|^\\)\\([?]?[a-z_][a-z0-9_']*\\):", "\\1<i><small>\\2:</small></i>"] descr

let markup2 descr =
  let descr = replace_greek (Glib.Markup.escape_text descr) in
  Miscellanea.replace_all [
    "\\([ \n(]\\|^\\)\\([?]?[a-z_][a-z0-9_']*\\):",  "\\1<small><i>\\2:</i></small>";
    "\\([A-Z][A-Za-z0-9_']+\\)\\.", "<b><small>\\1</small></b>."
     ] descr

let markup3 descr =
  let descr = replace_greek (Glib.Markup.escape_text descr) in
  Miscellanea.replace_all [
    "\\([ \n(]\\|^\\)\\([?]?[a-z_][a-z0-9_']*\\):",  "\\1<i>\\2:</i>";
    "\\([A-Z][A-Za-z0-9_']+\\)\\.", "<b>\\1</b>.";
     ] descr


