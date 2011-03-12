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

let greeks = [
  "&apos;a\\([0-9]*\\)", "&#945;<sub>\\1</sub>";
  "&apos;b\\([0-9]*\\)", "&#946;<sub>\\1</sub>";
  "&apos;c\\([0-9]*\\)", "&#947;<sub>\\1</sub>";
  "&apos;d\\([0-9]*\\)", "&#948;<sub>\\1</sub>";
  "&apos;e\\([0-9]*\\)", "&#949;<sub>\\1</sub>";
  "&apos;f\\([0-9]*\\)", "&#950;<sub>\\1</sub>";
  "&apos;g\\([0-9]*\\)", "&#951;<sub>\\1</sub>";
  "&apos;h\\([0-9]*\\)", "&#952;<sub>\\1</sub>";
  "&apos;i\\([0-9]*\\)", "&#953;<sub>\\1</sub>";
  "&apos;j\\([0-9]*\\)", "&#954;<sub>\\1</sub>";
  "&apos;k\\([0-9]*\\)", "&#955;<sub>\\1</sub>";
  "&apos;l\\([0-9]*\\)", "&#956;<sub>\\1</sub>";
  "&apos;m\\([0-9]*\\)", "&#957;<sub>\\1</sub>";
  "&apos;n\\([0-9]*\\)", "&#958;<sub>\\1</sub>";
  "&apos;o\\([0-9]*\\)", "&#959;<sub>\\1</sub>";
  "&apos;p\\([0-9]*\\)", "&#960;<sub>\\1</sub>";
  "&apos;q\\([0-9]*\\)", "&#961;<sub>\\1</sub>";
  "&apos;r\\([0-9]*\\)", "&#962;<sub>\\1</sub>";
  "&apos;s\\([0-9]*\\)", "&#963;<sub>\\1</sub>";
  "&apos;t\\([0-9]*\\)", "&#964;<sub>\\1</sub>";
  "&apos;u\\([0-9]*\\)", "&#965;<sub>\\1</sub>";
  "&apos;v\\([0-9]*\\)", "&#966;<sub>\\1</sub>";
  "&apos;w\\([0-9]*\\)", "&#967;<sub>\\1</sub>";
  "&apos;x\\([0-9]*\\)", "&#968;<sub>\\1</sub>";
  "&apos;y\\([0-9]*\\)", "&#969;<sub>\\1</sub>";
  (*"&apos;z\\([0-9]*\\)", "&#970;<sub>\\1</sub>";*)
];;

let gt = [ "-&gt;", " &#8594; "; (*"-&gt;", " <big>&#8594;</big> ";*)" *", " &#215;"];;

let replace_greek descr =
  if !Preferences.preferences.Preferences.pref_compl_greek then begin
    let descr = Miscellanea.replace_all ~regexp:false gt descr in
    let descr = Miscellanea.replace_all ~regexp:true greeks descr in
    descr
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


