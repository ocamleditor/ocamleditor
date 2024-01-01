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

let greeks_markup = [
  "&apos;a\\([0-9]*\\)\\b", "&#945;<sub>\\1</sub>";
  "&apos;b\\([0-9]*\\)\\b", "&#946;<sub>\\1</sub>";
  "&apos;c\\([0-9]*\\)\\b", "&#947;<sub>\\1</sub>";
  "&apos;d\\([0-9]*\\)\\b", "&#948;<sub>\\1</sub>";
  "&apos;e\\([0-9]*\\)\\b", "&#949;<sub>\\1</sub>";
  "&apos;f\\([0-9]*\\)\\b", "&#950;<sub>\\1</sub>";
  "&apos;g\\([0-9]*\\)\\b", "&#951;<sub>\\1</sub>";
  "&apos;h\\([0-9]*\\)\\b", "&#952;<sub>\\1</sub>";
  "&apos;i\\([0-9]*\\)\\b", "&#953;<sub>\\1</sub>";
  "&apos;j\\([0-9]*\\)\\b", "&#954;<sub>\\1</sub>";
  "&apos;k\\([0-9]*\\)\\b", "&#955;<sub>\\1</sub>";
  "&apos;l\\([0-9]*\\)\\b", "&#956;<sub>\\1</sub>";
  "&apos;m\\([0-9]*\\)\\b", "&#957;<sub>\\1</sub>";
  "&apos;n\\([0-9]*\\)\\b", "&#958;<sub>\\1</sub>";
  "&apos;o\\([0-9]*\\)\\b", "&#959;<sub>\\1</sub>";
  "&apos;p\\([0-9]*\\)\\b", "&#960;<sub>\\1</sub>";
  "&apos;q\\([0-9]*\\)\\b", "&#961;<sub>\\1</sub>";
  "&apos;r\\([0-9]*\\)\\b", "&#962;<sub>\\1</sub>";
  "&apos;s\\([0-9]*\\)\\b", "&#963;<sub>\\1</sub>";
  "&apos;t\\([0-9]*\\)\\b", "&#964;<sub>\\1</sub>";
  "&apos;u\\([0-9]*\\)\\b", "&#965;<sub>\\1</sub>";
  "&apos;v\\([0-9]*\\)\\b", "&#966;<sub>\\1</sub>";
  "&apos;w\\([0-9]*\\)\\b", "&#967;<sub>\\1</sub>";
  "&apos;x\\([0-9]*\\)\\b", "&#968;<sub>\\1</sub>";
  "&apos;y\\([0-9]*\\)\\b", "&#969;<sub>\\1</sub>";
  (*"&apos;z\\([0-9]*\\)", "&#970;<sub>\\1</sub>";*)
];;

let gt = [ "-&gt;", "&#8594;";(* "-&gt;", "<big>&#8594;</big>";*) "*", "&#215;"];;

let replace_simbols_in_markup descr =
  if Preferences.preferences#get.editor_completion_greek_letters then begin
    let descr = Miscellanea.replace_all ~regexp:false gt descr in
    let descr = Miscellanea.replace_all ~regexp:true greeks_markup descr in
    descr
  end else descr

let markup' descr =
  let descr = replace_simbols_in_markup (Glib.Markup.escape_text descr) in
  Miscellanea.replace_all
    ["\\([ \n(]\\|^\\)\\([?]?[a-z_][a-z0-9_']*\\):", "\\1<i><small>\\2:</small></i>"] descr

let markup = Miscellanea.Memo.create markup'

let markup2' descr =
  let descr = replace_simbols_in_markup (Glib.Markup.escape_text descr) in
  Miscellanea.replace_all [
    "\\([ \n(]\\|^\\)\\([?]?[a-z_][a-z0-9_']*\\):",  "\\1<small><i>\\2:</i></small>";

    (*"\\([A-Z][A-Za-z0-9_']+\\)\\.", "<b><small>\\1.</small></b>";*)

    "\\([A-Z`][A-Za-z0-9_']+\\)\\.", "<b><small>\\1.</small></b>";
    "\\([A-Z`][A-Za-z0-9_']+\\)", "<b>\\1</b>";
    "\\(method[ \t\r\n]+\\)\\([A-Za-z0-9_']+\\)", "<small>\\1</small>\\2";
  ] descr

let markup2 = Miscellanea.Memo.create markup2'

let markup3' descr =
  let descr = replace_simbols_in_markup (Glib.Markup.escape_text descr) in
  Miscellanea.replace_all [
    "\\([ \n(]\\|^\\)\\([?]?[a-z_][a-z0-9_']*\\):",  "\\1<i>\\2:</i>";
    "\\([A-Z`][A-Za-z0-9_']+\\)\\.", "<b>\\1</b>.";
  ] descr

let markup3 = Miscellanea.Memo.create markup3'

let greeks = [
  "'a\\([0-9]*\\)", "α\\1";
  "'b\\([0-9]*\\)", "β\\1";
  "'c\\([0-9]*\\)", "γ\\1";
  "'d\\([0-9]*\\)", "δ\\1";
  "'e\\([0-9]*\\)", "ε\\1";
  "'f\\([0-9]*\\)", "ζ\\1";
  "'g\\([0-9]*\\)", "η\\1";
  "'h\\([0-9]*\\)", "θ\\1";
  "'i\\([0-9]*\\)", "ι\\1";
  "'j\\([0-9]*\\)", "κ\\1";
  "'k\\([0-9]*\\)", "λ\\1";
  "'l\\([0-9]*\\)", "μ\\1";
  "'m\\([0-9]*\\)", "ν\\1";
  "'n\\([0-9]*\\)", "ξ\\1";
  "'o\\([0-9]*\\)", "ο\\1";
  "'p\\([0-9]*\\)", "π\\1";
  "'q\\([0-9]*\\)", "ρ\\1";
  "'r\\([0-9]*\\)", "ς\\1";
  "'s\\([0-9]*\\)", "σ\\1";
  "'t\\([0-9]*\\)", "τ\\1";
  "'u\\([0-9]*\\)", "υ\\1";
  "'v\\([0-9]*\\)", "φ\\1";
  "'w\\([0-9]*\\)", "χ\\1";
  "'x\\([0-9]*\\)", "ψ\\1";
  "'y\\([0-9]*\\)", "ω\\1";
  "'z\\([0-9]*\\)", "Ω\\1";
];;

let arrows = [
  "->", "\u{2192}";   (* \u{2192} *)
  "*", "\u{00d7}"
]

let replace_greeks_2 typ = Miscellanea.replace_all ~regexp:true greeks typ;;
let replace_symbols typ =
  if Preferences.preferences#get.editor_completion_greek_letters then begin
    Miscellanea.replace_all ~regexp:true greeks
      (Miscellanea.replace_all ~regexp:false arrows typ)
  end else typ;;


