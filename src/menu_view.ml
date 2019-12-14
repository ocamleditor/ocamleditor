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

let get_switch_viewer_label page =
  let device =
    match Oe_config.dot_viewer with
      | `PDF -> (module Dot_viewer_pdf.PDF : Dot_viewer_plugin.DEVICE)
      | _ -> !Dot_viewer_plugin.device
  in
  match device with (module DEV : Dot_viewer_plugin.DEVICE) ->
    if not DEV.have_embedded_viewer then "Dependency Graph"
    else begin
      match page with
        | Some page when page#button_dep_graph#active -> "Switch Viewer to \xC2\xABSource\xC2\xBB"
        | Some _ -> "Switch Viewer to \xC2\xABDependencies\xC2\xBB"
        | _ -> ""
    end

let get_switch_view_sensitive project page =
  (project.Prj.in_source_path page#get_filename) <> None

let update_labels
  ~code_folding
  ~select_in_outline
  ~enable_code_folding
  ~collapse_enclosing
  ~unfold_all
  ~show_whitespace_chars
  ~signal_show_whitespace_chars
  ~toggle_word_wrap
  ~signal_toggle_wrod_wrap
  ~switch_viewer
  ~rev_history
  editor =
  let page = editor#get_page `ACTIVE in
  let has_current_page = page <> None in
  let is_ml =
    match page with
      | None -> false
      | Some page ->
       let name = page#get_filename in
       name ^^^ ".ml" || name ^^^ ".mli"
  in
  code_folding#misc#set_sensitive is_ml;
  select_in_outline#misc#set_sensitive is_ml;
  enable_code_folding#set_active Preferences.preferences#get.Preferences.pref_code_folding_enabled;
  List.iter (fun x -> x#misc#set_sensitive enable_code_folding#active) [collapse_enclosing; unfold_all];
  show_whitespace_chars#misc#handler_block signal_show_whitespace_chars;
  show_whitespace_chars#set_active (editor#show_whitespace_chars);
  show_whitespace_chars#misc#handler_unblock signal_show_whitespace_chars;
  show_whitespace_chars#misc#set_sensitive has_current_page;
  toggle_word_wrap#misc#handler_block signal_toggle_wrod_wrap;
  toggle_word_wrap#set_active editor#word_wrap;
  toggle_word_wrap#misc#handler_unblock signal_toggle_wrod_wrap;
  toggle_word_wrap#misc#set_sensitive has_current_page;
  switch_viewer#misc#set_sensitive has_current_page;
  rev_history#misc#set_sensitive has_current_page;
  let text = get_switch_viewer_label page in
  switch_viewer#misc#set_property "label" (`STRING (Some text));
  Opt.may page (fun page ->
    switch_viewer#misc#set_sensitive (get_switch_view_sensitive editor#project page));;

let toggle_code_folding ~enable_code_folding editor =
  editor#code_folding_enabled#set enable_code_folding#active;
  editor#with_current_page (fun page -> page#ocaml_view#code_folding#set_enabled enable_code_folding#active);
  Preferences.preferences#get.Preferences.pref_code_folding_enabled <- enable_code_folding#active;
  Preferences.save()

