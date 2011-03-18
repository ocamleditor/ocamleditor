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
open Miscellanea
open Templates

type t = Templates.t list

(** apply *)
let apply project (view : Ocaml_text.view) (templ : Templates.t) =
  view#tbuffer#undo#begin_block "Templates.apply";
  let buffer = view#obuffer in
  let start, stop = buffer#selection_bounds in
  let bol = start#set_line_index 0 in
  let mark_begin = buffer#create_mark bol in
  let bol = bol#forward_find_char ((<>) 32) in
  let base = bol#line_index in
  let selection = buffer#get_text ~start ~stop () in
  let indent_width = ref 0 in
  let mark_i = ref None in
  let mark_s = ref None in
  let insert_block text = function
    | `ALIGN n ->
      let indent = (String.make (base + n) ' ') in
      let text = Str.global_replace (Miscellanea.regexp "\n\\(.\\)") ("\n" ^ indent ^ "\\1") text in
      let text = indent ^ text in
      buffer#insert text
    | `INDENT ->
      let indent = (String.make !indent_width ' ') in
      let text = Str.global_replace (Miscellanea.regexp "\n\\(.\\)") ("\n" ^ indent ^ "\\1") text in
      let text = indent ^ text in
      buffer#insert text
    | `NONE -> buffer#insert text
  in
  (** Parse template *)
  begin
    match templ with
      | Templ templ ->
        ignore (buffer#delete_selection ());
        List.iter begin function
          | I -> mark_i := Some (buffer#create_mark (buffer#get_iter `INSERT));
          | S -> mark_s := Some (buffer#create_mark (buffer#get_iter `INSERT));
          | NL -> buffer#insert "\n";
          | IN -> indent_width := !indent_width + view#obuffer#tab_width;
          | OUT -> indent_width := !indent_width - view#obuffer#tab_width;
          | T text -> buffer#insert text
          | T0 text -> insert_block text (`ALIGN 0);
          | TI text -> insert_block text (`ALIGN !indent_width);
          | SELECTION ->
            let is_block = String.contains selection '\n' in
            if is_block then begin
              insert_block selection `INDENT;
            end else begin
              buffer#insert selection;
            end
          | SELECTION_TRIM -> buffer#insert (trim selection)
          | SELECTION_OPT def ->
            let selection = trim selection in
            buffer#insert (if selection = "" then def else selection)
          | CURRENT_FILENAME ->
            Gaux.may view#obuffer#file ~f:(fun file -> buffer#insert file#name)
          | CURRENT_LINE -> buffer#insert (string_of_int ((buffer#get_iter `INSERT)#line + 1))
          | DESCRIPTION -> buffer#insert project.Project.description
        end templ;
      | Action func -> func view
  end;
  let mark_end = buffer#create_mark (buffer#get_iter `INSERT) in
  (** Place cursor *)
  begin
    match !mark_i, !mark_s with
      | None, None -> ()
      | (Some mark), None -> buffer#place_cursor ~where:(buffer#get_iter_at_mark (`MARK mark));
      | None, (Some mark) -> buffer#place_cursor ~where:(buffer#get_iter_at_mark (`MARK mark));
      | (Some mi), (Some ms) -> buffer#select_range (buffer#get_iter_at_mark (`MARK mi)) (buffer#get_iter_at_mark (`MARK ms));
  end;
  Gaux.may !mark_i ~f:(fun mark -> buffer#delete_mark (`MARK mark));
  Gaux.may !mark_s ~f:(fun mark -> buffer#delete_mark (`MARK mark));
  (** Colorize *)
  let remove_marks () =
    buffer#delete_mark (`MARK mark_begin);
    buffer#delete_mark (`MARK mark_end);
  in
  if buffer#lexical_enabled then begin
    Gtk_util.idle_add begin fun () ->
      Lexical.tag view#buffer
        ~start:(buffer#get_iter_at_mark (`MARK mark_begin))
        ~stop:(buffer#get_iter_at_mark (`MARK mark_end));
    end
  end else (remove_marks());
  view#tbuffer#undo#end_block()

(** widget *)
class widget ~project ~(view : Ocaml_text.view) ?packing ()=
  let vbox = GPack.vbox ~spacing:5 ~border_width:0 ?packing () in
  let cols = new GTree.column_list in
  let col_key  = cols#add Gobject.Data.string in
  let col_name  = cols#add Gobject.Data.string in
  let col_descr  = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [`CELL_BACKGROUND !Preferences.preferences.Preferences.pref_bg_color_popup] in
  let vc_name = GTree.view_column ~renderer:(renderer, ["markup", col_name]) ~title:"Name" () in
  let vc_descr = GTree.view_column ~renderer:(renderer, ["markup", col_descr]) ~title:"Description" () in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let lview = GTree.view ~model:model ~headers_visible:true ~reorderable:false ~width:620 ~height:200 ~packing:sw#add () in
  let _ = lview#append_column vc_name in
  let _ = lview#append_column vc_descr in
  let _ = lview#set_headers_visible false in
object (self)
  inherit GObj.widget vbox#as_widget
  initializer
    lview#misc#modify_base [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_bg_color_popup];
    List.iter begin fun (name, descr, templ) ->
      let row = model#append () in
      model#set ~row ~column:col_key name;
      model#set ~row ~column:col_name (sprintf "<b><tt>%s</tt></b>" (Glib.Markup.escape_text name));
      model#set ~row ~column:col_descr (sprintf "<tt>%s</tt>" (Glib.Markup.escape_text descr));
    end Templates.spec;
    ignore (lview#connect#row_activated ~callback:begin fun path _ ->
      let row = model#get_iter path in
      let name = model#get ~row ~column:col_key in
      try
        let _, _, templ = List.find (fun (x, _, _) -> name = x) Templates.spec in
        apply project view templ;
        Gaux.may (GWindow.toplevel vbox#coerce) ~f:(fun w -> w#destroy())
      with Not_found -> ()
    end)
end

(** popup *)
let popup project view =
  let x, y = view#get_location_at_cursor () in
  let widget = new widget ~project ~view () in
  Gtk_util.window widget#coerce ~view ~x ~y ()











