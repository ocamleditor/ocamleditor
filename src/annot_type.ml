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

type source_point = [`ITER of GText.iter | `XY of (int * int)]

type info = {
  at_start  : int * int;
  at_stop   : int * int;
  at_type   : string;
}

class annot_type ~page =
  let view : Ocaml_text.view = page#ocaml_view in
  let buffer = view#obuffer in
  let tag = buffer#create_tag ~name:"tag_annot" [] in
object (self)
  val mutable tag_bounds = None
  val mutable tag_popup = None

  method tag = tag

  method get_annot_at_iter iter =
    if not buffer#changed_after_last_autocomp then begin
      match buffer#as_text_buffer#file with
        | None -> None
        | Some file ->
          begin
            match page#project.Prj.in_source_path file#filename with
              | Some _ -> Binannot_type.find ~page ~iter ()
              | _ -> None
          end;
    end else None

  method get_annot_at_location ~x ~y =
    if view#has_focus && (not buffer#changed_after_last_autocomp) then begin
      let iter =
        let iter = view#get_iter_at_location ~x ~y in
        if iter#ends_line
        || Glib.Unichar.isspace iter#char
        || begin
          match Comments.enclosing
            (Comments.scan (Glib.Convert.convert_with_fallback ~fallback:"" ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset
              (view#buffer#get_text ()))) iter#offset
          with None -> false | _ -> true;
        end
        then None else (Some iter)
      in
      Option.fold iter ~none:None ~some:self#get_annot_at_iter
    end else None

  method get_type position =
    let annot = match position with
      | `ITER iter -> self#get_annot_at_iter iter
      | `XY (x, y) -> self#get_annot_at_location ~x ~y
    in
    let open Binannot_type in
    let open Location in
    let open Lexing in
    match annot with
      | Some {ba_loc; ba_type} ->
        let at_type = ba_type in
        let at_start = ba_loc.loc_start.pos_lnum, (ba_loc.loc_start.pos_cnum - ba_loc.loc_start.pos_bol) in
        let at_stop = ba_loc.loc_end.pos_lnum, (ba_loc.loc_end.pos_cnum - ba_loc.loc_end.pos_bol) in
        (*let type_annot = match Annotation.get_type annot_annotations with Some x -> x | None -> "" in
        let start = start.Oe.annot_lnum, (start.Oe.annot_cnum - start.Oe.annot_bol) in
        let stop = stop.Oe.annot_lnum, (stop.Oe.annot_cnum - stop.Oe.annot_bol) in*)
        (*Some (at_start, at_stop, at_type);*)
        Some {at_start; at_stop; at_type}
      | _ -> None

  method apply_tag (where : source_point) =
    match self#get_type where with
      | Some {at_start; at_stop; at_type} when (not view#buffer#has_selection) ->
        begin
          match tag_bounds with
            | Some (_, _, (prev_start, prev_stop)) when at_start = prev_start && at_stop = prev_stop -> None
            | _ ->
              self#remove_tag ();
              let line_count = buffer#line_count in
              let line, index1 = at_start in
              if line <= line_count then begin
                let start_iter = buffer#get_iter (`LINE (line - 1)) in
                if index1 < start_iter#bytes_in_line then begin
                  let bol = start_iter (*buffer#get_iter (`LINE (line - 1))*) in
                  let eol = bol#forward_to_line_end in
                  let index1 = Convert.offset_from_pos (buffer#get_text ~start:bol ~stop:eol ()) ~pos:index1 in
                  let start_iter = buffer#get_iter (`LINECHAR ((line - 1), index1)) in
                  let line, index2 = at_stop in
                  if line <= line_count then begin
                    let stop_iter = buffer#get_iter (`LINE (line - 1)) in
                    if index2 < stop_iter#bytes_in_line then begin
                      let bol = buffer#get_iter (`LINE (line - 1)) in
                      let eol = bol#forward_to_line_end in
                      let index2 = Convert.offset_from_pos (buffer#get_text ~start:bol ~stop:eol ()) ~pos:index2 in
                      let stop_iter = buffer#get_iter (`LINECHAR ((line - 1), index2)) in
                      buffer#apply_tag tag ~start:start_iter ~stop:stop_iter;
                      let mark1 = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Annot_type.apply_tag")*) start_iter in
                      let mark2 = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Annot_type.apply_tag")*) stop_iter in
                      tag_bounds <- Some (mark1, mark2, (at_start, at_stop));
                      let markup = (Print_type.markup2 (String.trim at_type)) in
                      Some markup
                    end else None
                  end else None
                end else None
              end else None
        end
      | _ ->
        self#remove_tag ();
        None

  method remove_tag () =
    Gaux.may tag_popup ~f:begin fun popup ->
      popup#destroy();
      tag_popup <- None
    end;
    match tag_bounds with
      | None -> ()
      | Some (start, stop, _) ->
        tag_bounds <- None;
        let start = (`MARK start) in
        let stop = (`MARK stop) in
        buffer#remove_tag tag ~start:(buffer#get_iter start) ~stop:(buffer#get_iter stop);
        buffer#delete_mark start;
        buffer#delete_mark stop;

  method tooltip where =
    if Preferences.preferences#get.Preferences.pref_annot_type_tooltips_impl = 0
    then (self#popup where ())
    else begin
      match self#apply_tag where with
        | None -> ()
        | Some markup ->
          let markup = sprintf "<span font='%s'>%s</span>"
            Preferences.preferences#get.Preferences.pref_compl_font markup in
          view#misc#set_tooltip_markup markup;
    end

  method popup ?(position = (`CURSOR : [`CURSOR | `TOP_RIGHT])) where () =
    match self#apply_tag where with
      | None -> ()
      | Some markup ->
        let popup = GWindow.window ~kind:`POPUP ~type_hint:`MENU
          ~decorated:false ~focus_on_map:false ~border_width:1 ~show:false () in
        let color = Color.add_value Preferences.preferences#get.Preferences.pref_bg_color_popup 0.1 in
        popup#misc#modify_bg [`NORMAL, `NAME color];
        popup#misc#set_can_focus false;
        let ebox = GBin.event_box ~packing:popup#add () in
        ebox#misc#modify_bg [`NORMAL, (`NAME Preferences.preferences#get.Preferences.pref_bg_color_popup)];
        let label = GMisc.label ~markup ~xalign:0.0 ~xpad:4 ~ypad:4 ~packing:ebox#add () in
        label#misc#modify_font_by_name Preferences.preferences#get.Preferences.pref_compl_font;
        tag_popup <- Some popup;
        begin
          match position with
            | `CURSOR ->
              begin
                match where with
                  | `XY _ ->
                    begin
                      match view#get_window `WIDGET with
                        | Some _window ->
                          let pX, pY = Gdk.Window.get_pointer_location popup#misc#window in
                          popup#move ~x:pX ~y:(pY + 10);
                          popup#present();
                        | _ -> assert false
                    end;
                  | `ITER _ ->
                    let x, y = view#get_location_at_cursor () in
                    (*if not Ocaml_config.is_mingw then popup#set_opacity 0.0;*)
                    popup#move ~x ~y:(y + 2);
              end;
            | `TOP_RIGHT ->
              if not Ocaml_config.is_mingw then popup#set_opacity 0.0;
              let x, y = view#get_location_top_right () in
              let y = y + 5 in
              popup#move ~x ~y;
              popup#show();
              let x = x - popup#misc#allocation.Gtk.width - 5 in
              popup#move ~x ~y;
        end;
        let incr = if Preferences.preferences#get.Preferences.pref_annot_type_tooltips_delay = 0 then 0.106 else 0.479 in
        Gmisclib.Util.fade_window ~incr popup

end
