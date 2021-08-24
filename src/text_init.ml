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


open GdkKeysyms
open Miscellanea

(** key_press *)
let key_press ~project view =
  ignore (view#event#connect#key_press ~callback:begin fun ev ->
      let state = GdkEvent.Key.state ev in
      let key = GdkEvent.Key.keyval ev in
      match state with
      | [] ->
          if key = _Tab then begin
            let ocp_indent_applied =
              (match view#tbuffer#file with Some file when not (file#filename ^^^ ".ml") && not (file#filename ^^^ ".ml") -> false | _ -> true) &&
              if Oe_config.ocp_indent_tab_key_enabled then
                (Ocp_indent.indent ~project ~view `SELECTION) else false
            in
            if not ocp_indent_applied then begin
              view#tbuffer#indent ?decrease:None ();
              view#draw_current_line_background ?force:(Some true) (view#buffer#get_iter `INSERT);
            end;
            true
          end else if key = _Return || key = _BackSpace then begin
            Gmisclib.Idle.add ~prio:100 begin fun () ->
              let iter = view#buffer#get_iter `INSERT in
              view#draw_current_line_background ?force:None iter;
            end;
            false
          end else if key = _Home then (Smart_keys.smart_home ~view state)
          else if key = _End then (Smart_keys.smart_end ~view state)
          else false
      | [`MOD2] when key = _igrave ->
          view#buffer#delete ~start:(view#buffer#get_iter `INSERT) ~stop:(view#buffer#get_iter `SEL_BOUND);
          view#buffer#insert ?iter:None ?tag_names:None ?tags:None "~";
          true
      | [`MOD2] when key = _apostrophe ->
          view#buffer#delete ~start:(view#buffer#get_iter `INSERT) ~stop:(view#buffer#get_iter `SEL_BOUND);
          view#buffer#insert ?iter:None ?tag_names:None ?tags:None "`";
          true
      | [`CONTROL] when key = _Up -> ignore (view#scroll `UP); true
      | [`CONTROL] when key = _Down -> ignore (view#scroll `DOWN); true
      | [`SHIFT] when key = _Home -> Smart_keys.smart_home ~view state
      | [`SHIFT] when key = _End -> Smart_keys.smart_end ~view state
      | _ ->
          if key = _ISO_Left_Tab then begin
            view#tbuffer#indent ?decrease:(Some true) ();
            view#draw_current_line_background ?force:(Some true) (view#buffer#get_iter `INSERT);
            true
          end else false
    end);;

(** realize *)
let realize view =
  let self = view in
  if not self#realized then begin
    ignore (self#misc#connect#after#realize ~callback:begin fun () ->
        (match self#gutter.Gutter.bg_color with
         | `WHITE -> self#gutter.Gutter.bg_color <-
               (match Oe_config.gutter_bg_color with
                | `CALC x -> Color.set_value x (`COLOR (self#misc#style#base `NORMAL))
                | `THEME -> `COLOR (self#misc#style#bg `PRELIGHT)
                | (`NAME _) as color-> color)
         | _ -> ());
        (match self#gutter.Gutter.fg_color with
         | `WHITE -> self#gutter.Gutter.fg_color <-
               (match Oe_config.gutter_fg_color with
                | `CALC x -> Color.set_value x (`COLOR (self#misc#style#base `NORMAL))
                | `THEME -> (Color.set_value 0.98 (`COLOR (self#misc#style#dark `NORMAL)))
                | (`NAME _) as color -> color)
         | _ -> ());
        (match self#gutter.Gutter.border_color with
         | `WHITE -> self#gutter.Gutter.border_color <-
               (match Oe_config.gutter_border_color with
                | `CALC x -> Color.set_value x (`COLOR (self#misc#style#base `NORMAL))
                | `THEME -> Color.set_value 0.95 (`COLOR (self#misc#style#bg `INSENSITIVE))
                | (`NAME _) as color -> color)
         | _ -> ());
        (match self#gutter.Gutter.marker_color with
         | `WHITE -> self#gutter.Gutter.marker_color <-
               (match Oe_config.gutter_marker_color with
                | `CALC x -> Color.set_value x (`COLOR (self#misc#style#base `NORMAL))
                | `THEME -> `COLOR (self#misc#style#dark `NORMAL)
                | (`NAME _) as color -> color)
         | _ -> ());
        (match self#gutter.Gutter.marker_bg_color with
          | `WHITE -> self#gutter.Gutter.marker_bg_color <-
              (match Oe_config.gutter_marker_bg_color with
                | `CALC x -> Color.set_value x (`COLOR (self#misc#style#base `NORMAL))
                | `THEME -> `COLOR (self#misc#style#dark `NORMAL)
                | (`NAME _) as color -> color)
          | _ -> ());
        (* Change the bg color of the gutter on screen *)
        view#misc#modify_bg [`NORMAL, self#gutter.Gutter.bg_color];
        self#set_realized true
      end)
  end
;;

(** select_lines_from_gutter *)
let select_lines_from_gutter view =
  let self = view in
  ignore (self#event#connect#after#button_press ~callback:begin fun ev ->
      let window = GdkEvent.get_window ev in
      match self#get_window `LEFT with
      | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
          let y0 = Gdk.Rectangle.y self#visible_rect in
          let y = GdkEvent.Button.y ev in
          let start = fst (self#get_line_at_y ((int_of_float y) + y0)) in
          view#gutter.Gutter.start_selection <- Some start;
          (*buffer#select_range start start#forward_line;*)
          false
      | _ -> false
    end);
  ignore (self#event#connect#after#motion_notify ~callback:begin fun ev ->
      let window = GdkEvent.get_window ev in
      match self#get_window `LEFT with
      | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
          let y0 = Gdk.Rectangle.y self#visible_rect in
          let y = GdkEvent.Motion.y ev in
          Gaux.may view#gutter.Gutter.start_selection ~f:begin fun start ->
            let stop = fst (self#get_line_at_y ((int_of_float y) + y0)) in
            view#buffer#select_range start stop#forward_line;
            (view#as_gtext_view : GText.view)#scroll_to_iter stop#forward_line;
          end;
          true
      | _ -> false
    end);
  ignore (self#event#connect#after#button_release ~callback:begin fun ev ->
      let window = GdkEvent.get_window ev in
      match self#get_window `LEFT with
      | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
          view#gutter.Gutter.start_selection <- None;
          false
      | _ -> false
    end);;
