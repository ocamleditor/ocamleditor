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

open GUtil

(** signals *)
class hover () = object
  inherit [(GText.iter * GText.iter) option ref * GText.iter] signal ()
end
and activate () = object
  inherit [GText.iter] signal ()
end
and signals ~hover ~activate = object
  inherit ml_signals [hover#disconnect; activate#disconnect; ]
  method hover = hover#connect ~after
  method activate = activate#connect ~after
end

(** hyperlink *)
and hyperlink ~(view : GText.view) ?(use_ctrl_key=true) () =
  let not_use_ctrl_key = not use_ctrl_key in
  let buffer = view#buffer in
  let hover = new hover () in
  let activate = new activate () in
  let tag = buffer#create_tag [`UNDERLINE `SINGLE] in
  object (self)
    val mutable current_hover = None
    method connect = new signals ~hover ~activate

    method private apply_hover ~x ~y =
      let x, y = view#window_to_buffer_coords ~tag:`TEXT ~x ~y in
      let iter = view#get_iter_at_location ~x ~y in
      let bounds = ref None in
      hover#call (bounds, iter);
      begin
        match !bounds with
        | None -> self#remove_hover ()
        | Some (start, stop) ->
            Gaux.may current_hover ~f:(fun (start, stop) -> buffer#remove_tag tag ~start ~stop);
            current_hover <- Some (start, stop);
            buffer#apply_tag tag ~start ~stop;
            Gaux.may (view#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `HAND1));
      end

    method private remove_hover () =
      Gaux.may (view#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `XTERM));
      Gaux.may current_hover ~f:(fun (start, stop) -> buffer#remove_tag tag ~start ~stop);
      current_hover <- None;

    method enable () =
      ignore(view#event#connect#after#motion_notify ~callback:begin fun ev ->
          if not_use_ctrl_key || GdkEvent.Motion.state ev = 4 then begin
            let x = int_of_float (GdkEvent.Motion.x ev) in
            let y = int_of_float (GdkEvent.Motion.y ev) in
            self#apply_hover ~x ~y;
          end else (self#remove_hover ());
          false
        end);
      ignore(view#event#connect#after#leave_notify ~callback:begin fun _ev ->
          self#remove_hover (); false
        end);
      ignore(view#buffer#connect#changed ~callback:begin fun () ->
          Gaux.may (view#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `XTERM));
          current_hover <- None;
        end);
      let button_event_callback ev =
        let x = int_of_float (GdkEvent.Button.x ev) in
        let y = int_of_float (GdkEvent.Button.y ev) in
        let x, y = view#window_to_buffer_coords ~tag:`TEXT ~x ~y in
        let iter = view#get_iter_at_location ~x ~y in
        activate#call iter;
      in
      if use_ctrl_key then begin
        ignore(view#event#connect#button_press ~callback:begin fun ev ->
            if GdkEvent.Button.button ev = 1 && GdkEvent.Button.state ev = 4
            then (button_event_callback ev; true) else false
          end);
        ignore(view#event#connect#after#key_press ~callback:begin fun ev ->
            let key = GdkEvent.Key.keyval ev in
            if key = GdkKeysyms._Control_L || key = GdkKeysyms._Control_R then begin
              Gaux.may (view#get_window `TEXT) ~f:begin fun window ->
                let x, y = Gdk.Window.get_pointer_location window in
                self#apply_hover ~x ~y;
              end;
              true
            end else false
          end);
        ignore(view#event#connect#key_release ~callback:begin fun ev ->
            let key = GdkEvent.Key.keyval ev in
            if not_use_ctrl_key || (key = GdkKeysyms._Control_L || key = GdkKeysyms._Control_R)
            then (self#remove_hover ());
            false
          end);
      end else begin
        ignore(view#event#connect#button_release ~callback:begin fun ev ->
            if GdkEvent.Button.button ev = 1 then (button_event_callback ev);
            false
          end);
      end
  end
