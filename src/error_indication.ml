(*

  OCamlEditor
  Copyright (C) 2010-2013 Francesco Tovagliari

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

let forward_non_blank iter =
  let rec f it =
    let stop = it#forward_char in
    let ch = it#get_text ~stop in
    if List.for_all ((<>) ch) [" "; "\t"; "\r"] then it
    else f stop
  in
  f iter

class error_indication (view : Ocaml_text.view) vscrollbar (global_gutter, global_gutter_ebox) =
  let buffer = view#tbuffer in
  let create_tags () =
    let ts = Unix.gettimeofday() in
    let tag_error = buffer#create_tag ~name:(sprintf "tag_error-%f" ts) [`LEFT_MARGIN view#left_margin] in
    Gobject.Property.set tag_error#as_tag {Gobject.name="underline"; conv=Gobject.Data.int} 4;
    let tag_warning = buffer#create_tag ~name:(sprintf "tag_warning-%f" ts) [`LEFT_MARGIN view#left_margin] in
    let tag_warning_unused = buffer#create_tag ~name:(sprintf "tag_warning_unused-%f" ts) Oe_config.warning_unused_properties in
    tag_error, tag_warning, tag_warning_unused
  in
  let tag_error, tag_warning, tag_warning_unused = create_tags () in
  let is_warning_unused = function
    | Oe.Warning 20 | Oe.Warning 26 | Oe.Warning 27 -> true
    | _ -> false
  in
object (self)
  val mutable tag_error_bounds = []
  val mutable tag_warning_bounds = []
  val mutable tag_popup = []
  val mutable enabled = true
  val has_errors = new GUtil.variable false
  val has_warnings = new GUtil.variable false
  val has_errors_or_warnings = new GUtil.variable false
  val mutable sticky_popup = false
  val mutable table = []
  val mutable error_gutter_markers = []
  val mutable flag_underline = Preferences.preferences#get.Preferences.pref_err_underline
  val mutable flag_tooltip = Preferences.preferences#get.Preferences.pref_err_tooltip
  val mutable flag_gutter = Preferences.preferences#get.Preferences.pref_err_gutter
  val mutable tag_error = tag_error
  val mutable tag_warning = tag_warning
  val mutable tag_warning_unused = tag_warning_unused

  method enabled = enabled
  method set_enabled x = enabled <- x

  method flag_underline = flag_underline
  method flag_tooltip = flag_tooltip
  method flag_gutter = flag_gutter
  method set_flag_underline x =
    flag_underline <- x;
    self#remove_tag();
    Gobject.Property.set tag_error#as_tag {Gobject.name="underline"; conv=Gobject.Data.int}
      (if flag_underline then 4 else 0)

  method create_tags () =
    let tag_table = new GText.tag_table buffer#tag_table in
    tag_table#remove tag_error#as_tag;
    tag_table#remove tag_warning#as_tag;
    tag_table#remove tag_warning_unused#as_tag;
    let tg_error, tg_warning, tg_warning_unused = create_tags () in
    tag_error <- tg_error;
    tag_warning <- tg_warning;
    tag_warning_unused <- tg_warning_unused

  method tag_of_error error =
    match error.Oe.er_level with
      | Oe.Error -> tag_error
      | Oe.Warning 20 | Oe.Warning 26 | Oe.Warning 27 -> tag_warning_unused
      | _ -> tag_warning

  method set_flag_tooltip x = flag_tooltip <- x
  method set_flag_gutter x =
    flag_gutter <- x;
    self#remove_tag();

  method has_errors = has_errors
  method first_error = match tag_error_bounds with x :: _-> Some x | _ -> None

  method has_warnings = has_warnings
  method first_warning = match tag_warning_bounds with x :: _ -> Some x | _ -> None

  method has_errors_or_warnings = has_errors_or_warnings
  method first_error_or_warning = match self#first_error with None -> self#first_warning | x -> x

  method private callback_gutter_marker mark =
    let iter = buffer#get_iter_at_mark (`MARK mark) in
    buffer#block_signal_handlers ();
    (* Protect the tooltip from the mark_set signal emission of place_cursor which otherwise destroys it *)
    buffer#place_cursor iter;
    view#draw_current_line_background ~force:true iter;
    buffer#unblock_signal_handlers ();
    Gmisclib.Idle.add ~prio:300 (fun () -> self#tooltip ~sticky:true ~need_focus:false (`ITER iter));
    true;

  method private do_apply_tag messages kind =
    (*Prf.crono Prf.prf_error_indication_appy_tag begin fun () ->*)
      let tview = (view :> Text.view) in
      let messages = List.sort (fun e1 e2 -> Pervasives.compare e1.Oe.er_line e2.Oe.er_line) messages in
      List.map begin fun error ->
        let line = error.Oe.er_line - 1 in
        let c1, c2 = error.Oe.er_characters in
        let iter = buffer#get_iter (`LINE line) in
        let text = iter#get_text buffer#end_iter in
        let c1x = Convert.offset_to_pos text ~pos:0 ~off:c1 in
        let start = iter#set_line_index c1x in
        let stop = iter#forward_chars c2 in
        let tag = self#tag_of_error error in
        buffer#apply_tag tag ~start ~stop;
        let mark_start = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Error_indication.do_apply_tag1")*) start in
        if flag_gutter then begin
          let kind, pixbuf =
            match kind with
              | `Warning -> `Warning error.Oe.er_message, Icons.warning_14
              | `Error -> `Error error.Oe.er_message, Icons.error_16
              | _ -> assert false
          in
          let marker = Gutter.create_marker ~kind
            ~mark:mark_start ~pixbuf ~callback:self#callback_gutter_marker ()
          in
          tview#gutter.Gutter.markers <- marker :: tview#gutter.Gutter.markers;
          error_gutter_markers <- marker :: error_gutter_markers;
        end;
        mark_start, (buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Error_indication.do_apply_tag2")*) stop), error
      end messages
    (*end ()*)

  method apply_tag messages =
    Gmisclib.Idle.add begin fun () ->
      if enabled then begin
        let tview = (view :> Text.view) in
        self#remove_tag();
        let errors = messages.Oe.er_errors in
        let warnings = messages.Oe.er_warnings in
        tag_error_bounds <- self#do_apply_tag errors `Error;
        tag_warning_bounds <- self#do_apply_tag warnings `Warning;
        has_errors#set (tag_error_bounds <> []);
        has_warnings#set (tag_warning_bounds <> []);
        let has_messages = has_errors#get || has_warnings#get in
        has_errors_or_warnings#set has_messages;
        if has_messages then (tview#draw_gutter());
        self#paint_global_gutter ();
      end;
    end;

  method private do_remove_tag tags =
    List.iter begin fun (start, stop, error) ->
      let start = `MARK start in
      let stop = `MARK stop in
      let tag = self#tag_of_error error in
      buffer#remove_tag tag ~start:(buffer#get_iter start) ~stop:(buffer#get_iter stop)#forward_line;
      buffer#delete_mark start;
      buffer#delete_mark stop;
    end tags;

  method remove_tag () =
    if has_errors_or_warnings#get then begin
      let had_messages = has_errors_or_warnings#get in
      self#do_remove_tag tag_error_bounds;
      self#do_remove_tag tag_warning_bounds;
      tag_error_bounds <- [];
      tag_warning_bounds <- [];
      has_errors#set false;
      has_warnings#set false;
      has_errors_or_warnings#set false;
      let tview = (view :> Text.view) in
      Gutter.destroy_markers tview#gutter error_gutter_markers;
      error_gutter_markers <- [];
      if had_messages then (tview#draw_gutter());
    end

  method hide_tooltip ?(force=true) () =
    if force || not sticky_popup then begin
      List.iter (fun (_, _, w) -> w#destroy()) tag_popup;
      tag_popup <- [];
      sticky_popup <- false
    end

  method private find_message iter tags =
    try
      List.find (fun (start, stop, error) -> iter#equal (buffer#get_iter_at_mark (`MARK start))) tags
    with Not_found -> begin
      List.find begin fun (start, stop, error) ->
        iter#in_range ~start:(buffer#get_iter_at_mark (`MARK start)) ~stop:(buffer#get_iter_at_mark (`MARK stop))
      end tags
    end

  method tooltip ?(sticky=false) ?(need_focus=true) (location : [`ITER of GText.iter | `XY of int * int]) =
    if enabled && (not need_focus || view#misc#get_flag `HAS_FOCUS) then begin
      let iter = match location with `XY (x, y) -> view#get_iter_at_location ~x ~y | `ITER it -> it in
      if not iter#ends_line then begin
        try
          (** When the tooltip is already shown, do nothing *)
          ignore (List.find begin fun (start, stop, popup) ->
            try
              let start = buffer#get_iter_at_mark (`MARK start) in
              let stop = buffer#get_iter_at_mark (`MARK stop) in
              iter#in_range ~start ~stop
            with Gmisclib.Util.Mark_deleted -> false
          end tag_popup);
          sticky_popup <- sticky;
        with Not_found -> begin
          try
            (*Prf.crono Prf.prf_error_indication_tooltip begin fun () ->*)
              (** Find message by iter *)
              let (start, stop, error), border_color, bg_color =
                try
                  (self#find_message iter tag_error_bounds),
                    Oe_config.error_popup_border_color, Oe_config.error_popup_bg_color
                with Not_found -> begin
                  if Oe_config.warning_tootip_enabled || not sticky then (raise Not_found);
                  (self#find_message iter tag_warning_bounds),
                    Oe_config.warning_popup_border_color, Oe_config.warning_popup_bg_color
                end
              in
              (** Create popup *)
              self#hide_tooltip();
              let popup = GWindow.window ~kind:`POPUP ~type_hint:`MENU ~decorated:false ~focus_on_map:false ~border_width:1 ~show:false () in
              tag_popup <- (start, stop, popup) :: tag_popup;
              sticky_popup <- sticky;
              popup#misc#modify_bg [`NORMAL, border_color];
              let ebox = GBin.event_box ~packing:popup#add () in
              ebox#misc#modify_bg [`NORMAL, bg_color];
              let error_message = Glib.Convert.convert_with_fallback ~fallback:"?"
                ~from_codeset:Oe_config.ocaml_codeset ~to_codeset:"UTF-8" error.Oe.er_message
              in
              let markup = (*(error.Oe.er_location) ^*)
                (Print_type.markup3 error_message) in
              let label = GMisc.label ~markup ~xpad:5 ~ypad:5 ~packing:ebox#add () in
              label#misc#modify_font_by_name Preferences.preferences#get.Preferences.pref_compl_font;
              (** Positioning *)
              begin
                popup#move ~x:(-1000) ~y:(-1000);
                match location with
                  | `ITER _ ->
                    let x, y = view#get_location_at_iter iter in
                    popup#show();
                    let y = y - popup#misc#allocation.Gtk.height - 5 in
                    popup#move ~x ~y;
                  | `XY _ ->
                    let x, y = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
                    popup#show();
                    popup#move ~x ~y:(y - popup#misc#allocation.Gtk.height - 12);
              end;
              let incr = if Preferences.preferences#get.Preferences.pref_annot_type_tooltips_delay = 0 then 0.106 else 0.479 in
              Gmisclib.Util.fade_window ~incr popup;
            (*end ()*)
          with Not_found -> (if not sticky_popup then (self#hide_tooltip()))
        end
      end else (if not sticky_popup then (self#hide_tooltip()));
    end

  method paint_global_gutter () =
    try
      let window = global_gutter#misc#window in
      table <- [];
      let drawable = new GDraw.drawable window in
      drawable#set_line_attributes ~width:1 ~style:`SOLID ~join:`ROUND ();
      let width, height = drawable#size in
      let alloc = vscrollbar#misc#allocation in
      (** Clean up *)
      drawable#set_foreground view#gutter.Gutter.bg_color;
      drawable#rectangle ~filled:true ~x:0 ~y:0 ~width ~height ();
      (** Rectangles at the top and bottom *)
      drawable#set_foreground view#gutter.Gutter.fg_color;
      drawable#rectangle ~filled:false ~x:0 ~y:0 ~width:(width - 1) ~height:(alloc.Gtk.width - 1) ();
      drawable#rectangle ~filled:false ~x:0 ~y:(height - alloc.Gtk.width) ~width:(width - 1) ~height:(alloc.Gtk.width - 2) ();
      (** Rectangle at the top in different color *)
      let color = if self#has_errors#get then (Some Oe_config.error_underline_color)
        else if self#has_warnings#get then (Some Oe_config.warning_popup_border_color)
        else None (*(Some Oe_config.global_gutter_no_errors)*)
      in
      Gaux.may color ~f:begin fun color ->
        drawable#set_foreground color;
        drawable#rectangle ~filled:true ~x:1 ~y:1 ~width:(width - 2) ~height:(alloc.Gtk.width - 2) ();
      end;
      (** Draw markers *)
      let height = height - 2 * alloc.Gtk.width in
      let line_count = float buffer#line_count in
      let height = float height in
      (*let width = width - 1 in*)
      (** Comments *)
      if Oe_config.global_gutter_comments_enabled && tag_error_bounds = [] && view#mark_occurrences_manager#table = [] then begin
        match Comments.scan_utf8 (buffer#get_text ()) with
          | Comments.Utf8 comments ->
            Gdk.GC.set_dashes drawable#gc ~offset:0 [1; 1];
            List.iter begin fun (start, stop, _, odoc) ->
              let iter = buffer#get_iter (`OFFSET start) in
              let is_fold = view#code_folding#is_folded iter#forward_to_line_end <> None in
              let line_start = float iter#line in
              let line_stop = float (buffer#get_iter (`OFFSET stop))#line in
              let y1 = int_of_float ((line_start /. line_count) *. height) in
              let y2 = int_of_float ((line_stop /. line_count) *. height) in
              let y1 = y1 + alloc.Gtk.width in
              let y2 = y2 + alloc.Gtk.width in
              let filled = odoc in
              let offset = if y1 = y2 then 0 else 1 in
              let style = if is_fold then `ON_OFF_DASH else `SOLID in
              drawable#set_line_attributes ~width:1 ~style ();
              if filled then begin
                drawable#set_foreground Oe_config.global_gutter_comments_bgcolor;
                drawable#rectangle ~filled ~x:0 ~y:y1 ~width:(width - offset) ~height:(y2 - y1) ();
              end;
              drawable#set_foreground Oe_config.global_gutter_comments_color;
              drawable#rectangle (*~filled*) ~x:0 ~y:y1 ~width:(width - offset) ~height:(y2 - y1) ();
            end comments
          | _ -> assert false
      end;
      (** Draw a marker *)
      let draw_marker start color =
        drawable#set_foreground color;
        let line_start = float (buffer#get_iter_at_mark (`MARK start))#line in
        let y = int_of_float ((line_start /. line_count) *. height) in
        table <- (y + 1, start) :: table;
        let y = y + alloc.Gtk.width in
        drawable#rectangle ~filled:true ~x:0 ~y ~width ~height:3 ();
      in
      (** Warnings *)
      List.iter begin fun (start, _, warning) ->
        draw_marker start (if is_warning_unused warning.Oe.er_level
          then (`NAME Oe_config.warning_unused_color) else Oe_config.warning_popup_border_color);
      end tag_warning_bounds;
      (** Mark Occurrences *)
      begin
        match Preferences.preferences#get.Preferences.pref_editor_mark_occurrences with
          | true, color ->
            let bg = `NAME color in
            let border = `NAME (Color.add_value color ~sfact:0.75 0.13) in
            List.iter begin fun (m1, m2) ->
              let start = buffer#get_iter_at_mark m1 in
              let stop = buffer#get_iter_at_mark m2 in
              let line_start = float start#line in
              let line_stop = float stop#line in
              let y1 = int_of_float ((line_start /. line_count) *. height) in
              let y2 = int_of_float ((line_stop /. line_count) *. height) in
              let y1 = y1 + alloc.Gtk.width - 1 in
              let y2 = y2 + alloc.Gtk.width + 1 in
              let width = width - 1 in
              let height = y2 - y1 in
              drawable#set_line_attributes ~width:1 ~style:`SOLID ();
              drawable#set_foreground bg;
              drawable#rectangle ~filled:true ~x:0 ~y:y1 ~width ~height ();
              drawable#set_foreground border;
              drawable#rectangle ~filled:false ~x:0 ~y:y1 ~width ~height ();
            end view#mark_occurrences_manager#table;
          | _ -> ()
      end;
      (** Errors *)
      List.iter begin fun (start, _, _) ->
        draw_marker start Oe_config.error_underline_color;
      end tag_error_bounds;
    with Gpointer.Null -> ()

  method private expose ev =
    if flag_underline then begin
      match view#get_window `TEXT with
        | Some window ->
          let vrect = view#visible_rect in
          let x0 = Gdk.Rectangle.x vrect in
          let y0 = Gdk.Rectangle.y vrect in
          (* Draw exposed area only *)
          let expose_area = GdkEvent.Expose.area ev in
          let ya = y0 + Gdk.Rectangle.y expose_area in
          let top, _ = view#get_line_at_y ya in
          let bottom, _ = view#get_line_at_y (ya + (Gdk.Rectangle.height expose_area)) in
          (*  *)
          let drawable = new GDraw.drawable window in
          drawable#set_foreground Oe_config.warning_underline_color;
          drawable#set_line_attributes ~width:1 ~style:`SOLID ();
          List.iter begin function
            | (start, stop, error) when (not (is_warning_unused error.Oe.er_level)) ->
              let start = ref (buffer#get_iter_at_mark (`MARK start)) in
              let stop = buffer#get_iter_at_mark (`MARK stop) in
              let stop = if bottom#compare stop < 0 then bottom else stop in
              while !start#compare stop < 0 do
                begin
                  try
                    if top#compare !start <= 0 && !start#chars_in_line > 1 then begin
                      start := forward_non_blank !start;
                      let iter =
                        if !start#ends_line then (raise Exit)
                        else
                          let line_end = !start#forward_to_line_end in
                          if stop#compare line_end <= 0 then stop else line_end
                      in
                      let ys, h = view#get_line_yrange !start in
                      let y = ys + h - y0 in
                      let x = ref (Gdk.Rectangle.x (view#get_iter_location !start) - x0) in
                      let x2 = Gdk.Rectangle.x (view#get_iter_location iter) - x0 in
                      let yu = y - 2 in
                      let segments = ref [] in
                      while !x <= x2 do
                        segments := (!x + 2, y) :: (!x, yu) :: !segments; x := !x + 4; (* 7 *)
                        (*segments := (!x + 4, y) :: (!x + 3, y) :: (!x + 1, yu) :: (!x, yu) :: !segments; x := !x + 6 (* 8 *)*)
                        (*segments := (!x + 8, y) :: (!x + 5, y) :: (!x + 2, yu) :: (!x, yu) :: !segments; x := !x + 11*)
                      done;
                      drawable#lines !segments;
                    end;
                  with Exit | Invalid_argument("PointArray.new") -> ()
                end;
                start := !start#forward_line
              done;
            | _ -> ()
          end tag_warning_bounds;
          Gdk.GC.set_fill drawable#gc `SOLID;
          false
        | _ -> false
    end else false

  initializer
    self#set_flag_underline flag_underline;
    (** Hide tooltip *)
    let unsticky _ = if sticky_popup then (self#hide_tooltip()); false in
    view#event#connect#focus_out ~callback:unsticky;
    view#event#connect#button_press ~callback:unsticky;
    view#event#connect#leave_notify ~callback:unsticky;
    (** View: on expose draw underline for warnings *)
    let signal_expose =
      ref (view#event#connect#after#expose ~callback:(fun ev -> (*Prf.crono Prf.prf_error_indication_view_expose*)self#expose ev))
    in
    ignore (vscrollbar#connect#value_changed ~callback:(fun () -> view#misc#handler_block !signal_expose));
    ignore (vscrollbar#connect#after#value_changed ~callback:(fun () ->
        Gmisclib.Idle.add ~prio:300 (fun () -> view#misc#handler_unblock !signal_expose)));
    (** Global_gutter: expose *)
    global_gutter#event#connect#expose ~callback:(fun _ -> (*Prf.crono Prf.prf_paint_global_gutter*) self#paint_global_gutter (); false);
    (** Global_gutter: button_press  *)
    ignore (global_gutter_ebox#event#connect#button_press ~callback:begin fun ev ->
      let alloc = vscrollbar#misc#allocation in
      let y = GdkEvent.Button.y ev in
      if y > float (alloc.Gtk.width) then begin
        let window = global_gutter#misc#window in
        let drawable = new GDraw.drawable window in
        let width, height = drawable#size in
        let height = float (height - 2 * alloc.Gtk.width) in
        let y = y -. (float alloc.Gtk.width) in
        let tooltip, iter =
          try
            let _, mark = List.find (fun (yy, _) -> let yy = float yy in yy -. 4. <= y && y <= yy +. 4.) (List.rev table) in
            true, (buffer#get_iter_at_mark (`MARK mark))
          with Not_found -> begin
            let line_count = float buffer#line_count in
            let line = int_of_float (y /. height *. line_count) in
            false, buffer#get_iter (`LINE line);
          end
        in
        view#scroll_lazy iter;
        buffer#place_cursor iter;
        if tooltip then begin
          Gmisclib.Idle.add ~prio:300 (fun () -> self#tooltip ~sticky:true (`ITER iter));
        end;
      end else begin
        let iter =
          match self#first_error_or_warning with
            | None -> buffer#start_iter
            | Some (start, _, _) -> buffer#get_iter_at_mark (`MARK start)
        in
        view#scroll_lazy iter;
        buffer#place_cursor iter;
      end;
      false
    end);
end




















