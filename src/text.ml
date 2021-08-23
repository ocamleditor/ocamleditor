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
open Text_util
open Miscellanea
open Cairo_drawable



(** Buffer *)
class buffer =
  let word_bound = Miscellanea.regexp "\\b" in
  fun ?project ?buffer ?file () ->
    let buffer = match buffer with None -> GText.buffer () | Some b -> b in
    let create_tmp_filename () = Filename.temp_file "buffer-" ".tmp", None in
    object (self)
      inherit GText.buffer buffer#as_buffer

      val undo = Gmisclib.Text.undo_manager ~buffer
      val mutable tab_width = 2
      val mutable tab_spaces = true
      val mutable signal_handlers = []
      val mutable tag_ocamldoc_paragraph : GText.tag option = None
      val mutable tmp_filename = "" (*tmp_filename*)
      val mutable orig_filename = ""
      val mutable project_tmp_path = None (*project_tmp_path*)

      initializer
        self#reset_tmp_filename ();
        orig_filename <- tmp_filename ^ ".orig";
        at_exit begin fun () ->
          match project with
          | None ->
              if Sys.file_exists orig_filename then Sys.remove orig_filename;
              if Sys.file_exists tmp_filename then Sys.remove tmp_filename
          | _ -> ()
        end

      method tmp_filename = tmp_filename
      method orig_filename = orig_filename

      method reset_tmp_filename () =
        let a, b =
          match project with
          | Some project ->
              begin
                match file with
                | Some file ->
                    begin
                      match Project.tmp_of_abs project file#filename with
                      | None -> create_tmp_filename ()
                      | (Some (tmp, relname) as temp) -> tmp // relname, temp
                    end;
                | _ -> create_tmp_filename ()
              end;
          | _ -> create_tmp_filename ()
        in
        tmp_filename <- a;
        project_tmp_path <- b


      method tag_ocamldoc_paragraph = tag_ocamldoc_paragraph

      method init_tags ?tags ?colors
          ?ocamldoc_paragraph_enabled
          ?ocamldoc_paragraph_bgcolor_1
          ?ocamldoc_paragraph_bgcolor_2 () =
        Lexical.init_tags ?tags ?colors
          ?ocamldoc_paragraph_enabled
          ?ocamldoc_paragraph_bgcolor_1
          ?ocamldoc_paragraph_bgcolor_2 self#as_gtext_buffer;
        let table = new GText.tag_table self#tag_table in
        tag_ocamldoc_paragraph <- Gaux.may_map ~f:(new GText.tag) (table#lookup "ocamldoc-paragraph");

      method as_gtext_buffer = (self :> GText.buffer)

      method undo = undo

      method add_signal_handler id = signal_handlers <- id :: signal_handlers
      method remove_signal_handler id = signal_handlers <- List.filter (fun x -> x != id) signal_handlers
      method block_signal_handlers () =
        List.iter (GtkSignal.handler_block buffer#as_buffer) signal_handlers;
      method unblock_signal_handlers () =
        List.iter (GtkSignal.handler_unblock buffer#as_buffer) signal_handlers;

      method tab_width = tab_width
      method set_tab_width x = tab_width <- x

      method tab_spaces = tab_spaces
      method set_tab_spaces x = tab_spaces <- x

      method file : Editor_file.file option = file

      method select_all () = self#select_range self#start_iter self#end_iter

      (* Deprecated: use get_iter_at_mark_opt *)
      method! get_iter_at_mark mark =
        new GText.iter (Gmisclib.Util.get_iter_at_mark_safe self#as_buffer (self#get_mark mark))

      method get_iter_at_mark_opt mark =
        match Gmisclib.Util.get_iter_at_mark_opt self#as_buffer (self#get_mark mark) with
        | Some i -> Some (new GText.iter i)
        | _ -> None

      method select_marks ~start ~stop =
        self#select_range (self#get_iter_at_mark stop) (self#get_iter_at_mark start);

      method selection_text () =
        let start, stop = self#selection_bounds in
        self#get_text ~start ~stop ()

      method get_line_at_iter (it : GText.iter) =
        self#get_text ~start:(it#set_line_index 0) ~stop:it#forward_line ()

      method indent = Alignment.indent ~buffer:self

      method select_word ?(iter=(self#get_iter `INSERT)) ?(pat=word_bound) ?(select=true) ?(search=true) () =
        let line = self#get_line_at_iter iter in
        let pos = iter#line_index in
        let start = try Str.search_backward pat line pos with Not_found -> 0 in
        if start = pos && pos > 0 then begin
          if Glib.Unichar.isspace iter#backward_char#char then begin
            if search then
              self#select_word ~iter:iter#backward_char ~pat ~select ~search ()
            else (iter, iter)
          end else self#select_word ~iter:iter#backward_char ~pat ~select ~search ()
        end else begin
          let start = if start = 0 then start else start + 1 in
          let stop = try Str.search_forward pat line pos with Not_found -> Glib.Utf8.length line in
          let start = iter#set_line_index start in
          let stop = iter#set_line_index stop in
          if select then self#select_range start stop;
          (start, stop)
        end

      method toggle_case () =
        undo#begin_block ~name:"toggle_case";
        let start, stop = self#get_iter `SEL_BOUND, self#get_iter `INSERT in
        let text = self#get_text ~start ~stop () in
        let uppercase = Glib.Utf8.uppercase text in
        let text = if text = uppercase then Glib.Utf8.lowercase text else uppercase in
        self#delete ~start ~stop;
        self#insert text;
        let stop = self#get_iter `INSERT in
        self#select_range (stop#backward_chars (Glib.Utf8.length text)) stop;
        undo#end_block ();


      method save_buffer ?(filename=tmp_filename) () =
        let text = buffer#get_text () in
        let text = match project with Some project -> Project.convert_from_utf8 project text | _ -> text in
        (*Glib.Convert.convert_with_fallback ~fallback:"?"
          ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset text
          in*)
        Miscellanea.mkdir_p (Filename.dirname filename);
        let chan = open_out_bin filename in
        begin
          try
            output_string chan text;
            close_out chan;
            filename, project_tmp_path
          with ex -> (close_out chan; raise ex)
        end;
    end

(** View *)
and view ?project ?buffer () =
  let buffer = match buffer with None -> new buffer ?project () | Some b -> b in
  let view = GText.view ~buffer:buffer#as_gtext_buffer () in
  let create_highlight_current_line_tag () =
    buffer#create_tag ~name:(sprintf "highlight_current_line_tag_%f" (Unix.gettimeofday())) []
  in
  object (self)
    inherit GText.view view#as_view as super

    val mutable options = new Text_options.options ()
    val mutable tbuffer = buffer
    val mutable prev_line_background = 0
    val mutable highlight_current_line_tag = create_highlight_current_line_tag ()
    val mutable current_matching_tag_bounds = []
    val mutable current_matching_tag_bounds_draw = []
    val mutable approx_char_width = 0
    val line_num_labl = Line_num_labl.create()
    val visible_height = new GUtil.variable 0
    val mutable signal_expose : GtkSignal.id option = None
    val gutter = Gutter.create()
    val mutable gutter_icons = []
    val hyperlink = Gmisclib.Text.hyperlink ~view ()
    val mutable realized = false
    val mutable signal_id_highlight_current_line = None
    val mutable mark_occurrences_manager = None
    val mutable current_line_border_x1 = 0
    method set_current_line_border_x1 x = current_line_border_x1 <- x
    val mutable current_line_border_x2 = 0
    method set_current_line_border_x2 x = current_line_border_x2 <- x

    method project = project

    method mark_occurrences_manager = match mark_occurrences_manager with Some x -> x | _ -> assert false

    method as_gtext_view = (self :> GText.view)
    method tbuffer = buffer

    method options = options

    method realized = realized
    method set_realized x = realized <- x
    method hyperlink = hyperlink

    method signal_expose = signal_expose
    method gutter = gutter

    method! set_buffer buf =
      let tbuf = new buffer ~buffer:buf () in
      tbuffer <- tbuf;
      super#set_buffer buf

    method approx_char_width = approx_char_width

    method modify_font fontname =
      self#misc#modify_font_by_name fontname;
      approx_char_width <- GPango.to_pixels (self#misc#pango_context#get_metrics())#approx_char_width;
      Gmisclib.Idle.add self#draw_gutter

    method create_highlight_current_line_tag () =
      let tag_table = new GText.tag_table buffer#tag_table in
      tag_table#remove highlight_current_line_tag#as_tag;
      highlight_current_line_tag <- create_highlight_current_line_tag();
      self#options#set_highlight_current_line options#highlight_current_line

    method line_num_labl = line_num_labl

    method highlight_current_line_tag = highlight_current_line_tag

    method scroll_lazy iter =
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        self#scroll_to_iter ~use_align:(self#scroll_to_iter iter) ~xalign:1.0 ~yalign:0.38 iter |> ignore;
        Gmisclib.Idle.add ~prio:300 (fun () -> GtkBase.Widget.queue_draw self#as_widget)
      end;

    method scroll dir =
      let rect = self#visible_rect in
      let it = match dir with
        | `UP ->
            let y = Gdk.Rectangle.y rect in
            let it, _ = self#get_line_at_y y in
            it#backward_lines 2;
        | `DOWN ->
            let y = Gdk.Rectangle.y rect + Gdk.Rectangle.height rect + 2 in (* 2 = pixels below ocamldoc paragraphs *)
            let it, _ = self#get_line_at_y y in
            it#forward_line
      in
      ignore (self#scroll_to_iter it);
      self#place_cursor_onscreen() |> ignore;
      Gmisclib.Idle.add ~prio:300 (fun () -> GtkBase.Widget.queue_draw self#as_widget)

    method goto () = Dialog_goto.show ~view:self ()

    method private matching_delim_apply_tag (text : string) (offset : int) = (*(None : (int * int * int * int) option)*)
      match Delimiters.find_match ~utf8:false text offset with
      | None -> None
      | Some (lstart, lstop, rstart, rstop) as delim when lstop <> rstart ->
          let start = self#buffer#get_iter (`OFFSET lstart) in
          let stop = self#buffer#get_iter (`OFFSET lstop) in
          current_matching_tag_bounds <-
            ((self#buffer#create_mark ~name:"delim_left_start" start),
             (self#buffer#create_mark ~name:"delim_left_stop" stop)) :: [];
          self#buffer#apply_tag_by_name "tag_matching_delim" ~start ~stop;
          let start = self#buffer#get_iter (`OFFSET rstart) in
          let stop = self#buffer#get_iter (`OFFSET rstop) in
          current_matching_tag_bounds <-
            ((self#buffer#create_mark ~name:"delim_right_start" start),
             (self#buffer#create_mark ~name:"delim_right_stop" stop)) :: current_matching_tag_bounds;
          self#buffer#apply_tag_by_name "tag_matching_delim" ~start ~stop;
          current_matching_tag_bounds_draw <- current_matching_tag_bounds;
          GtkBase.Widget.queue_draw self#as_widget;
          delim
      | Some (_, lstop, rstart, _) (*as delim*) when lstop = rstart ->
          ignore (self#innermost_enclosing_delim text lstop);
          None
      | _ -> None

    method matching_delim_remove_tag () =
      List.iter begin function (start, stop) ->
        let mstart = (`MARK start) in
        let mstop = (`MARK stop) in
        let start = self#buffer#get_iter_at_mark mstart in
        let stop = self#buffer#get_iter_at_mark mstop in
        self#buffer#remove_tag_by_name "tag_matching_delim" ~start ~stop;
        self#buffer#delete_mark mstart;
        self#buffer#delete_mark mstop;
      end current_matching_tag_bounds;
      current_matching_tag_bounds_draw <- [];
      current_matching_tag_bounds <- [];

    method matching_delim () =
      self#matching_delim_remove_tag();
      let pos = self#buffer#get_iter `INSERT in
      let start = pos#backward_chars 5 in
      let stop = pos#forward_chars 5 in
      let text55 = Glib.Convert.convert_with_fallback ~fallback:"?"
          ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset (self#buffer#get_text ~start ~stop ()) in
      let text = Glib.Convert.convert_with_fallback ~fallback:"?"
          ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset (self#buffer#get_text ()) in
      if Delimiters.is_delimiter ~utf8:false text55 (pos#offset - start#offset) then begin
        ignore (self#matching_delim_apply_tag text pos#offset)
      end else begin
        try
          ignore (self#innermost_enclosing_delim text pos#offset);
        with ex -> eprintf "%s\n%!" (Printexc.to_string ex);
      end

    method matching_delim_goto ?(select=false) ?(strict=true) () =
      match self#buffer#selection_bounds with
      | b1, b2 when (not (b1#equal b2)) ->
          self#buffer#place_cursor ~where:b1;
          self#matching_delim_goto ~select ~strict:false ()
      (*let b1 = if List.mem b1#char blanks then
        b1#forward_find_char not_blank else b1 in
        let b2 = if List.mem b2#char blanks then
        (b2#backward_find_char not_blank)#forward_to_line_end else b2 in
        self#buffer#select_range b1 b2*)
      | _ -> begin
          match current_matching_tag_bounds with
          | [(rstart, rstop); (lstart, lstop)] ->
              let ins = self#buffer#get_iter `INSERT in
              let right_start = self#buffer#get_iter_at_mark (`MARK rstart) in
              let left_stop = self#buffer#get_iter_at_mark (`MARK lstop) in
              if select then begin
                let start =
                  if strict then begin
                    let start = left_stop#forward_find_char not_blank in
                    let start = start#set_line_index 0 in
                    if start#compare left_stop <= 0 then left_stop else start
                  end else (self#buffer#get_iter_at_mark (`MARK lstart))
                in
                let stop =
                  if strict then begin
                    let stop = right_start#backward_find_char not_blank in
                    let stop = stop#forward_line in
                    if stop#compare right_start <= 0 then stop else right_start
                  end else (self#buffer#get_iter_at_mark (`MARK rstop))
                in
                self#buffer#select_range start stop;
              end else begin
                (*let right_stop = Gtk_util.get_iter_mark self#buffer rstop in
                  let left_start = Gtk_util.get_iter_mark self#buffer lstart in*)
                let right_start, left_stop = if right_start#compare left_stop <= 0
                  then (right_start, left_stop) else (left_stop, right_start) in
                let left_is_nearest = abs (ins#offset - right_start#offset) > abs (ins#offset - left_stop#offset)
                in
                if left_is_nearest
                then (self#buffer#place_cursor ~where:right_start)
                else (self#buffer#place_cursor ~where:left_stop);
                ignore (self#scroll_to_mark `INSERT);
              end
          | _ -> ()
        end

    method private innermost_enclosing_delim (text : string) (offset : int) = (*(None : (int * int * int * int) option)*)
      (*Prf.crono Prf.innermost_enclosing_delim (fun () ->*)
      match Delimiters.find_innermost_enclosing_delim ~utf8:false text offset with
      | ((start, stop) :: _) (*as stack*)  ->
          if stop = offset
          then (self#innermost_enclosing_delim text start)
          else (self#matching_delim_apply_tag text start);
      | _ -> None (* ) () *)

    method current_matching_tag_bounds = current_matching_tag_bounds

    method get_location_at_cursor () =
      let iter = self#buffer#get_iter `INSERT in
      let rect_it = self#get_iter_location iter in
      let rect_vis = self#visible_rect in
      let win = (match self#get_window `WIDGET
                 with None -> failwith "Text.text#get_location_at_cursor `WIDGET = None" | Some w -> w) in
      let x, y = Gdk.Window.get_position win in
      let win = Gdk.Window.get_parent win in
      let x1, y1 = Gdk.Window.get_position win in
      let vxs, vys = x + x1, y + y1 in
      let xb, yb = Gdk.Rectangle.x rect_it, Gdk.Rectangle.y rect_it in
      let vxb, vyb = Gdk.Rectangle.x rect_vis, Gdk.Rectangle.y rect_vis in
      (vxs + xb - vxb + (self#get_border_window_size `LEFT)),
      (vys + yb - vyb + Gdk.Rectangle.height rect_it)

    method get_location_at_iter iter =
      let rect = view#get_iter_location iter in
      let x = Gdk.Rectangle.x rect in
      let y = Gdk.Rectangle.y rect in
      let x0, y0 =
        let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.get_parent self#misc#window) in
        let win = (match view#get_window `TEXT with None -> assert false | Some w -> w) in
        let px, py = Gdk.Window.get_pointer_location win in
        (pX - px), (pY - py)
      in
      let vrect = view#visible_rect in
      let x = x - Gdk.Rectangle.x vrect in
      let y = y - Gdk.Rectangle.y vrect in
      let x = x0 + x in
      let y = y0 + y in
      x, y

    method get_location_top_right () =
      let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.get_parent self#misc#window) in
      let win = (match self#get_window `WIDGET
                 with None -> failwith "Text.text#get_location_top_right `WIDGET = None" | Some w -> w) in
      let px, py = Gdk.Window.get_pointer_location win in
      (pX - px + self#misc#allocation.Gtk.width),
      (pY - py)

    method get_scroll_top () =
      let vrect = self#visible_rect in
      let y0 = Gdk.Rectangle.y vrect in
      let start, _ = self#get_line_at_y y0 in
      start#offset

    method draw_current_line_background ?(force=false) (ins : GText.iter) =
      let cur_line = ins#line in
      if cur_line <> prev_line_background || ins#ends_line || force then begin
        let prev = self#buffer#get_iter (`LINE prev_line_background) in
        let prev_0 = prev#set_line_offset 0 in
        self#buffer#remove_tag highlight_current_line_tag
          ~start:prev_0#backward_line ~stop:prev#forward_line#forward_to_line_end;
        let ins_0 = ins#set_line_offset 0 in
        let ins_1 = ins_0#backward_char in
        let start = if ins_1#starts_line then ins_0 else ins_1 in
        let stop = ins_0#forward_char in
        self#buffer#apply_tag highlight_current_line_tag ~start ~stop;
        prev_line_background <- cur_line;
      end

    method private set_gutter_size () =
      let gutter_fold_size = gutter.Gutter.fold_size + 4 in (* 4 = borders around fold_size *)
      let fixed = if options#show_markers then Gutter.icon_size + gutter_fold_size else 0 in
      let size =
        if options#show_line_numbers then begin
          approx_char_width <- GPango.to_pixels (self#misc#pango_context#get_metrics())#approx_char_width;
          let max_line = buffer#end_iter#line in
          let n_chars = String.length (string_of_int (max_line + 1)) in
          gutter.Gutter.chars <- n_chars;
          (max (Gutter.icon_size * 2) (n_chars * approx_char_width + gutter.Gutter.spacing)) + gutter_fold_size
        end else begin
          gutter.Gutter.chars <- 0;
          fixed
        end;
      in
      self#set_border_window_size ~typ:`LEFT ~size;
      gutter.Gutter.size <- size;
      gutter.Gutter.fold_x <- size - gutter.Gutter.fold_size; (* 2 borders on the right of fold_size *)

    method private gutter_icons_same_pos child x y ym =
      match List_opt.assoc ym gutter_icons with
      | Some other ->
          if child#misc#parent <> None && other#misc#parent <> None && other#misc#get_oid <> child#misc#get_oid then begin
            let offset = (gutter.Gutter.size - gutter.Gutter.fold_size) / 4 in
            self#move_child ~child:other ~x:(x - offset) ~y;
            self#move_child ~child ~x:(x + offset) ~y
          end
      | _ -> ()

    method draw_gutter () = (* 0.008 *)
      (*Prf.crono Prf.prf_draw_gutter begin fun () ->*)
      try
        self#set_gutter_size();
        let vrect = self#visible_rect in
        let h0 = Gdk.Rectangle.height vrect in
        let y0 = Gdk.Rectangle.y vrect in
        let start, _ = self#get_line_at_y y0 in
        let stop, _ = self#get_line_at_y (y0 + h0) in
        (* Line Numbers *)
        if realized && options#show_line_numbers then begin
          (*Prf.crono Prf.prf_line_numbers begin fun () ->*)
          Line_num_labl.reset line_num_labl;
          let iter = ref start#backward_line in
          let stop = stop#forward_line in
          let x = gutter.Gutter.size - gutter.Gutter.fold_size - 4 - gutter.Gutter.spacing in
          let y = ref 0 in
          let h = ref 0 in
          let num = ref 0 in
          while not (!iter#equal stop) do
            num := !iter#line + 1;
            let yl, hl = self#get_line_yrange !iter in
            y := yl + self#pixels_above_lines;
            h := hl;
            Line_num_labl.print ~view:self ~num:!num ~x ~y:!y ~width_chars:gutter.Gutter.chars line_num_labl;
            iter := !iter#forward_line;
          done;

          let y = !y  + !h in
          incr num;
          Line_num_labl.print ~view:self ~num:!num ~x ~y ~width_chars:gutter.Gutter.chars line_num_labl
          (*end()*)
        end;
        (* Markers *)
        (*Prf.crono Prf.prf_other_markers begin fun () ->*)
        let x = (gutter.Gutter.size - gutter.Gutter.fold_size - 3 - Gutter.icon_size) / 2 (*1*) in
        List.iter begin fun mark ->
          match mark.Gutter.icon_pixbuf with
          | Some pixbuf ->
              begin
                match buffer#get_iter_at_mark_opt (`MARK mark.Gutter.mark) with
                | Some mark_iter ->
                    let ym, h = self#get_line_yrange mark_iter in
                    let y = ym - y0 in
                    if false then
                      Line_num_labl.hide (y + self#pixels_above_lines) line_num_labl;
                    let y = y + (h - Gutter.icon_size) / 2 in
                    let child = match mark.Gutter.icon_obj with
                      | None ->
                          let ebox = GBin.event_box () in
                          ebox#misc#set_property "visible-window" (`BOOL false);
                          let icon = GMisc.image ~pixbuf () in
                          ebox#add icon#coerce;
                          Gaux.may mark.Gutter.callback ~f:begin fun callback ->
                            ignore (ebox#event#connect#enter_notify ~callback:begin fun ev ->
                                let window = GdkEvent.get_window ev in
                                Gdk.Window.set_cursor window (Gdk.Cursor.create `HAND1);
                                true
                              end);
                            ignore (ebox#event#connect#leave_notify ~callback:begin fun ev ->
                                let window = GdkEvent.get_window ev in
                                Gdk.Window.set_cursor window (Gdk.Cursor.create `ARROW);
                                true
                              end);
                            ebox#event#connect#button_press ~callback:begin fun _ ->
                              self#misc#grab_focus();
                              callback mark.Gutter.mark
                            end
                          end;
                          let child = ebox#coerce in
                          ignore (child#misc#connect#destroy ~callback:(fun () -> gutter_icons <- List.remove_assoc ym gutter_icons));
                          self#add_child_in_window ~child ~which_window:`LEFT ~x ~y;
                          mark.Gutter.icon_obj <- Some child;
                          gutter_icons <- (ym, child) :: gutter_icons;
                          child
                      | Some child -> self#move_child ~child ~x ~y; child
                    in
                    self#gutter_icons_same_pos child x y ym;
                | _ -> ()
              end;
          | _ -> ()
        end gutter.Gutter.markers;
        (*end ()*)
      with ex -> eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())
    (*end ()*)

    method private expose _ =
      try
        begin
          match self#get_window `TEXT with
          | Some window ->
              let vrect       = self#visible_rect in
              let h0          = Gdk.Rectangle.height vrect in
              let w0          = Gdk.Rectangle.width vrect in
              let y0          = Gdk.Rectangle.y vrect in
              let start, _    = self#get_line_at_y y0 in
              let stop, _     = self#get_line_at_y (y0 + h0) in
              visible_height#set h0;
              (* Expose area *)
            (*
            let expose_area = GdkEvent.Expose.area ev in
            let expose_y    = y0 + Gdk.Rectangle.y expose_area in
            let expose_top, _ = self#get_line_at_y expose_y in
            let expose_bottom, _ = self#get_line_at_y (expose_y + (Gdk.Rectangle.height expose_area)) in
            *)
              let expose_y = y0 in
              let expose_top, _ = self#get_line_at_y expose_y in
              let expose_bottom, _ = self#get_line_at_y (expose_y + h0) in
              (*  *)
              let adjust      = Oe_config.current_line_border_adjust in
              let hadjust     = int_of_float self#hadjustment#value - self#left_margin in
              let drawable    = GDraw.Cairo.create window in
              let { Cairo.x; y; w; h } = Cairo.clip_extents drawable in
              (* Indentation guidelines *)
              if options#show_indent_lines && not options#show_whitespace_chars
              then (Text_indent_lines.draw_indent_lines self drawable) start stop y0;
              (* Gutter border *)
              begin
                match self#get_window `LEFT with
                | Some window ->
                    let drawable = GDraw.Cairo.create window in
                    let { Cairo.x; y; w; h } = Cairo.clip_extents drawable in
                    set_line_attributes drawable ~width:2 ~style:`SOLID ();
                    set_foreground drawable gutter.Gutter.border_color;
                    line drawable (gutter.Gutter.size - 1) 0 (gutter.Gutter.size - 1) h0;
                    if gutter.Gutter.fold_size > 0 then
                      line drawable (gutter.Gutter.size - 2 - gutter.Gutter.fold_size) 0 (gutter.Gutter.size - 2 - gutter.Gutter.fold_size) h0;
                | _ -> ()
              end;
              (* Right margin line *)
              begin
                match options#visible_right_margin with
                | Some (column, color) ->
                    let x = approx_char_width * column - hadjust - 1 in (* -1 per evitare sovrapposizione col cursore *)
                    set_line_attributes drawable ~width:4 ~style:`SOLID ();
                    set_foreground drawable color;
                    line drawable x 0 x h0;
                | _ -> ()
              end;
              (* ocamldoc_paragraph_bgcolor_enabled *)
              if Oe_config.ocamldoc_paragraph_border_enabled
              then (self#draw_paragraph_border drawable start stop y0 w0);
              (* Special bookmarks *)
              let iter = ref (expose_top#set_line_index 0) in
              begin
                match project with
                | Some project ->
                    begin
                      match buffer#file with
                      | Some file ->
                          let filename = file#filename in
                          while !iter#compare expose_bottom < 0 do
                            begin
                              match Project.find_bookmark project filename buffer#as_gtext_buffer !iter with
                              | Some bm when bm.Oe.bm_num >= Bookmark.limit ->
                                  set_line_attributes drawable ~width:2 ~style:`SOLID ();
                                  set_foreground drawable options#indent_lines_color_dashed (*options#text_color*);
                                  let y, h = view#get_line_yrange !iter in
                                  let y = y - y0 + h in
                                  line drawable 0 y w0 y;
                              | _ -> ()
                            end;
                            iter := !iter#forward_line;
                          done;
                      | _ -> ()
                    end;
                | _ -> ()
              end;
              (* Whitespace characters *)
              if options#show_whitespace_chars then begin
                let iter        = ref expose_top in
                let pango       = self#misc#pango_context in
                let layout      = pango#create_layout in
                let draw _iter text =
                  layout#set_text text;
                  set_foreground drawable options#indent_lines_color_solid;
                  Cairo_pango.show_layout drawable layout#as_layout;
                  (*drawable#put_layout ~x ~y ~fore:options#base_color layout;
                    drawable#put_layout ~x ~y ~fore:options#indent_lines_color_solid layout;*)
                in
                while !iter#compare expose_bottom < 0 do
                  let line_num = !iter#line in
                  while !iter#line = line_num do
                    let char = !iter#char in
                    begin
                      match char with
                      | 32 ->
                          let start = !iter in
                          let pos = start#line_index in
                          iter := !iter#forward_find_char not_blank;
                          draw start (create_middot_string (!iter#line_index - pos));
                      | 13 -> draw !iter whitespace_crlf
                      | 9 -> draw !iter whitespace_tab
                      | _ when !iter#ends_line -> draw !iter whitespace_lf
                      | _ -> ()
                    end;
                    iter := !iter#forward_char
                  done;
                done
              end;
              (* Dot leaders *)
              if options#show_dot_leaders && not options#show_whitespace_chars then begin
                (*Prf.crono Prf.prf_draw_dot_leaders begin fun () ->*)
                (*Gdk.GC.set_fill drawable#gc `SOLID;*)
                set_line_attributes drawable ~width:1 ~style:Oe_config.dash_style ();
                set_foreground drawable options#text_color;
                let offset = self#left_margin - hadjust in
                Alignment.iter ~start:expose_top ~stop:expose_bottom begin fun _ _ start stop _ ->
                  let start = start#forward_char in
                  let len = stop#line_index - start#line_index in
                  if len > 2 then begin
                    let x1 = approx_char_width * start#line_index + offset in
                    let x2 = approx_char_width * (stop#line_index - 1) + offset in
                    let y, h = self#get_line_yrange start in
                    let y = y - y0 + h - 3 (*(min 3 (h / 5))*) in
                    (*Gdk.GC.set_dashes drawable#gc ~offset:(x2 - 6 (*- x1*)) [1; approx_char_width - 1];*)
                    (*Gdk.GC.set_dashes drawable#gc ~offset:(x2 - approx_char_width - 2) [1; approx_char_width - 1];*)
                    line drawable x1 y x2 y;
                  end
                end
                (*end;*)
              end (*()*);
              (* Current line border *)
              begin
                if self#has_focus && options#current_line_border_enabled then begin
                  match options#highlight_current_line with
                  | Some _ ->
                      let iter = buffer#get_iter `INSERT in
                      let y, h = view#get_line_yrange iter in
                      let y = y - y0 in
                      if iter#equal buffer#end_iter && iter#line_index = 0 then begin
                        (* Fix for draw_current_line_background *)
                        set_foreground drawable options#current_line_bg_color;
                        rectangle drawable ~x:self#left_margin ~y ~filled:true ~width:w0 ~height:h ();
                      end;
                      set_line_attributes drawable ~join:Oe_config.current_line_join ~width:Oe_config.current_line_width ~style:Oe_config.current_line_style ();
                      set_foreground drawable options#current_line_border_color;
                      (*Gdk.GC.set_dashes drawable#gc ~offset:1 Oe_config.on_off_dashes;*)
                      rectangle drawable ~x:current_line_border_x1 ~y ~filled:false
                        ~width:(w0 - current_line_border_x2) ~height:(h - adjust) ();
                  | _ -> ()
                end;
              end;
              (* Border around matching delimiters *)
              begin
                match current_matching_tag_bounds_draw with
                | (lstart, lstop) :: (rstart, rstop) :: [] ->
                    set_foreground drawable Oe_config.matching_delim_border_color;
                    set_line_attributes drawable ~width:1 ~style:`SOLID ();
                    let draw start stop =
                      match buffer#get_iter_at_mark_opt (`MARK start) with
                      | Some start ->
                          begin
                            match buffer#get_iter_at_mark_opt (`MARK stop) with
                            | Some stop ->
                                let yl1, hl1 = view#get_line_yrange start in
                                let yl1 = yl1 - y0 in
                                (* count_displayed_lines *)
                                let iter = ref (stop#set_line_index 0) in
                                let x_chars = ref 0 in
                                let lines_displayed = ref 1 in (* n-th display-line where "start" lies, counting from 1 *)
                                while not (!iter#equal start) do
                                  if !iter#char = 9 then begin
                                    x_chars := ((!x_chars / 8) * 8 + 8);
                                  end else (incr x_chars);
                                  iter := !iter#forward_char;
                                  if view#starts_display_line !iter then (x_chars := 0; incr lines_displayed)
                                done;
                                (* count how many display-lines constitute the line *)
                                let n_display_lines = ref !lines_displayed in
                                let i = stop#copy in
                                while view#forward_display_line i && i#line = stop#line do
                                  incr n_display_lines
                                done;
                                (*  *)
                                let x = approx_char_width * !x_chars - hadjust - 1 in (* -1 per evitare sovrapposizione col cursore *)
                                let width_chars = stop#line_index - start#line_index in
                                let width = approx_char_width * width_chars in
                                let pango = self#misc#pango_context in
                                let metrics = pango#get_metrics() in
                                let height = (metrics#ascent + metrics#descent) / Pango.scale -  1 in
                                let y =
                                  if !lines_displayed > 1 (*0 ?*)
                                  then yl1 + ((!lines_displayed - 1) * (hl1 / !n_display_lines))
                                  else yl1 + view#pixels_above_lines
                                in
                                rectangle drawable ~x ~y ~width ~height ()
                            | _ -> ()
                          end
                      | _ -> ()
                    in
                    draw lstart lstop;
                    draw rstart rstop;
                | _ -> ()
              end;
              false;
          | _ -> false
        end;
      with ex ->
        Printf.eprintf "File \"text.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
        false

    method private draw_paragraph_border drawable start stop y0 w0 =
      match buffer#tag_ocamldoc_paragraph with
      | Some tag ->
          begin
            let start = ref (start#set_line_index 0) in
            let stop = stop#forward_line#set_line_index 0 in
            match Preferences.preferences#get.Preferences.pref_ocamldoc_paragraph_bgcolor_1 with
            | Some color ->
                set_foreground drawable (`NAME (Color.add_value color 0.08));
                set_line_attributes drawable ~width:1 ~style:`SOLID ();
                let hadjust = int_of_float self#hadjustment#value in
                while !start#forward_line#compare stop <= 0 && not (!start#equal self#buffer#end_iter) do
                  if !start#has_tag tag then begin
                    let x = 0 - hadjust + self#left_margin in
                    let y1, _ = view#get_line_yrange !start in
                    let y = y1 - y0 in
                    let width = w0 + hadjust - self#left_margin - 2 in
                    let stop_tag = !start#forward_to_tag_toggle (Some tag) in
                    let y2, h = view#get_line_yrange stop_tag in
                    let height = y2 - y0 + h - y in
                    rectangle drawable ~x ~y ~width ~height ();
                    start := stop_tag#set_line_index 0;
                  end;
                  start := !start#forward_line;
                done
            | _ -> ()
          end
      | _ -> ()

    initializer
      ignore (options#connect#mark_occurrences_changed ~callback:(fun _ -> self#mark_occurrences_manager#mark()));
      ignore (options#connect#after#mark_occurrences_changed ~callback:begin function
        | true, color ->
            self#mark_occurrences_manager#tag#set_property (`BACKGROUND_GDK (GDraw.color (`NAME color)));
        | _ -> ()
        end);
      ignore (options#connect#after#line_numbers_changed ~callback:begin fun _ ->
          Line_num_labl.reset line_num_labl;
          Gmisclib.Idle.add self#draw_gutter
        end);
      ignore (options#connect#line_numbers_font_changed ~callback:begin fun fontname ->
          Line_num_labl.iter (fun x -> x#misc#modify_font_by_name fontname) line_num_labl
        end);
      options#set_line_numbers_font view#misc#pango_context#font_name;
      ignore (options#connect#after#show_markers_changed ~callback:(fun _ -> Gmisclib.Idle.add self#draw_gutter));
      ignore (options#connect#word_wrap_changed ~callback:(fun x -> self#set_wrap_mode (if x then `WORD else `NONE)));
      ignore (options#connect#after#highlight_current_line_changed ~callback:begin fun x ->
          prev_line_background <- 0;
          match x with
          | None ->
              Gaux.may signal_id_highlight_current_line ~f:begin fun id ->
                self#tbuffer#remove_signal_handler id;
                GtkSignal.disconnect self#tbuffer#as_buffer id;
                signal_id_highlight_current_line <- None
              end
          | Some color ->
              options#set_current_line_bg_color (`NAME color);
              options#set_current_line_border_color
                (Oe_config.current_line_border_color (Color.add_value ?sfact:None) color);
              Gmisclib.Util.set_tag_paragraph_background highlight_current_line_tag color;
              let id = self#buffer#connect#mark_set ~callback:begin fun iter mark ->
                  match GtkText.Mark.get_name mark with
                  | Some name when name = "insert" -> self#draw_current_line_background iter
                  | _ -> ()
                end in
              signal_id_highlight_current_line <- Some id;
              self#tbuffer#add_signal_handler id;
              self#draw_current_line_background ~force:true (self#buffer#get_iter `INSERT)
        end);
      Text_init.key_press self;
      Text_init.realize self;
      Text_init.select_lines_from_gutter self;
      (* Margin and line spacings *)
      (* To avoid strange application crash, avoid to draw the border of
         matching delimiters when we are in the middle of an insert_text event.
         This is done by setting current_matching_tag_bounds_draw to [], still
         keeping marks in current_matching_tag_bounds to be used for syntax
         coloring after the insert_text event. *)
      ignore (buffer#connect#insert_text ~callback:(fun _ _ -> current_matching_tag_bounds_draw <- []));
      (* Expose *)
      signal_expose <- Some (self#misc#connect#after#draw ~callback:self#expose);
      ignore (visible_height#connect#changed ~callback:(fun _ -> self#draw_gutter()));
      (* Refresh gutter and right margin line when scrolling *)
      ignore (self#hadjustment#connect#after#value_changed ~callback:begin fun _ ->
          (* Redraw the entire window on horizontal scroll to refresh right margin *)
          print_endline "----- notify_hadjustment";
          GtkBase.Widget.queue_draw self#as_widget
        end
        );
      ignore (self#vadjustment#connect#after#value_changed ~callback:begin fun _ ->
          (* Update gutter on vertical scroll changes *)

          Gmisclib.Idle.add self#draw_gutter;
          Gmisclib.Idle.add ~prio:300 (fun () -> GtkBase.Widget.queue_draw self#as_widget)
        end
        );
      (* Fix bug in draw_current_line_background *)
      let before, after =
        let old_mark_occurrences = ref None in
        let mark = ref None in
        begin fun ~name:_ ->
          old_mark_occurrences := Some self#options#mark_occurrences;
          self#options#set_mark_occurrences (false, "");
          mark := Some (buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Text.initializer")*) (buffer#get_iter `INSERT))
        end,
        begin fun ~name:_ ->
          Option.iter self#options#set_mark_occurrences !old_mark_occurrences;
          match !mark with
          | Some m ->
              let iter = ref (buffer#get_iter_at_mark (`MARK m)) in
              let stop = buffer#get_iter `INSERT in
              while !iter#compare buffer#end_iter < 0 && !iter#compare stop <= 0 do
                self#draw_current_line_background ~force:true !iter;
                iter := !iter#forward_line;
              done;
              buffer#delete_mark (`MARK m);
              mark := None
          | _ -> ()
        end
      in
      ignore (buffer#undo#connect#undo ~callback:before);
      ignore (buffer#undo#connect#after#undo ~callback:after);
      ignore (buffer#undo#connect#redo ~callback:before);
      ignore (buffer#undo#connect#after#redo ~callback:after);
      Gmisclib.Idle.add (fun () -> self#draw_current_line_background ~force:true (buffer#get_iter `INSERT));
      mark_occurrences_manager <- Some (new Mark_occurrences.manager ~view:self);
  end
