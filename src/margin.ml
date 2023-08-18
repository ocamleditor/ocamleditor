open Gutter

class virtual margin () =
  object (self)
    method virtual is_visible : bool
    method virtual size : int (* width of the margin in pixels *)
    method virtual draw :
      view:GText.view ->
      top:int -> left:int -> height:int ->
      start:GText.iter -> stop:GText.iter -> unit
  end

class line_numbers () =
  object (self)
    inherit margin ()
    val labels = Line_num_labl.create()
    method is_visible = true
    method size = 50 (* TODO depends on the font size *)

    method draw ~view ~top ~left ~height ~start ~stop =
      Line_num_labl.reset labels;
      let iter = ref start#backward_line in
      let stop = stop#forward_line in
      let y = ref 0 in
      let h = ref 0 in
      let num = ref 0 in
      while not (!iter#equal stop) do
        num := !iter#line + 1;
        let yl, hl = view#get_line_yrange !iter in
        y := yl - top + view#pixels_above_lines;
        h := hl;
        self#print_numbers ~view ~num:!num ~left ~top:!y labels;
        iter := !iter#forward_line;
      done;
      let y = !y  + !h in
      incr num;
      self#print_numbers ~view ~num:!num ~left ~top:y labels

    method private print_numbers ~view ~left ~top ~num labels =
      let open Line_num_labl in
      let open Settings_t in
      let text = string_of_int num in
      let label = match labels.free with
        | label :: tl ->
            labels.free <- tl;
            label#set_text text;
            label
        | [] ->
            let label = GMisc.label ~xalign:1.0 ~yalign:0.5 ~text ~show:false () in
            (*label#misc#modify_fg [`NORMAL, `COLOR (view#misc#style#fg `NORMAL)];*)
            label#misc#modify_font_by_name Preferences.preferences#get.editor_base_font;
            view#add_child_in_window ~child:label#coerce ~which_window:`LEFT ~x:0 ~y:0;
            label
      in
      (match List.assoc_opt top labels.locked with Some x -> x#misc#hide() | _ -> ());
      labels.locked <- (top, label) :: labels.locked;
      (*      label#set_width_chars 3;*)
      label#misc#show();
      let width = max label#misc#allocation.Gtk.width labels.max_width in
      view#move_child ~child:label#coerce ~x:left ~y:top;
      if width > labels.max_width then (labels.max_width <- width)

  end

class container (view : GText.view) =
  let gutter = Gutter.create () in
  object (self)
    val update = new update
    val mutable childs : margin list = []
    val mutable width = 0
    val line_num_labl = Line_num_labl.create()
    val mutable icons = []
    val mutable approx_char_width = 0

    method gutter = gutter
    method approx_char_width = approx_char_width

    method add margin = childs <- margin :: childs

    method draw () =
      let vrect = view#visible_rect in
      let height = Gdk.Rectangle.height vrect in
      let top = Gdk.Rectangle.y vrect in
      let start, _ = view#get_line_at_y top in
      let stop, _ = view#get_line_at_y (top + height) in
      let size =
        childs
        |> List.fold_left begin fun left margin ->
          if margin#is_visible then begin
            margin#draw ~view ~top ~left ~height ~start ~stop;
            left + margin#size
          end else left
        end 0
      in
      gutter.size <- size;
      view#set_border_window_size ~typ:`LEFT ~size;
      approx_char_width <- GPango.to_pixels (view#misc#pango_context#get_metrics())#approx_digit_width;
      update#call ();

    method private set_size_old show_markers show_line_numbers approx_char_width =
      let gutter_fold_size = gutter.fold_size + 4 in (* 4 = borders around fold_size *)
      let fixed = if show_markers then icon_size + gutter_fold_size else 0 in
      let size =
        if show_line_numbers then begin
          let max_line = view#buffer#end_iter#line in
          let n_chars = String.length (string_of_int (max_line + 1)) in
          gutter.chars <- n_chars;
          (max (icon_size * 2) (n_chars * approx_char_width + gutter.spacing)) + gutter_fold_size
        end else begin
          gutter.chars <- 0;
          fixed
        end;
      in
      let size = size + Oe_config.gutter_diff_size in
      view#set_border_window_size ~typ:`LEFT ~size;
      gutter.size <- size;
      gutter.fold_x <- size - gutter.fold_size; (* 2 borders on the right of fold_size *)

    method draw_old ~show_markers ~show_line_numbers ~approx_char_width = (* 0.008 *)
      (*Prf.crono Prf.prf_draw_gutter begin fun () ->*)
      try
        self#set_size_old show_markers show_line_numbers approx_char_width;
        let vrect = view#visible_rect in
        let h0 = Gdk.Rectangle.height vrect in
        let y0 = Gdk.Rectangle.y vrect in
        let start, _ = view#get_line_at_y y0 in
        let stop, _ = view#get_line_at_y (y0 + h0) in
        (** Line Numbers *)
        (* TODO Is checking `REALIZED really necessary? *)
        if (*view#misc#get_flag `REALIZED &&*) show_line_numbers then begin
          (*Prf.crono Prf.prf_line_numbers begin fun () ->*)
          Line_num_labl.reset line_num_labl;
          let iter = ref start#backward_line in
          let stop = stop#forward_line in
          let x = gutter.size - gutter.fold_size - 4 - gutter.spacing - Oe_config.gutter_diff_size in
          let y = ref 0 in
          let h = ref 0 in
          let num = ref 0 in
          while not (!iter#equal stop) do
            num := !iter#line + 1;
            let yl, hl = view#get_line_yrange !iter in
            y := yl - y0 + view#pixels_above_lines;
            h := hl;
            self#print_line_numbers ~view ~num:!num ~x ~y:!y ~width_chars:gutter.chars line_num_labl;
            iter := !iter#forward_line;
          done;
          let y = !y  + !h in
          incr num;
          self#print_line_numbers ~view ~num:!num ~x ~y ~width_chars:gutter.chars line_num_labl
          (*end()*)
        end;
        (** Markers *)
        (*Prf.crono Prf.prf_other_markers begin fun () ->*)
        let x = (gutter.size - gutter.fold_size - Oe_config.gutter_diff_size - icon_size - 3) / 2 (*1*) in
        List.iter begin fun mark ->
          match mark.icon_pixbuf with
          | Some pixbuf ->
              begin
                match Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer mark.mark with
                | Some mark_iter ->
                    let ym, h = view#get_line_yrange (new GText.iter mark_iter) in
                    let y = ym - y0 in
                    Line_num_labl.hide (y + view#pixels_above_lines) line_num_labl;
                    let y = y + (h - icon_size) / 2 in
                    let child = match mark.icon_obj with
                      | None ->
                          let ebox = GBin.event_box () in
                          ebox#misc#set_property "visible-window" (`BOOL false);
                          let icon = GMisc.image ~pixbuf () in
                          ebox#add icon#coerce;
                          Gaux.may mark.callback ~f:begin fun callback ->
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
                              view#misc#grab_focus();
                              callback mark.mark
                            end
                          end;
                          let child = ebox#coerce in
                          ignore (child#misc#connect#destroy ~callback:(fun () -> icons <- List.remove_assoc ym icons));
                          view#add_child_in_window ~child ~which_window:`LEFT ~x ~y;
                          mark.icon_obj <- Some child;
                          icons <- (ym, child) :: icons;
                          child
                      | Some child -> view#move_child ~child ~x ~y; child
                    in
                    self#gutter_icons_same_pos child x y ym;
                | _ -> ()
              end;
          | _ -> ()
        end gutter.markers;
        (* Diff margin *)
        (*end ()*)
      with ex -> Printf.eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())
    (*end ()*)

    method private gutter_icons_same_pos child x y ym =
      match List_opt.assoc ym icons with
      | Some other ->
          if child#misc#parent <> None && other#misc#parent <> None && other#misc#get_oid <> child#misc#get_oid then begin
            let offset = (gutter.size - Oe_config.gutter_diff_size - gutter.fold_size) / 4 in
            view#move_child ~child:other ~x:(x - offset) ~y;
            view#move_child ~child ~x:(x + offset) ~y
          end
      | _ -> ()

    method print_line_numbers ~view ~x ~y ~num ~width_chars lnl =
      let open Line_num_labl in
      let open Settings_t in
      let text = string_of_int num in
      let label = match lnl.free with
        | label :: tl ->
            lnl.free <- tl;
            label#set_text text;
            label
        | [] ->
            let label = GMisc.label ~xalign:1.0 ~yalign:0.5 ~text ~show:false () in
            label#misc#modify_fg [`NORMAL, gutter.fg_color];
            label#misc#modify_font_by_name Preferences.preferences#get.editor_base_font;
            view#add_child_in_window ~child:label#coerce ~which_window:`LEFT ~x:0 ~y:0;
            label
      in
      (match List_opt.assoc y lnl.locked with Some x -> x#misc#hide() | _ -> ());
      lnl.locked <- (y, label) :: lnl.locked;
      label#set_width_chars width_chars;
      label#misc#show();
      let width = max label#misc#allocation.Gtk.width lnl.max_width in
      view#move_child ~child:label#coerce ~x:(x - width) ~y;
      if width > lnl.max_width then (lnl.max_width <- width)

    method draw_border ~height =
      match view#get_window `LEFT with
      | Some window ->
          let drawable = new GDraw.drawable window in
          drawable#set_line_attributes ~style:`SOLID ();
          drawable#set_foreground (`NAME "red") (*gutter.border_color*);
          drawable#line ~x:(gutter.size - 1) ~y:0 ~x:(gutter.size - 1) ~y:height;
          if gutter.fold_size > 0 then
            drawable#line ~x:(gutter.size - 2 - gutter.fold_size) ~y:0
              ~x:(gutter.size - 2 - gutter.fold_size) ~y:height;
      | _ -> ()

    method connect = new container_signals ~update
  end

and container_signals ~update = object
  inherit GUtil.ml_signals [ update#disconnect ]
  method update = update#connect ~after
end

and update = object inherit [unit] GUtil.signal () end

