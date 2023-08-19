open Gutter

let icon_size = 15
let icon_size_3_4 = icon_size * 3 / 4

class virtual margin () =
  object (self)
    method virtual is_visible : bool
    method virtual size : int (* width of the margin in pixels *)
    method virtual draw :
      view:GText.view ->
      top:int -> left:int -> height:int ->
      start:GText.iter -> stop:GText.iter -> unit
  end

class line_numbers (view : GText.view) =
  let label_max_chars = 4 in
  object (self)
    inherit margin ()
    val labels = Line_num_labl.create()
    val mutable size = 0

    initializer
      self#resize();

    method is_visible = true
    method size = size
    method reset () = Line_num_labl.reset labels
    method private iter func = Line_num_labl.iter func labels
    method hide_label id = Line_num_labl.hide id labels

    method resize ?desc () =
      let desc =
        match desc with
        | Some d -> d
        | _ ->
            Preferences.preferences#get.Settings_t.editor_base_font
            |> GPango.font_description
      in
      let char_width = GPango.to_pixels (view#misc#pango_context#get_metrics ~desc ())#approx_digit_width in
      size <- label_max_chars * char_width;
      self#iter (fun lab -> lab#misc#modify_font desc)

    method modify_color color =
      self#iter (fun x -> x#misc#modify_fg [`NORMAL, color])

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
            view#move_child ~child:label#coerce ~x:left ~y:top;
            label
        | [] ->
            let label = GMisc.label ~xalign:1.0 ~yalign:0.5 ~text ~show:false () in
            label#misc#modify_fg [`NORMAL, `COLOR (view#misc#style#fg `NORMAL)];
            label#misc#modify_font_by_name Preferences.preferences#get.editor_base_font;
            label#set_width_chars label_max_chars;
            view#add_child_in_window ~child:label#coerce ~which_window:`LEFT ~x:left ~y:top;
            label
      in
      (match List.assoc_opt top labels.locked with Some x -> x#misc#hide() | _ -> ());
      labels.locked <- (top, label) :: labels.locked;
      label#misc#show();
      let width = max label#misc#allocation.Gtk.width labels.max_width in
      if width > labels.max_width then (labels.max_width <- width)
  end

class markers gutter margin_line_numbers =
  object (self)
    inherit margin ()
    val mutable icons = []
    method is_visible = true
    method size = 0

    method draw ~view ~top ~left ~height ~start ~stop =
      let width = margin_line_numbers#size in
      let left = left - width + width / 2 - icon_size_3_4 in
      gutter.markers
      |> List.iter begin fun mark ->
        match mark.icon_pixbuf with
        | Some pixbuf ->
            begin
              match Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer mark.mark with
              | Some mark_iter ->
                  let ym, h = view#get_line_yrange (new GText.iter mark_iter) in
                  let y = ym - top in
                  margin_line_numbers#hide_label (y + view#pixels_above_lines);
                  let y = y + (h - icon_size) / 2 in
                  let child = match mark.icon_obj with
                    | None ->
                        let ebox = GBin.event_box () in
                        ebox#misc#set_property "visible-window" (`BOOL false);
                        let icon = GMisc.image ~pixbuf () in
                        ebox#add icon#coerce;
                        Gaux.may mark.callback ~f:begin fun callback ->
                          ebox#event#connect#enter_notify ~callback:begin fun ev ->
                            let window = GdkEvent.get_window ev in
                            Gdk.Window.set_cursor window (Gdk.Cursor.create `HAND1);
                            true
                          end |> ignore;
                          ebox#event#connect#leave_notify ~callback:begin fun ev ->
                            let window = GdkEvent.get_window ev in
                            Gdk.Window.set_cursor window (Gdk.Cursor.create `ARROW);
                            true
                          end |> ignore;
                          ebox#event#connect#button_press ~callback:begin fun _ ->
                            view#misc#grab_focus();
                            callback mark.mark
                          end
                        end;
                        let child = ebox#coerce in
                        child#misc#connect#destroy ~callback:(fun () -> icons <- List.remove_assoc ym icons) |> ignore;
                        view#add_child_in_window ~child ~which_window:`LEFT ~x:left ~y;
                        mark.icon_obj <- Some child;
                        icons <- (ym, child) :: icons;
                        child
                    | Some child -> view#move_child ~child ~x:left ~y; child
                  in
                  self#icons_same_pos view child left y ym;
              | _ -> ()
            end;
        | _ -> ()
      end

    method private icons_same_pos view child x y ym =
      match List_opt.assoc ym icons with
      | Some other ->
          if child#misc#parent <> None && other#misc#parent <> None && other#misc#get_oid <> child#misc#get_oid then begin
            let offset = icon_size / 2 in
            view#move_child ~child:other ~x:(x - offset) ~y;
            view#move_child ~child ~x:(x + offset) ~y
          end
      | _ -> ()
  end

class container (view : GText.view) =
  let gutter = Gutter.create () in
  object (self)
    val update = new update
    val mutable childs : margin list = []
    val mutable width = 0
    val mutable approx_char_width = 0

    method gutter = gutter
    method approx_char_width = approx_char_width

    method add margin = childs <- childs @ [margin]

    method draw () =
      (* Check `REALIZED to avoid caching line numbers without parent. *)
      if view#misc#get_flag `REALIZED then begin
        let vrect = view#visible_rect in
        let height = Gdk.Rectangle.height vrect in
        let top = Gdk.Rectangle.y vrect in
        let start, _ = view#get_line_at_y top in
        let stop, _ = view#get_line_at_y (top + height) in
        view#set_border_window_size ~typ:`LEFT ~size:1; (* dummy initial size *)
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
      end

    method connect = new container_signals ~update
  end

and container_signals ~update = object
  inherit GUtil.ml_signals [ update#disconnect ]
  method update = update#connect ~after
end

and update = object inherit [unit] GUtil.signal () end

