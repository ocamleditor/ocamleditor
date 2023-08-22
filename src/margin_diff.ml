open Margin
open Odiff
module ColorOps = Color
open Preferences

class widget view =
  let drawing_area = GMisc.drawing_area () in
  let color_add = `NAME (ColorOps.add_value (?? Oe_config.global_gutter_diff_color_add) 0.25) in
  let color_del = `NAME (ColorOps.add_value (?? Oe_config.global_gutter_diff_color_del) 0.25) in
  let color_change = `NAME (?? Oe_config.global_gutter_diff_color_change) in
  let sig_id = ref None in
  let spacing = 5 in
  let line_width = 1 in
  let filled = true in
  object (self)
    inherit margin ()
    val mutable diffs : Odiff.diffs = []
    val mutable color_base = `COLOR (view#misc#style#base `NORMAL)
    method size = 13
    method is_visible = true
    method set_diffs x = diffs <- x

    initializer
      view#add_child_in_window ~child:drawing_area#coerce ~which_window:`LEFT ~x:0 ~y:0;
      Preferences.preferences#connect#changed ~callback:begin fun _ ->
        color_base <- `COLOR (view#misc#style#base `NORMAL)
      end |> ignore

    method draw ~view ~top ~left ~height ~start ~stop =
      let area_width = self#size - spacing in
      drawing_area#set_size ~width:area_width ~height;
      let width = area_width - line_width in
      let drawable = new GDraw.drawable drawing_area#misc#window in
      drawable#set_background color_base;
      drawable#set_line_attributes ~width:line_width ();
      let start_line = start#line + 1 in
      let stop_line = stop#line + 1 in
      let draw_bar ind color =
        drawable#set_foreground color;
        match ind with
        | One ln when start_line <= ln && ln <= stop_line ->
            let iter = view#buffer#get_iter (`LINE (ln - 1)) in
            let y, height = view#get_line_yrange iter in
            drawable#rectangle ~x:0 ~y:(y - top) ~width ~height ~filled ()
        | Many (l1, l2) when start_line <= l1 || l2 <= stop_line ->
            let iter1 = view#buffer#get_iter (`LINE (l1 - 1)) in
            let y1, _ = view#get_line_yrange iter1 in
            let iter2 = view#buffer#get_iter (`LINE (l2 - 1)) in
            let y2, height2 = view#get_line_yrange iter2 in
            drawable#rectangle ~x:0 ~y:(y1 - top) ~width ~height:(y2 + height2 - y1) ~filled ()
        | One _ | Many _ -> ()
      in
      let draw_triangle ind color =
        drawable#set_foreground color;
        match ind with
        | One ln when start_line <= ln && ln <= stop_line ->
            Printf.printf "draw_triangle %d\n%!" ln;
            let iter = view#buffer#get_iter (`LINE (ln - 1)) in
            let y, height = view#get_line_yrange iter in
            let y = y - top in
            drawable#polygon ~filled [ 0, y; 0, y - height; width, y - height / 2 ];
        | One _ | Many _ ->
            Printf.printf "draw_triangle\n%!" ;
            ()
      in
      view#move_child ~child:drawing_area#coerce ~x:(left + spacing) ~y:0;
      !sig_id |> Option.iter (GtkSignal.disconnect drawing_area#as_widget);
      sig_id :=
        Some (drawing_area#event#connect#expose ~callback:begin fun _ ->
            drawable#set_foreground color_base;
            drawable#rectangle ~x:0 ~y:0 ~width:area_width ~height ~filled:true ();
            diffs
            |> List.iter begin fun diff ->
              match diff with
              | Add (_, ind, a) -> draw_bar ind color_add
              | Delete (_, ind, a) -> draw_triangle ind color_del
              | Change (_, a, ind, b) -> draw_bar ind color_change
            end;
            false
          end)
  end
