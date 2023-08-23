open Margin
open Odiff
module ColorOps = Color
open Preferences

class widget view =
  let drawing_area = GMisc.drawing_area () in
  let color_add =
    let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else -0.2, -0.4 in
    `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_add) ~sat ~value)
  in
  let color_del =
    let sat, value = if Preferences.preferences#get.theme_is_dark then 0.2, 0.4 else -0.65, 1.0 in
    `NAME (ColorOps.modify (?? Oe_config.global_gutter_diff_color_del) ~sat ~value)
  in
  let color_change = `NAME (?? Oe_config.global_gutter_diff_color_change) in
  let spacing = 5 in
  let line_width = 1 in
  let filled = true in
  let size = 13 in
  let area_width = size - spacing in
  let width = area_width - line_width in
  object (self)
    inherit margin ()
    val mutable diffs : Odiff.diffs = []
    val mutable color_base = `COLOR (view#misc#style#base `NORMAL)
    val mutable start_line = 0
    val mutable stop_line = 0
    val mutable top = 0
    val mutable height = 0
    method size = size
    method is_visible = true
    method set_diffs x = diffs <- x

    initializer
      view#add_child_in_window ~child:drawing_area#coerce ~which_window:`LEFT ~x:0 ~y:0;
      Preferences.preferences#connect#changed ~callback:begin fun _ ->
        color_base <- `COLOR (view#misc#style#base `NORMAL)
      end |> ignore;
      let drawable = new GDraw.drawable drawing_area#misc#window in
      drawable#set_background color_base;
      drawable#set_line_attributes ~width:line_width ();
      drawing_area#event#connect#expose ~callback:begin fun _ ->
        drawable#set_foreground color_base;
        drawable#rectangle ~x:0 ~y:0 ~width:area_width ~height ~filled:true ();
        diffs
        |> List.iter begin function
        | Add (_, ind, _) ->
            drawable#set_foreground color_add;
            self#draw_bar drawable ind
        | Delete (_, ind, _) ->
            drawable#set_foreground color_del;
            self#draw_triangle drawable ind
        | Change (_, _, ind, _) ->
            drawable#set_foreground color_change;
            self#draw_bar drawable ind
        end;
        false
      end |> ignore

    method draw_bar drawable = function
      | One ln when start_line <= ln && ln <= stop_line ->
          let iter = view#buffer#get_iter (`LINE (ln - 1)) in
          let y, height = view#get_line_yrange iter in
          drawable#rectangle ~x:0 ~y:(y - top) ~width ~height ~filled ()
      | Many (l1, l2) when l1 <= stop_line && l2 >= start_line ->
          let iter1 = view#buffer#get_iter (`LINE (l1 - 1)) in
          let y1, _ = view#get_line_yrange iter1 in
          let iter2 = view#buffer#get_iter (`LINE (l2 - 1)) in
          let y2, height2 = view#get_line_yrange iter2 in
          drawable#rectangle ~x:0 ~y:(y1 - top) ~width ~height:(y2 + height2 - y1) ~filled ()
      | One _ | Many _ -> ()

    method draw_triangle drawable = function
      | One ln when start_line <= ln && ln <= stop_line ->
          let iter = view#buffer#get_iter (`LINE (ln - 1)) in
          let y, height = view#get_line_yrange iter in
          let y = y - top + height in
          let dy = height / 3 in
          drawable#polygon ~filled [ 0, y - dy; 0, y + dy; width, y ];
      | One _ | Many _ -> ()

    method draw ~view ~top:t ~left ~height:h ~start ~stop =
      drawing_area#set_size ~width:area_width ~height;
      start_line <- start#line + 1;
      stop_line <- stop#line + 1;
      top <- t;
      height <- h;
      view#move_child ~child:drawing_area#coerce ~x:(left + spacing) ~y:0;
  end
