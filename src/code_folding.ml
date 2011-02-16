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
open Editor_types
open Miscellanea

type hover = Out | Mark of (int * int) | Region
type tag_table = Hidden | Readonly 

let draw_focus_ribbon = false
let fold_size = 11 (*10 *)
let dx = 5 (*4*)

let split_length num =
  let rec f acc parts fact = function
    | 0 -> acc
    | 1 -> 1 :: acc
    | n when parts = 1 -> n :: acc
    | n ->
      let sect = int_of_float ((float n) *. fact) in
      let sect = if sect = 0 && parts > 1 then (n - sect) else sect in
      sect :: (f acc (parts - 1) fact (n - sect))
  in
  let parts = min 5 num in
  f [] parts 0.62 num;;

class manager ~(view : Text.view) =
  let explicit = false in
  let min_length = 3 in
  let buffer = view#buffer in
  let font = Gaux.may_map Oe_config.code_folding_font ~f:Gdk.Font.load_fontset in 
  let code_folding_scope_color = Oe_config.code_folding_scope_color in
  let code_folding_fold_line_color = `COLOR (Preferences.tag_color "lident") in 
  let set_highlight_background tag = Gtk_util.set_tag_paragraph_background tag in
object (self)
  val mutable enabled = true;
  val mutable folding_points = []
  val mutable graphics = []
  val mutable tag_highlight_applied = false
  val mutable tag_highlight_busy = false
  val mutable table_tag_hidden = []
  val mutable table_tag_readonly = []
  val mutable signal_expose = None
  val mutable changed_after_last_draw_markers = false
  val mutable tag_highlight = buffer#create_tag ~name:"tag_code_folding_focus" []

  method enabled = enabled
  method set_enabled x =
    enabled <- x;
    if enabled then begin
      view#gutter.Gutter.fold_size <- fold_size;
      Gtk_util.idle_add view#paint_gutter;
      self#scan_folding_points(); 
    end else begin
      self#expand_all();
      view#gutter.Gutter.fold_size <- 0;
      folding_points <- [];
      Gtk_util.idle_add view#paint_gutter;  
    end;

  method scan_folding_points () =
    if enabled then begin
      Gtk_util.idle_add ~prio:300 begin fun () ->
        if not explicit then begin
          (*Prf.crono Prf.prf_scan_folding_points begin fun () ->*)
            let text = buffer#get_text () in
            folding_points <- Delimiters.scan_folding_points text;
            let comments = match Comments.scan_utf8 text with Comments.Utf8 x -> x | _ -> assert false in
            let comments = List.map (fun (a, b, _, _) ->
              (a, Convert.offset_from_pos text ~pos:b)) comments
            in
            folding_points <- List.sort (fun (a, _) (b, _) -> Pervasives.compare a b) (folding_points @ comments);
            changed_after_last_draw_markers <- true;
          (*end ()*)
        end;
      end
    end

  method is_folded (i1 : GText.iter) =
    try
      let (_, (_, _, lines)) = List.find (fun (_, (_, t, lines)) -> i1#has_tag t) table_tag_hidden in
      Some lines
    with Not_found -> None

  method private draw_line y n_lines =
    match view#get_window `TEXT with
      | Some window ->
        let drawable = new GDraw.drawable window in
        let vrect = view#visible_rect in
        let y0 = Gdk.Rectangle.y vrect in
        let w0 = Gdk.Rectangle.width vrect in
        let offset = match Oe_config.dash_style_offset with Some x -> x | _ -> w0 in
        let y = y - y0 in
        begin
          match font with
            | None ->
              drawable#set_foreground code_folding_fold_line_color;
              Gdk.GC.set_fill drawable#gc `SOLID;
              Gdk.GC.set_dashes drawable#gc ~offset [2; 2];
              drawable#set_line_attributes ~style:Oe_config.dash_style ();
              drawable#line ~x:0 ~y ~x:w0 ~y;
            | Some font ->
              let text = sprintf "  %d lines  " n_lines in
              Gdk.GC.set_font drawable#gc font;
              let w = Gdk.Font.string_width font text in
              let aw = 9 in
              let margin = 40 in
              let w0 = w0 - margin - w/2 in
              drawable#set_foreground code_folding_fold_line_color;
              drawable#arc ~x:w0 ~y:(y - 6) ~width:aw ~height:12 ~start:89. ~angle:181. ();
              drawable#arc ~x:(w0 + w - aw) ~y:(y - 6) ~width:aw ~height:12 ~start:91. ~angle:(-.181.) ();
              drawable#string text ~font ~x:w0 ~y:(y + 3);
              (* Draw line *)
              drawable#set_foreground code_folding_fold_line_color;
              Gdk.GC.set_fill drawable#gc `SOLID;
              Gdk.GC.set_dashes drawable#gc ~offset [2; 2];
              drawable#set_line_attributes ~style:Oe_config.dash_style ();
              drawable#line ~x:0 ~y ~x:w0 ~y;
              drawable#line ~x:(w0 + w) ~y ~x:(w0 + w + margin) ~y;
        end
      | _ -> ()

  method private draw_markers ev =
    match view#get_window `LEFT with
      | Some window ->
        (*Prf.crono Prf.prf_draw_markers begin fun () ->*)
          let xs = view#gutter.Gutter.fold_x in
          let xm = xs + view#gutter.Gutter.fold_size / 2 in (* center of the fold part *)
          let folds = ref [] in
          let vrect = view#visible_rect in
          let y0 = Gdk.Rectangle.y vrect in
          (* Filter folding_points by visible area *)
          let h0 = Gdk.Rectangle.height vrect in
          let top, _ = view#get_line_at_y y0 in
          let bottom, _ = view#get_line_at_y (y0 + h0) in
          let top = top#offset in
          let bottom = bottom#offset in
          let exposed = List.filter begin fun (of1, of2) ->
            top <= of1 && of1 <= bottom || top <= of2 && of2 <= bottom
          end folding_points in
          (* Filter folding_points to be drawn *)
          List.iter begin fun (of1, of2) ->
            let i1 = buffer#get_iter (`OFFSET of1) in
            let i2 = buffer#get_iter (`OFFSET of2) in
            let i2 = i2#forward_line in
            if i2#line - i1#line > min_length then begin
              if (self#is_folded i1#backward_char) = None then begin
                let collapsed =
                  match self#is_folded (if i1#ends_line then i1 else i1#forward_to_line_end) with
                  | None -> false
                  | Some lines ->
                    let y, h = view#get_line_yrange i1 in
                    self#draw_line (y + h) lines;
                    true
                in
                let yb1, h1 = view#get_line_yrange i1 in
                let yb2, h2 = view#get_line_yrange i2 in
                let yv1 = yb1 - y0 in
                let yv2 = yb2 - y0 in
                let ym1 = yv1 + h1/2 - 1 in
                let ym2 = yv2 - h2 + h2/2 + 3 in
                let ys1 = yv1 in
                let ys2 = yv2 + 1 in
                let of2 = i2#offset in
                match !folds with
                  | (a, b, ms) :: tl when draw_focus_ribbon && a <= ys1 && ys2 <= b ->
                    ms := ((of1, of2, yv1, yv2, h1), (collapsed, ym1, ym2, h1, h2)) :: !ms
                  | _ ->
                    folds := (ys1, ys2, ref [(of1, of2, yv1, yv2, h1), (collapsed, ym1, ym2, h1, h2)]) :: !folds
              end
            end
          end exposed;
          (* Draw lines and markers in the same iter (to reduce flickering?) *)
          let drawable = new GDraw.drawable window in
          drawable#set_foreground view#gutter.Gutter.marker_color;
          drawable#set_line_attributes ~width:2 ~cap:`PROJECTING ~style:`SOLID ();
          List.iter begin fun (ys1, ys2, ms) ->
            (*(* Focus ribbon (disabled) *)
            if draw_focus_ribbon && (ys1 >= 0 || ys2 >= 0) && (ys1 <= h0 || ys1 <= h0) then begin
              drawable#set_foreground code_folding_scope_color;
              drawable#set_line_attributes ~width:1 ~cap:`PROJECTING ~style:`SOLID ();
              drawable#rectangle ~x:xs ~y:ys1 ~width ~height:(ys2 - ys1 - 1) ~filled:true ();
            end;*)
            (* Markers *)
            let xm = xm - 1 in
            List.iter begin fun ( _, (collapsed, ym1, ym2, h1, h2)) ->
              if collapsed then begin
                let xm = xm - 2 in
                let ym1 = ym1 + 1 in
                drawable#polygon ~filled:true [(xm, ym1 - 5); (xm, ym1 + 5); (xm + 5, ym1)];
              end else begin
                let ym1 = ym1 + 1 in
                drawable#polygon ~filled:true [(xm - 4, ym1); (xm + dx, ym1); (xm, ym1 + dx)];
                let xm = xm - 2 in
                let ym2 = ym2 - 3 in
                drawable#segments [((xm, (ym2 - 3)), (xm, ym2)); ((xm, ym2), ((xm + dx), ym2))];
              end;
            end !ms;
          end !folds;
          graphics <- !folds;
          changed_after_last_draw_markers <- false;
        (*end ()*)
      | _ -> ()

  method private is_hover x y =
    try
      let xs = view#gutter.Gutter.fold_x in
      let ms =
        try
          let _, _, ms = List.find begin fun (y1, y2, _) ->
            x >= xs && x <= view#gutter.Gutter.size && y1 <= y && y <= y2
          end graphics in
          ms
        with Not_found -> (raise Exit)
      in
      let (yb1, yb2, _, _, _), _ = List.find begin fun ((_, _, yv1, yv2, h1), _) ->
        yv1 <= y && y <= yv1 + h1
      end !ms in
      Mark (yb1, yb2)
    with Not_found -> Out | Exit -> Region

  method private range ~fold start stop =
    let iter = ref start in
    let stop = stop#set_line_index 0 in
    while not (!iter#equal stop) do
      begin
        try
          let _, (_, tag, _) = List.find (fun (_, (_, t, _)) -> !iter#has_tag t) table_tag_hidden in
          tag#set_properties [`INVISIBLE fold; `INVISIBLE_SET fold]
        with Not_found -> ()
      end;
      iter := !iter#forward_line
    done

  method private remove_tag_from_table which_table iter =
    let tag_table = match which_table with Hidden -> table_tag_hidden | Readonly -> table_tag_readonly in
    let res, tab =
      List.fold_left begin fun (res, acc) ((_, (_, t, _)) as x) ->
        if res = None && iter#has_tag t then (Some x, acc) else (res, x :: acc)
      end (None, []) tag_table
    in
    (match which_table with Hidden -> table_tag_hidden <- tab | Readonly -> table_tag_readonly <- tab);
    res

  method private fold_offsets o1 o2 =
    let start_folding_point = buffer#get_iter (`OFFSET o1) in
    let start_of_line_folding_point = start_folding_point#set_line_index 0 in
    let start =
      if start_folding_point#char <> 13 && start_folding_point#char <> 10
      then start_folding_point#forward_to_line_end else start_folding_point
    in
    let where = start#set_line_index 0 in
    let stop = (buffer#get_iter (`OFFSET o2))#set_line_index 0 in
    if stop#line - start#line >= min_length then begin
      begin
        match self#remove_tag_from_table Hidden start with
          | None ->
            view#matching_delim_remove_tag ();
            view#buffer#place_cursor ~where;
            view#scroll_lazy where;
            Gaux.may view#signal_expose ~f:(fun id -> view#misc#handler_block id);
            view#matching_delim_remove_tag ();
            self#range ~fold:false start stop;
            let tag_readonly = buffer#create_tag [`EDITABLE false] in
            let tag_hidden = buffer#create_tag [`INVISIBLE_SET true; `INVISIBLE true(*; `EDITABLE false*)] in
            let m1 = `MARK (buffer#create_mark start) in
            let m2 = `MARK (buffer#create_mark stop) in
            (*Gtk_util.set_tag_paragraph_background tag_readonly Oe_config.code_folding_highlight_color;*)
            buffer#apply_tag tag_readonly ~start:start_of_line_folding_point ~stop;
            buffer#apply_tag tag_hidden ~start ~stop;
            table_tag_hidden <- (m1, (m2, tag_hidden, stop#line - start#line - 1)) :: table_tag_hidden;
            table_tag_readonly <- (m1, (m2, tag_readonly, 1)) :: table_tag_readonly;
            Gtk_util.idle_add view#paint_gutter;
            Gaux.may view#signal_expose ~f:(fun id -> view#misc#handler_unblock id);
          | Some (m1, (m2, tag, _)) ->
            self#range ~fold:true start stop;
            let iter = ref start in
            let n = stop#line - start#line in
            let sections = List.rev (split_length n) in
            Gaux.may view#signal_expose ~f:(fun id -> view#misc#handler_block id);
            Gaux.may signal_expose ~f:(fun id -> view#misc#handler_block id);
            ignore (Gtk_util.idle_add_gen begin let i = ref (List.length sections - 1) in fun () ->
              try
                if !i > 0 && !iter#compare stop < 0 then begin
                  let lines = max 3 (List.nth sections !i) in
                  iter := !iter#forward_lines lines;
                  buffer#remove_tag tag ~start ~stop:!iter;
                  decr i;
                  true
                end else begin
                  buffer#remove_tag tag ~start ~stop;
                  begin
                    match self#remove_tag_from_table Readonly start with
                      | Some (_, (_, tag_ro, _)) ->
                        buffer#remove_tag tag_ro ~start:start_of_line_folding_point ~stop;
                      | _ -> assert false
                  end;
                  Gtk_util.idle_add view#paint_gutter;
                  Gaux.may view#signal_expose ~f:(fun id -> view#misc#handler_unblock id);
                  Gaux.may signal_expose ~f:(fun id -> view#misc#handler_unblock id);
                  false
                end;
              with ex -> (eprintf "%s\n%!" (Printexc.to_string ex); false)
            end);
            buffer#delete_mark m1;
            buffer#delete_mark m2;
      end;
    end

  method private fold (window : Gdk.window) x y =
    match self#is_hover x y with
      | Mark (o1, o2) ->
        self#fold_offsets o1 o2;
        true
      | Region -> false
      | _ -> true

  method toggle_current_fold () =
    let iter = buffer#get_iter `INSERT in
    let o = iter#forward_to_line_end#offset in
    let points = List.filter (fun (a, b) -> a <= o && o <= b) folding_points in
    let points = List.sort (fun (a1, _) (a2, _) -> Pervasives.compare a2 a1) points in
    try
      let o1, o2 = List.hd points in
      self#fold_offsets o1 o2
    with Failure "hd" -> ()

  method expand_current () = self#expand (buffer#get_iter `INSERT)#forward_to_line_end

  method expand (iter : GText.iter) =
    let tags = List.filter (fun (_, (_, tag, _)) -> iter#has_tag tag) table_tag_hidden in
    let tags = List.sort begin fun (ma, _) (mb, _) ->
      let ia = buffer#get_iter_at_mark ma in
      let ib = buffer#get_iter_at_mark mb in
      ia#compare ib
    end tags in
    List.iter begin fun (m1, (m2, tag, _)) ->
      let start = buffer#get_iter_at_mark m1 in
      let stop = buffer#get_iter_at_mark m2 in
      buffer#remove_tag tag ~start ~stop;
      buffer#delete_mark m1;
      buffer#delete_mark m2;
      self#range ~fold:true start stop;
      table_tag_hidden <- List.remove_assq m1 table_tag_hidden;
    end tags;
    if List.length tags > 0 then (Gtk_util.idle_add view#paint_gutter);

  method expand_all () =
    List.iter begin fun (m1, (m2, tag, _)) ->
      let start = buffer#get_iter_at_mark m1 in
      let stop = buffer#get_iter_at_mark m2 in
      buffer#remove_tag tag ~start ~stop;
      buffer#delete_mark m1;
      buffer#delete_mark m2;
    end table_tag_hidden;
    table_tag_hidden <- [];
    Gtk_util.idle_add view#paint_gutter

  method private highlight x y =
    match view#get_window `LEFT with
      | Some window ->
        begin
          match self#is_hover x y with
            | Mark (o1, o2) ->
              Gdk.Window.set_cursor window (!Gtk_util.cursor `HAND1);
              if not tag_highlight_busy && not tag_highlight_applied then begin
                set_highlight_background tag_highlight Oe_config.code_folding_highlight_color;
                let start = buffer#get_iter (`OFFSET o1) in
                let stop = (buffer#get_iter (`OFFSET o2))#set_line_index 0 in
                buffer#apply_tag tag_highlight ~start ~stop;
                tag_highlight_applied <- true;
              end;
            | _ ->
              Gdk.Window.set_cursor window (!Gtk_util.cursor `ARROW);
              if tag_highlight_applied && not tag_highlight_busy then begin
                tag_highlight_busy <- true;
                let grad = Oe_config.code_folding_hightlight_gradient in
                if grad = [] then begin
                  buffer#remove_tag tag_highlight ~start:buffer#start_iter ~stop:buffer#end_iter;
                  tag_highlight_applied <- false;
                  tag_highlight_busy <- false;
                end else begin
                  ignore (GMain.Timeout.add ~ms:20 ~callback:begin let i = ref 0 in fun () ->
                    tag_highlight_busy <- true;
                    if !i = (List.length grad - 1) then begin
                      buffer#remove_tag tag_highlight ~start:buffer#start_iter ~stop:buffer#end_iter;
                      tag_highlight_applied <- false;
                      tag_highlight_busy <- false;
                      false
                    end else begin
                      tag_highlight_applied <- true;
                      let color = List.nth grad !i in
                      set_highlight_background tag_highlight color;
                      incr i;
                      true
                    end
                  end);
                end
              end;
        end
      | _ -> ()

  initializer
    signal_expose <- Some (view#event#connect#after#expose ~callback:begin fun ev ->
      if enabled then (self#draw_markers ev);
      false
    end);
    view#event#connect#button_press ~callback:begin fun ev ->
      if enabled then begin
        let window = GdkEvent.get_window ev in
        match view#get_window `LEFT with
          | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
            let x = GdkEvent.Button.x ev in
            let y = GdkEvent.Button.y ev in
            Gaux.may signal_expose ~f:(fun id -> view#misc#handler_block id);
            let handled = self#fold window (int_of_float x) (int_of_float y) in
            Gaux.may signal_expose ~f:(fun id -> view#misc#handler_unblock id);
            handled
          | _ -> false
      end else false
    end;
    view#misc#connect#query_tooltip ~callback:begin fun ~x ~y ~kbd _ ->
      if enabled then (self#highlight x y);
      false
    end;
    ()
end

















