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
open Miscellanea
open Text_util

type tag_table_kind = Hidden
type tag_table_entry = {
  mark_start_fold : GText.mark;
  mark_stop_fold  : GText.mark;
  tag             : GText.tag;
}

type fold_iters = {
  fit_start_marker : GText.iter;
  fit_start_fold : GText.iter;
  fit_stop : GText.iter;
}

(* See src/code-folding.svg *)
type region = {
  i1 : GText.iter; (* offset 0 of the line containing the token starting the folding region *)
  i2 : GText.iter; (* offset 0 of the line containing the token that ends the folding region *)
  ic : GText.iter; (* offset 0 of the line at the beginning of the folding region *)
  has_end_token : bool;
  fp1 : int;
  fp2 : int option;
  y0 : int; (* pixels in buffer coordinates of the visible rect. *)
  mutable y1 : int; (* top of i1 line relative to y0 *)
  mutable y1m : int; (* middle of the i1 line relative to y0 *)
  mutable y2 : int; (* top of i2 line relative to y0 *)
  h0 : int; (* height of the visible rect. *)
  (*hh : int;
    hr : int;*)
  mutable h1 : int; (* height of the i1 line *)
  mutable h2 : int; (* height of the i2 line *)
  mutable is_collapsed : bool;
}

let string_of_region r =
  sprintf "{ fp:%d,%s i:%d,%d,%d y:%d,%d,%d h:%d,%d,%d has_end_token:%b is_collapsed:%b }"
    r.fp1 (match r.fp2 with Some x -> string_of_int x | _ -> "") 
    r.i1#offset r.ic#offset r.i2#offset
    r.y1 r.y2 r.y1m
    r.h0 r.h1 r.h2
    r.has_end_token r.is_collapsed

type hover = Out | Mark of region | Region

let fold_size = 11 (*10 *)
let dx = 5 (*4*)
let dx1 = dx - 1
let dx12 = (dx - 1) / 2
let dxdx12 = dx - dx12

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
  (*let explicit = false in*)
  let min_lines = 2 in
  let buffer = view#buffer in
  let set_highlight_background tag = Gmisclib.Util.set_tag_paragraph_background tag in
  object (self)
    val mutable enabled = true;
    val mutable folding_points = []
    val mutable comments = []
    val mutable regions = []
    val mutable markers = []
    val mutable tag_highlight_applied = None
    val mutable tag_highlight_busy = false
    val mutable table_tag_hidden : tag_table_entry list = []
    val mutable signal_expose = None
    val mutable tag_highlight = buffer#create_tag
        ~name:(sprintf "tag_code_folding_focus_%f" (Unix.gettimeofday())) []
    val toggled = new toggled ()
    val mutable fold_line_color = `NAME "#000000"
    val mutable light_marker_color = `NAME "#000000"

    method enabled = enabled
    method set_enabled x =
      enabled <- x;
      if enabled then begin
        view#gutter.Gutter.fold_size <- fold_size;
        Gmisclib.Idle.add view#draw_gutter;
        self#scan_folding_points();
      end else begin
        self#expand_all();
        view#gutter.Gutter.fold_size <- 0;
        folding_points <- [];
        Gmisclib.Idle.add view#draw_gutter;
      end;

    method fold_line_color = fold_line_color
    method set_fold_line_color x =
      fold_line_color <- x

    method scan_folding_points () =
      if enabled then begin
        Gmisclib.Idle.add begin fun () ->
          crono ~label:"scan_folding_points" begin fun () -> 
            let vrect = view#visible_rect in
            let h0 = Gdk.Rectangle.height vrect in
            let y0 = Gdk.Rectangle.y vrect in
            let start, _ = view#get_line_at_y y0 in
            let stop, _ = view#get_line_at_y (y0 + h0) in
            (* Adjust start and stop positions *)
            let start = start#backward_line#set_line_index 0 in
            let stop = stop#forward_line#set_line_index 0 in
            let start_offset = start#offset in
            let stop_offset = stop#offset in
            (* Find all comments in the buffer and filter by visible rect. *)
            comments <-
              buffer#get_text ()
              |> Glib.Convert.convert_with_fallback ~fallback:"" 
                ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset
              |> Comments.scan_locale
              |> List.filter 
                (fun (a, _, _) -> start_offset <= a && a <= stop_offset);
            let text = buffer#get_text ~start ~stop:buffer#end_iter () in
            let fp, pos = crono ~label:"Delimiters" Delimiters.scan_folding_points text in
            let offset = start_offset + pos in
            let fp = 
              fp 
              (* Find folding points in the visible text *)
              |> List.filter_map begin fun (a, has_end_token) ->
                let pos = offset + a in
                if start_offset <= pos && pos <= stop_offset then
                  Some (pos, None, has_end_token)
                else None
              end 
              (* Exclude folding points inside comments *)
              |> List.filter begin function
              | (a, Some b, _) -> comments |> List.for_all (fun (bc, ec, _) -> not (a >= bc && b <= ec))
              | (a, None, _) -> comments |> List.for_all (fun (bc, ec, _) -> not (a >= bc && a <= ec))
              end
            in
            (* Join folding points to comments *)
            let cmts = List.map (fun (a, b, _) -> (a, Some b, true)) comments in
            let fp = List.sort (fun (a, _, _) (b, _, _) -> Stdlib.compare a b) (fp @ cmts) in
            folding_points <- fp;
          end ()
        end
      end

    method is_folded (i1 : GText.iter) =
      List.exists (fun {tag=t; _} -> i1#has_tag t) table_tag_hidden

    method private is_hover x y =
      try
        let xs = view#gutter.Gutter.fold_x in
        let region =
          try
            regions 
            |> List.find begin fun region ->
              xs <= x && x <= view#gutter.Gutter.size && 
              region.y1 <= y && y <= region.y2
            end 
          with Not_found -> (raise Exit)
        in
        if region.y1 <= y && y <= region.y1 + region.h1
        then Mark region
        else Out
      with Exit -> Region

    method private draw_fold iter =
      match view#get_window `TEXT with
      | Some window ->
          let y, h = view#get_line_yrange iter in
          let drawable = new GDraw.drawable window in
          let vrect = view#visible_rect in
          let width = 2 in
          let y0 = Gdk.Rectangle.y vrect in
          let w0 = Gdk.Rectangle.width vrect in
          let offset = match Oe_config.dash_style_offset with Some x -> x | _ -> w0 in
          let y = y + h - y0 + width / 2 in
          drawable#set_foreground fold_line_color;
          Gdk.GC.set_fill drawable#gc `SOLID;
          Gdk.GC.set_dashes drawable#gc ~offset [2; 2];
          drawable#set_line_attributes ~width ~style:Oe_config.dash_style ();
          drawable#line ~x:0 ~y ~x:w0 ~y;
      | _ -> ()

    method private add_region_info info =
      let yb1, h1 = view#get_line_yrange info.i1 in
      let yb2, h2 = view#get_line_yrange info.i2 in
      info.y1 <- yb1 - info.y0;
      info.y2 <- yb2 - info.y0 + 1;
      info.y1m <- info.y1 + h1/2 - 1;
      info.h1 <- h1;
      info.h2 <- h2;
      info.is_collapsed <- self#is_folded info.ic;
      info

    method private draw_markers () =
      match view#get_window `LEFT with
      | Some window ->
          let xs = view#gutter.Gutter.fold_x in
          let xm = xs + view#gutter.Gutter.fold_size / 2 in (* center of the fold part *)
          let vrect = view#visible_rect in
          let y0 = Gdk.Rectangle.y vrect in
          let h0 = Gdk.Rectangle.height vrect in
          (* Filter folding_points by visible area *)
          regions <- [];
          let add_region fp1 fp2 i1 i2 has_end_token =
            if not (self#is_folded i1) then begin
              let ic = i1#forward_line#set_line_index 0 in
              let region = 
                { i1; i2; ic; fp1; fp2; y0; y1 = -1; y1m = -1; y2 = -1; h0; h1 = -1; h2 = -1;
                  is_collapsed = false; has_end_token
                } |> self#add_region_info
              in
              regions <- region :: regions;
            end
          in
          folding_points 
          |> List.iter begin function
          | (fp1, ((Some of2) as fp2), has_end_token) ->
              let i1 = (buffer#get_iter (`OFFSET fp1))#set_line_index 0 in
              let i2 = (buffer#get_iter (`OFFSET of2))#set_line_index 0 in 
              if i2#line - i1#line > min_length then 
                add_region fp1 fp2 i1 i2 has_end_token
          | (fp1, None, has_end_token) ->
              let i1 = (buffer#get_iter (`OFFSET fp1))#set_line_index 0 in
              add_region fp1 None i1 buffer#end_iter has_end_token;
          end;
          (*  *)

          (*  *)
          let drawable = new GDraw.drawable window in
          drawable#set_foreground view#gutter.Gutter.marker_color;
          drawable#set_line_attributes ~width:2 ~cap:`PROJECTING ~style:`SOLID ();
          regions 
          |> List.iter begin fun region ->
            (* Draw fold *)
            if region.is_collapsed then self#draw_fold region.i1;
            (* Markers *)
            let xm = xm - 3 in
            let ym1 = region.y1m - dx in
            let ya = ym1 + 2*dx in
            let square = [(xm - dx, ym1); (xm + dx, ym1); (xm + dx, ya); (xm - dx, ya)] in
            if region.is_collapsed then begin
              drawable#set_foreground view#gutter.Gutter.marker_bg_color;
              drawable#polygon ~filled:true square;
              drawable#set_foreground view#gutter.Gutter.marker_color;
              drawable#polygon ~filled:false square;
              drawable#segments [(xm, ym1 + dx12 + 1), (xm, ym1 + dx1*2 - 1); (xm - dxdx12 + 1, ym1 + dx), (xm + dxdx12 - 1, ym1 + dx)];
            end else begin
              drawable#set_foreground view#gutter.Gutter.bg_color;
              drawable#polygon ~filled:true square;
              begin
                match region.fp2 with
                | None ->
                    drawable#set_foreground light_marker_color;
                    drawable#polygon ~filled:false square;
                | _ ->
                    drawable#set_foreground view#gutter.Gutter.marker_color;
                    drawable#polygon ~filled:false square;
              end;
              drawable#segments [(xm - dxdx12 + 1, ym1 + dx), (xm + dxdx12 - 1, ym1 + dx)];
            end;
          end;
      | _ -> ()

    method private update_tag ~fold start stop =
      let iter = ref start in
      let stop = stop#set_line_index 0 in
      while not (!iter#equal stop) do
        begin
          match List_opt.find (fun {tag=t; _} -> !iter#has_tag t) table_tag_hidden with
          | Some entry -> entry.tag#set_properties [`INVISIBLE fold; `INVISIBLE_SET fold]
          | _ -> ()
        end;
        iter := !iter#forward_line
      done;

    method private fold_between start stop =
        match self#remove_tag_from_table Hidden start with
        | None ->
            view#matching_delim_remove_tag ();
            let ins = buffer#get_iter `INSERT in
            let is_in_range = ins#in_range ~start ~stop in
            Gaux.may view#signal_expose ~f:(fun id -> view#misc#handler_block id);
            view#matching_delim_remove_tag ();
            self#update_tag ~fold:false start stop;
            let tag_hidden = buffer#create_tag [`INVISIBLE_SET true; `INVISIBLE true(*; `EDITABLE false*)] in
            let m1 = `MARK (buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Code_folding.fold_offset1")*) start) in
            let m2 = `MARK (buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Code_folding.fold_offset2")*) stop) in
            (*Gmisclib.Util.set_tag_paragraph_background tag_readonly "yellow" (*Oe_config.code_folding_highlight_color*);*)
            buffer#apply_tag tag_hidden ~start ~stop;
            table_tag_hidden <- {mark_start_fold=m1; mark_stop_fold=m2; tag=tag_hidden} :: table_tag_hidden;
            self#scan_folding_points();
            self#highlight_remove ();
            Gmisclib.Idle.add view#draw_gutter;
            if is_in_range then
              Gmisclib.Idle.add begin fun () ->
                let where = start#backward_line in
                view#buffer#place_cursor ~where;
                view#scroll_lazy where;
              end;
            Gaux.may view#signal_expose ~f:(fun id -> view#misc#handler_unblock id);
            toggled#call (true, start, stop);
        | Some {mark_start_fold=m1; mark_stop_fold=m2; tag=tag; _} ->
            self#update_tag ~fold:true start stop;
            let iter = ref start in
            let n = stop#line - start#line in
            let sections = List.rev (split_length n) in
            Gaux.may view#signal_expose ~f:(fun id -> view#misc#handler_block id);
            Gaux.may signal_expose ~f:(fun id -> view#misc#handler_block id);
            Gmisclib.Idle.add_gen begin 
              let i = ref (List.length sections - 1) in 
              fun () ->
                try
                  if !i > 0 && !iter#compare stop < 0 then begin
                    let lines = max 3 (List.nth sections !i) in
                    iter := !iter#forward_lines lines;
                    buffer#remove_tag tag ~start ~stop:!iter;
                    decr i;
                    true
                  end else begin
                    buffer#remove_tag tag ~start ~stop;
                    Gmisclib.Idle.add ~prio:100 view#draw_gutter;
                    Gaux.may view#signal_expose ~f:(fun id -> view#misc#handler_unblock id);
                    Gaux.may signal_expose ~f:(fun id -> view#misc#handler_unblock id);
                    false
                  end;
                with ex -> (eprintf "%s\n%!" (Printexc.to_string ex); false)
            end |> ignore;
            Gmisclib.Idle.add ~prio:300 begin fun () ->
              buffer#delete_mark m1;
              buffer#delete_mark m2;
            end;
            toggled#call (false, start, stop);

    method private fold_region region =
      let o2 =
        match region.fp2 with
        | None -> self#find_matching_delimiter region.fp1
        | _ -> region.i2#forward_line
      in
      self#fold_between region.ic o2;

    method private fold_at x y =
      try
        begin
          match self#is_hover x y with
          | Mark region ->
              self#fold_region region;
              true
          | Region -> false
          | Out -> true
        end;
      with Exit -> true

    method private search_code_backward (iter : GText.iter) =
      let iter = iter#backward_find_char not_blank in
      match Comments.enclosing (Comments.Locale comments) iter#offset with
      | Some (start, stop) -> self#search_code_backward (buffer#get_iter (`OFFSET start))
      | _ -> iter

    method private find_matching_delimiter o1 =
      let start = buffer#get_iter (`OFFSET o1) in
      let text = buffer#get_text ~start ~stop:buffer#end_iter () in
      let stop, is_end_token = Delimiters.find_folding_point_end text in
      let stop = start#offset + stop in
      (*Printf.printf "find_matching_delimiter: %d->%d\n%!" o1 stop;*)
      let iter = buffer#get_iter (`OFFSET stop) in
        if is_end_token then begin
          if iter#ends_line then iter#forward_char
          else iter#forward_to_line_end#forward_char
        end else begin
        (self#search_code_backward iter)#forward_to_line_end#forward_char
      end

    method private remove_tag_from_table which_table iter =
      let tag_table = match which_table with Hidden -> table_tag_hidden in
      let res, tab =
        List.fold_left begin fun (res, acc) ({tag=t; _} as entry) ->
          if res = None && iter#has_tag t then (Some entry, acc) else (res, entry :: acc)
        end (None, []) tag_table
      in
      (match which_table with Hidden -> table_tag_hidden <- tab);
      res

    method toggle_current_fold () =
      (* TODO: consider the case when the beginning of the enclosing region is 
         not within the visible rect.  *)
      let iter = buffer#get_iter `INSERT in
      let ins = iter#forward_to_line_end#offset in
      let enclosing_regions =
        regions 
        |> List.filter (fun r -> r.i1#offset <= ins && ins <= r.i2#offset)
        |> List.sort (fun a b -> Stdlib.compare b.i1#offset a.i1#offset)
      in
      match enclosing_regions with
      | r :: _ -> self#fold_region r
      | [] -> ()

    method expand_current () = self#expand (buffer#get_iter `INSERT)#forward_to_line_end

    method expand (iter : GText.iter) =
      let tags_owned_by_iter ~which_table =
        let tags = List.filter (fun {tag=tag; _} -> iter#has_tag tag) which_table in
        List.sort begin fun {mark_start_fold=ma; _} {mark_start_fold=mb; _} ->
          let ia = buffer#get_iter_at_mark ma in
          let ib = buffer#get_iter_at_mark mb in
          ia#compare ib
        end tags
      in
      (*  *)
      let tags = tags_owned_by_iter ~which_table:table_tag_hidden in
      List.iter begin fun {mark_start_fold=m1; mark_stop_fold=m2; tag=tag; _} ->
        let start = buffer#get_iter_at_mark m1 in
        let stop = buffer#get_iter_at_mark m2 in
        buffer#remove_tag tag ~start ~stop;
        self#update_tag ~fold:true start stop; (* re-collapse folds inside the expanded fold, if they were folded *)
        buffer#delete_mark m1;
        buffer#delete_mark m2;
        table_tag_hidden <- List.filter (fun x -> x.mark_start_fold != m1) table_tag_hidden;
      end tags;
      if List.length tags > 0 then (Gmisclib.Idle.add view#draw_gutter);

    method expand_all () =
      List.iter begin fun {mark_start_fold=m1; mark_stop_fold=m2; tag=tag; _} ->
        let start = buffer#get_iter_at_mark m1 in
        let stop = buffer#get_iter_at_mark m2 in
        buffer#remove_tag tag ~start ~stop;
      end table_tag_hidden;
      table_tag_hidden <- [];
      Gmisclib.Idle.add view#draw_gutter

    method private highlight x y =
      match view#get_window `LEFT with
      | Some window ->
          self#draw_markers();
          begin
            match self#is_hover x y with
            | Mark rm as mark ->
                Printf.printf "highlight 1\n%!" ;
                if not tag_highlight_busy && tag_highlight_applied = None then begin
                  if not (self#is_folded rm.ic) then begin
                    set_highlight_background tag_highlight Oe_config.code_folding_highlight_color;
                    let start = rm.i1 in
                    let stop =
                      match rm.fp2 with
                      | None -> self#find_matching_delimiter rm.fp1
                      | Some fp2 when rm.has_end_token ->
                          let iter = buffer#get_iter (`OFFSET fp2) in
                          if iter#ends_line then (iter#set_line_index 0)#forward_to_line_end
                          else iter#forward_to_line_end
                      | Some fp2 ->
                          (buffer#get_iter (`OFFSET fp2))#set_line_index 0
                    in
                    buffer#apply_tag tag_highlight ~start ~stop;
                    tag_highlight_applied <- Some mark;
                  end
                end else begin
                  match [@warning "-4"] tag_highlight_applied with
                  | Some (Mark r) when 
                      r.fp1 <> rm.fp1 && r.fp2 <> rm.fp2 -> 
                      self#highlight_remove ();
                  | _ -> ();
                end;
            | Region
            | Out -> 
                Printf.printf "highlight 2\n%!" ;
                self#highlight_remove ()
          end
      | _ -> ()

    method private highlight_remove () =
      if tag_highlight_applied <> None && not tag_highlight_busy then begin
        tag_highlight_busy <- true;
        let grad = Oe_config.code_folding_hightlight_gradient in
        if grad = [] then begin
          buffer#remove_tag tag_highlight ~start:buffer#start_iter ~stop:buffer#end_iter;
          tag_highlight_applied <- None;
          tag_highlight_busy <- false;
        end else begin
          ignore (GMain.Timeout.add ~ms:20 ~callback:begin let i = ref 0 in fun () ->
              tag_highlight_busy <- true;
              if !i = (List.length grad - 1) then begin
                buffer#remove_tag tag_highlight ~start:buffer#start_iter ~stop:buffer#end_iter;
                tag_highlight_applied <- None;
                tag_highlight_busy <- false;
                false
              end else begin
                (*tag_highlight_applied <- true;*)
                let color = List.nth grad !i in
                set_highlight_background tag_highlight color;
                incr i;
                true
              end
            end);
        end
      end;

    method private init () =
      signal_expose <- Some (view#event#connect#after#expose ~callback:begin fun _ ->
          if enabled then (self#draw_markers ());
          false
        end);
      ignore (view#connect#set_scroll_adjustments ~callback:begin fun _ vertical ->
          match vertical with
          | Some vertical ->
              ignore (vertical#connect#value_changed ~callback:begin fun () ->
                  Gaux.may signal_expose ~f:(fun id -> view#misc#handler_block id);
                end);
              ignore (vertical#connect#after#value_changed ~callback:(fun () ->
                  Gmisclib.Idle.add ~prio:300 self#scan_folding_points;
                  Gmisclib.Idle.add ~prio:100 (fun () ->
                      Gaux.may signal_expose ~f:(fun id -> view#misc#handler_unblock id))));
          | _ -> ()
        end);
      view#event#connect#after#button_release ~callback:begin fun ev ->
        if enabled then begin
          GMain.Idle.add begin fun () ->
            let window = GdkEvent.get_window ev in
            match view#get_window `LEFT with
            | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
                let x = GdkEvent.Button.x ev in
                let y = GdkEvent.Button.y ev in
                Gaux.may signal_expose ~f:(fun id -> view#misc#handler_block id);
                self#fold_at (int_of_float x) (int_of_float y) |> ignore;
                Gaux.may signal_expose ~f:(fun id -> view#misc#handler_unblock id);
                false
            | _ -> false
          end |> ignore;
          false
        end else false
      end |> ignore;
      view#misc#connect#query_tooltip ~callback:begin fun ~x ~y ~kbd:_ _ ->
        if enabled then (self#highlight x y);
        false
      end |> ignore;

      view#misc#connect#after#realize ~callback:begin fun () ->
        light_marker_color <-
          (let r, g, b = Color.rgb_of_gdk (GDraw.color view#gutter.Gutter.marker_color) in
           Color.hsv_of_name r g b
             (fun h s v ->
                `NAME (Color.name_of_hsv h s (v +. 0.35))));
      end |> ignore;

    initializer self#init()

    method connect = new code_folding_list_signals ~toggled

  end

and code_folding_list_signals ~toggled = object
  inherit GUtil.ml_signals [toggled#disconnect]
  method toggled = toggled#connect ~after
end

and toggled () = object inherit [bool * GText.iter * GText.iter] GUtil.signal () end















