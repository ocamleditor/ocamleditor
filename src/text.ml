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

open Str
open GdkKeysyms
open Printf
open Miscellanea


let string_width s =
  let width = ref 0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\t' then width := (!width / 8 + 1) * 8
    else incr width
  done;
  !width

let blanks = [13;10;32;9]
let not_blank c = not (List.mem c blanks)

let whitespace_middot      = "\xC2\xB7"
let whitespace_tab         = "\xC2\xBB"
let whitespace_crlf        = "\xC2\xA4\xC2\xB6"
let whitespace_lf          = "\xC2\xB6"
let create_middot_string = Miscellanea.Memo.fast
  ~f:(fun x -> String.concat "" (Miscellanea.Xlist.list_full whitespace_middot x))

(** Buffer *)
class buffer =
  let word_bound = Miscellanea.regexp "\\b" in
  let blanks = Miscellanea.regexp "[ \t]*" in
  let mark_count = ref 0 in
  let new_mark_name () = incr mark_count; "indent_mark_" ^ (string_of_int !mark_count) in
fun ?buffer ?file () ->
  let buffer = match buffer with None -> GText.buffer () | Some b -> b in
object (self)
  inherit GText.buffer buffer#as_buffer

  val undo = new Undo.manager ~buffer
  val mutable tab_width = 2
  val mutable tab_spaces = true

  method undo = undo

  method tab_width = tab_width
  method set_tab_width x = tab_width <- x

  method tab_spaces = tab_spaces
  method set_tab_spaces x = tab_spaces <- x

  method file : File.file option = file

  method select_all () = self#select_range self#start_iter self#end_iter

  method select_marks ~start ~stop =
    self#select_range (self#get_iter_at_mark stop) (self#get_iter_at_mark start);

  method selection_text () =
    let start, stop = self#selection_bounds in
    self#get_text ~start ~stop ()

  method get_line_at_iter (it : GText.iter) =
    self#get_text ~start:(it#set_line_index 0)
      ~stop:(it#forward_line#backward_char) ()

  method indent ?(dir=(`FORWARD : [`FORWARD | `BACKWARD])) ?start ?stop () =
    let indentation_length = tab_width in
    let use_spaces = tab_spaces in
    let indentation length = String.make length (if use_spaces then ' ' else '\t') in
    let i1, i2 = match start, stop with
      | Some m1, Some m2 ->
        let i12 = self#get_iter_at_mark (`NAME m1), self#get_iter_at_mark (`NAME m2) in
        self#delete_mark (`NAME m1);
        self#delete_mark (`NAME m2);
        self#select_range (fst i12) (snd i12);
        i12
      | _ -> self#selection_bounds in
    let comp = i1#compare i2 in
    if comp <> 0 || dir = `BACKWARD then begin
      undo#begin_block ~name:"indent";
      let l1, l2 = i1#line, i2#line in
      let l3 = if (self#get_text ~start:i2#backward_char ~stop:i2 ()) = "\n" then l2 - 1 else l2 in
      begin
        let start ~line = self#get_iter_at_char 0 ~line in
        match dir with
          | `FORWARD ->
            let indent ~iter = self#insert (indentation indentation_length) ~iter in
            for line = l1 to l3 do indent ~iter:(start ~line) done;
            let pos = if comp > 0 then `INSERT else `SEL_BOUND in
              self#move_mark pos ~where:(start ~line:(self#get_iter_at_mark pos)#line);
          | `BACKWARD ->
              let len = indentation_length - 1 in
              for line = l1 to l3 do
                let start = start ~line in
                let get, del = self#get_text ~start, self#delete ~start in
                for j = 0 to len do
                  let stop = start#forward_char in
                  let c = get ~stop () in
                  if c = " " || c = "\t" then (ignore (del ~stop (*()*)));
                done;
              done;
      end;
      let i1, i2 = self#selection_bounds in
      let start, stop = new_mark_name(), new_mark_name() in
      self#create_mark ~left_gravity:false ~name:start i1;
      self#create_mark ~left_gravity:false ~name:stop i2;
      undo#end_block();
    end else begin
      let ins = self#get_iter `INSERT in
      let l, c = ins#line, ins#line_offset
      and line = self#get_text ~start:(ins#set_line_index 0) ~stop:ins#forward_to_line_end () in
      ignore (Str.string_match blanks line 0);
      let len = Str.match_end () in
      if len < c then self#insert "\t" ~iter:ins else
        let width = string_width (Str.matched_string line) in
        self#select_range (ins#set_line_index len) (ins#set_line_index len);
        let indent =
          if l <= 1 then indentation_length else
          let previous =
            self#get_text ~start:(ins#backward_line#set_line_index 0)
              ~stop:(ins#backward_line#forward_to_line_end) () in
          ignore (Str.string_match blanks previous 0);
          let previous = Str.matched_string previous in
          let width_previous = string_width previous in
          if  width_previous <= width then indentation_length else width_previous - width
        in
        self#insert_interactive (indentation indent) ~iter:ins;
        GtkSignal.stop_emit();
    end

  method select_word ?(iter=(self#get_iter `INSERT)) ?(pat=word_bound) ?(select=true) ?(limit=[]) () =
    let line = (self#get_line_at_iter iter)^"\n" in
    let pos = iter#line_index in
    let rec find_bounds pos =
      let start = try search_backward pat line pos with Not_found -> 0 in
      if start = 0 || pat = word_bound then start, search_forward pat line (start + 1)
      else if start = pos then begin
        if List.mem line.[pos] limit then pos + 1, pos + 1
        else find_bounds (pos - 1)
      end else start + 1, search_forward pat line (start + 1)
    in try
      let start, stop = find_bounds pos in
      let start = iter#set_line_index start in
      let stop = iter#set_line_index stop in
      if select then self#select_range start stop;
      (start, stop)
    with Not_found | Invalid_argument _ -> (iter#set_line_index pos, iter#set_line_index pos)

  method toggle_case () =
    let start, stop = self#get_iter `SEL_BOUND, self#get_iter `INSERT in
    let text = self#get_text ~start ~stop () in
    let uppercase = Glib.Utf8.uppercase text in
    let text = if text = uppercase then Glib.Utf8.lowercase text else uppercase in
    self#delete ~start ~stop;
    self#insert text;
    let stop = self#get_iter `INSERT in
    self#select_range (stop#backward_chars (Glib.Utf8.length text)) stop

  method as_tbuffer = (self :> GText.buffer)
end


(** View *)
and view ?buffer () =
  let buffer = match buffer with None -> new buffer () | Some b -> b in
  let view = GText.view ~buffer:buffer#as_tbuffer () in
  let create_highlight_current_line_tag () =
    buffer#create_tag ~name:(sprintf "highlight_current_line_tag_%f" (Unix.gettimeofday())) []
  in
object (self)
  inherit GText.view view#as_view as super

  val mutable tbuffer = buffer
  val mutable hadjustment : GData.adjustment option = None
  val mutable vadjustment : GData.adjustment option = None
  val mutable highlight_current_line = None
  val mutable prev_insert_line = None
  val mutable highlight_current_line_tag = create_highlight_current_line_tag ()
  val mutable base_color : GDraw.color = `WHITE
  val mutable current_matching_tag_bounds = []
  val mutable approx_char_width = 0
  val mutable smart_home = true;
  val mutable smart_end = true;
  val mutable visible_right_margin : (int * GDraw.color) option = None;
  val mutable current_line_border_color :GDraw.color = `NAME "#d0d0d0"
  val visible_right_margin_dash = [1; 3]
  val line_num_labl = Line_num_labl.create()
  val mutable line_numbers_font = view#misc#pango_context#font_name
  val mutable show_line_numbers = true
  val mutable show_indent_lines = true
  val visible_height = new GUtil.variable 0
  val mutable signal_expose = None
  val gutter = Gutter.create()
  val mutable gutter_icons = []
  val hyperlink = new Hyperlink.hyperlink ~view ()
  val mutable realized = false
  val mutable show_whitespace_chars = false
  val mutable word_wrap = false

  method show_whitespace_chars = show_whitespace_chars
  method set_show_whitespace_chars x = show_whitespace_chars <- x

  method word_wrap = word_wrap
  method set_word_wrap x =
    self#set_wrap_mode (if x then `WORD else `NONE);
    word_wrap <- x

  method realized = realized
  method hyperlink = hyperlink

  method signal_expose = signal_expose
  method gutter = gutter

  method as_tview = (self :> GText.view)
  method tbuffer = buffer

  method hadjustment = hadjustment
  method vadjustment = vadjustment

  method set_buffer buf =
    let tbuf = new buffer ~buffer:buf () in
    tbuffer <- tbuf;
    super#set_buffer buf

  method smart_home = smart_home
  method set_smart_home x = smart_home <- x

  method smart_end = smart_end
  method set_smart_end x = smart_end <- x

  method set_base_color x = base_color <- x
  method base_color = base_color

  method modify_font fontname =
    self#misc#modify_font_by_name fontname;
    approx_char_width <- GPango.to_pixels (self#misc#pango_context#get_metrics())#approx_char_width;
    Gtk_util.idle_add self#paint_gutter

  method visible_right_margin = visible_right_margin
  method set_visible_right_margin x = visible_right_margin <- x

  method set_line_numbers_font fontname =
    Line_num_labl.iter (fun x -> x#misc#modify_font_by_name fontname) line_num_labl;
    line_numbers_font <- fontname;

  method set_show_line_numbers x =
    show_line_numbers <- x;
    Line_num_labl.reset line_num_labl;
    Gtk_util.idle_add self#paint_gutter

  method show_line_numbers = show_line_numbers

  method set_show_indent_lines x = show_indent_lines <- x
  method show_indent_lines = show_indent_lines

  method line_numbers_font = line_numbers_font

  method current_line_border_color = current_line_border_color
  method set_current_line_border_color x = current_line_border_color <- x

  method create_highlight_current_line_tag () =
    let tag_table = new GText.tag_table buffer#tag_table in
    tag_table#remove highlight_current_line_tag#as_tag;
    highlight_current_line_tag <- create_highlight_current_line_tag();
    self#set_highlight_current_line highlight_current_line

  method line_num_labl = line_num_labl

  method set_highlight_current_line x =
    highlight_current_line <- x;
    prev_insert_line <- None;
    match x with None -> () | Some color ->
      current_line_border_color <- Color.set_value 0.82 (`NAME color);
      Gtk_util.set_tag_paragraph_background highlight_current_line_tag color;

  method highlight_current_line = highlight_current_line

  method highlight_current_line_tag = highlight_current_line_tag

  method scroll_lazy iter =
    Gtk_util.idle_add ~prio:300 (fun () -> ignore (self#scroll_to_iter ~use_align:(self#scroll_to_iter iter) ~xalign:1.0 ~yalign:0.38 iter));

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
    self#place_cursor_onscreen();

  method line_char =
    let iter = self#buffer#get_iter `INSERT in
    iter#line + 1, iter#line_offset, iter#line_index, iter#offset

  method goto () = try begin
    let w = GWindow.window
      ~title: "Go to..."
      ~resizable:false
      ~type_hint:`DIALOG
      ~allow_grow:false
      ~allow_shrink:false
      ~position:`CENTER_ALWAYS
      ~modal:true () in
      let vbox = GPack.vbox ~border_width:8 ~spacing:8 ~packing:w#add () in
      let eb = GPack.hbox ~spacing:3 ~packing:vbox#add () in
      let _ = GMisc.label ~text:"Line: " ~xalign:0.0 ~width:70 ~packing:(eb#pack ~expand:false) () in
      let line = GEdit.entry ~packing:eb#add () in
      let eb = GPack.hbox ~spacing:3 ~packing:vbox#add () in
      let _ = GMisc.label ~text:"Character: " ~xalign:0.0 ~width:70 ~packing:(eb#pack ~expand:false) () in
      let char = GEdit.entry ~packing:eb#add () in
      let _ = GMisc.separator `HORIZONTAL ~packing:vbox#add () in
      let bbox = GPack.button_box `HORIZONTAL ~layout:`END ~spacing:8 ~packing:vbox#add () in
      let button_ok = GButton.button ~stock:`OK ~packing:bbox#add () in
      let button_cancel = GButton.button ~stock:`CANCEL ~packing:bbox#add () in
      let callback () =
        try
          let buf = self#buffer in
          let line = (int_of_string line#text) - 1 in
          let char = try int_of_string char#text with _ -> 0 in
          let where = buf#get_iter (`LINE line) in
          let char = max 0 (min (where#chars_in_line - 1) char) in
          let where = buf#get_iter (`LINECHAR (line, char)) in
          self#buffer#place_cursor ~where;
          self#scroll_lazy where;
          w#destroy();
        with e -> Dialog.display_exn self e
      in
      button_ok#connect#clicked ~callback:(fun () -> callback(); w#destroy());
      button_cancel#connect#clicked ~callback:w#destroy;
      w#event#connect#key_press ~callback:begin fun ev ->
        let key = GdkEvent.Key.keyval ev in
        if key = _Return then begin
          callback();
          true
        end else if key = _Escape then begin
          w#destroy();
          true
        end else false;
      end;
      line#misc#grab_focus();
      Gaux.may ~f:(fun x -> w#set_transient_for x#as_window) (GWindow.toplevel self);
      w#present()
  end with e -> Dialog.display_exn self e;

  method private matching_delim_apply_tag (text : string) (offset : int) = (*(None : (int * int * int * int) option)*)
    match Delimiters.find_match ~utf8:false text offset with
      | None -> None
      | Some (lstart, lstop, rstart, rstop) as delim when lstop <> rstart ->
        let start = self#buffer#get_iter (`OFFSET lstart) in
        let stop = self#buffer#get_iter (`OFFSET lstop) in
        current_matching_tag_bounds <-
          ((self#buffer#create_mark ~name:"delim_left_start" start),
          (self#buffer#create_mark ~name:"delim_left_stop" stop)) :: current_matching_tag_bounds;
        self#buffer#apply_tag_by_name "tag_matching_delim" ~start ~stop;
        let start = self#buffer#get_iter (`OFFSET rstart) in
        let stop = self#buffer#get_iter (`OFFSET rstop) in
        current_matching_tag_bounds <-
          ((self#buffer#create_mark ~name:"delim_right_start" start),
          (self#buffer#create_mark ~name:"delim_right_stop" stop)) :: current_matching_tag_bounds;
        self#buffer#apply_tag_by_name "tag_matching_delim" ~start ~stop;
        delim
      | Some (_, lstop, rstart, _) (*as delim*) when lstop = rstart ->
        self#innermost_enclosing_delim text lstop;
        None
      | _ -> None

  method matching_delim_remove_tag () =
    List.iter begin function (start, stop) ->
      let mstart = `MARK start in
      let mstop = `MARK stop in
      self#buffer#remove_tag_by_name "tag_matching_delim"
        ~start:(self#buffer#get_iter_at_mark mstart) ~stop:(self#buffer#get_iter_at_mark mstop);
      self#buffer#delete_mark mstart;
      self#buffer#delete_mark mstop;
    end current_matching_tag_bounds;
    current_matching_tag_bounds <- [];

  method matching_delim () =
    self#matching_delim_remove_tag();
    let pos = self#buffer#get_iter `INSERT in
    let start = pos#backward_chars 5 in
    let stop = pos#forward_chars 5 in
    let text55 = Glib.Convert.convert_with_fallback ~fallback:"?"
      ~from_codeset:"utf8" ~to_codeset:Oe_config.ocaml_codeset (self#buffer#get_text ~start ~stop ()) in
    let text = Glib.Convert.convert_with_fallback ~fallback:"?"
      ~from_codeset:"utf8" ~to_codeset:Oe_config.ocaml_codeset (self#buffer#get_text ()) in
    if Delimiters.is_delimiter ~utf8:false text55 (pos#offset - start#offset) then begin
      self#matching_delim_apply_tag text pos#offset
    end else begin
      try
        self#innermost_enclosing_delim text pos#offset;
        None
      with ex -> eprintf "%s\n%!" (Printexc.to_string ex); None
    end

  method private innermost_enclosing_delim (text : string) (offset : int) = (*(None : (int * int * int * int) option)*)
    match Delimiters.find_innermost_enclosing_delim ~utf8:false text offset with
      | ((start, stop) :: _) (*as stack*)  ->
        if stop = offset
        then (self#innermost_enclosing_delim text start)
        else (self#matching_delim_apply_tag text start);
      | _ -> None

  method matching_delim_goto ?(select=false) ?(strict=true) () =
    match self#buffer#selection_bounds with
      | b1, b2 when (not (b1#equal b2)) ->
        let b1 = if List.mem b1#char blanks then
          b1#forward_find_char not_blank else b1 in
        let b2 = if List.mem b2#char blanks then
          (b2#backward_find_char not_blank)#forward_to_line_end else b2 in
        self#buffer#select_range b1 b2
      | _ -> begin
        match current_matching_tag_bounds with
          | [(rstart, rstop); (lstart, lstop)] ->
            let ins = self#buffer#get_iter `INSERT in
            let right_start = Gtk_util.get_iter_mark self#buffer rstart in
            let left_stop = Gtk_util.get_iter_mark self#buffer lstop in
            if select then begin
              let start =
                if strict then begin
                  let start = left_stop#forward_find_char not_blank in
                  let start = start#set_line_index 0 in
                  if start#compare left_stop <= 0 then left_stop else start
                end else (Gtk_util.get_iter_mark self#buffer lstart)
              in
              let stop =
                if strict then begin
                  let stop = right_start#backward_find_char not_blank in
                  let stop = stop#forward_line in
                  if stop#compare right_start <= 0 then stop else right_start
                end else (Gtk_util.get_iter_mark self#buffer rstop)
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
      let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
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
    let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
    let win = (match self#get_window `WIDGET
      with None -> failwith "Text.text#get_location_top_right `WIDGET = None" | Some w -> w) in
    let px, py = Gdk.Window.get_pointer_location win in
    (pX - px + self#misc#allocation.Gtk.width),
    (pY - py)

  method paint_current_line_background ins =
    Gaux.may prev_insert_line ~f:begin fun cur ->
      let cur = self#buffer#get_iter (`LINE cur) in
      self#buffer#remove_tag highlight_current_line_tag
        ~start:(cur#set_line_offset 0) ~stop:cur#forward_to_line_end;
    end;
    (*if not self#buffer#has_selection then begin*)
      self#buffer#apply_tag highlight_current_line_tag
        ~start:(ins#set_line_offset 0) ~stop:ins#forward_char;
      prev_insert_line <- Some ins#line;
    (*end*)

  method private set_gutter_size () =
    let gutter_fold_size = gutter.Gutter.fold_size + 4 in (* 4 = borders around fold_size *)
    let fixed = Gutter.icon_size + gutter_fold_size in
    let size =
      if show_line_numbers then begin
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
    try
      let other = List.assoc ym gutter_icons in
      if child#misc#parent <> None && other#misc#parent <> None && other#misc#get_oid <> child#misc#get_oid then begin
        let offset = (gutter.Gutter.size - gutter.Gutter.fold_size) / 4 in
        self#move_child ~child:other ~x:(x - offset) ~y;
        self#move_child ~child ~x:(x + offset) ~y
      end
    with Not_found -> ()

  method paint_gutter () = (* 0.008 *)
    (*Prf.crono Prf.prf_paint_gutter begin fun () ->*)
      try
        self#set_gutter_size();
        let vrect = self#visible_rect in
        let h0 = Gdk.Rectangle.height vrect in
        let y0 = Gdk.Rectangle.y vrect in
        let start, _ = self#get_line_at_y y0 in
        let stop, _ = self#get_line_at_y (y0 + h0) in
        (** Line Numbers *)
        if realized && show_line_numbers then begin
          (*Prf.prf_line_numbers begin fun () ->*)
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
              y := yl - y0 + self#pixels_above_lines;
              h := hl;
              Line_num_labl.print ~view:self ~num:!num ~x ~y:!y ~width_chars:gutter.Gutter.chars line_num_labl;
              iter := !iter#forward_line;
            done;
            let y = !y  + !h in
            incr num;
            Line_num_labl.print ~view:self ~num:!num ~x ~y ~width_chars:gutter.Gutter.chars line_num_labl
          (*end()*)
        end;
        (** Markers *)
        (*Prf.prf_other_markers begin fun () ->*)
          let x = (gutter.Gutter.size - gutter.Gutter.fold_size - 3 - Gutter.icon_size) / 2 (*1*) in
          List.iter begin fun mark ->
            let ym, h = self#get_line_yrange (Gtk_util.get_iter_mark buffer mark.Gutter.mark) in
            let y = ym - y0 in
            Line_num_labl.hide (y + self#pixels_above_lines) line_num_labl;
            let y = y + (h - Gutter.icon_size) / 2 in
            let child = match mark.Gutter.icon_obj with
              | None ->
                let ebox = GBin.event_box () in
                Gtk_util.set_ebox_invisible ebox;
                let icon = GMisc.image ~pixbuf:mark.Gutter.icon_pixbuf () in
                ebox#add icon#coerce;
                Gaux.may mark.Gutter.callback ~f:begin fun callback ->
                  ebox#event#connect#enter_notify ~callback:begin fun ev ->
                    let window = GdkEvent.get_window ev in
                    Gdk.Window.set_cursor window (!Gtk_util.cursor `HAND1);
                    true
                  end;
                  ebox#event#connect#leave_notify ~callback:begin fun ev ->
                    let window = GdkEvent.get_window ev in
                    Gdk.Window.set_cursor window (!Gtk_util.cursor `ARROW);
                    true
                  end;
                  ebox#event#connect#button_press ~callback:(fun _ -> self#misc#grab_focus(); callback mark.Gutter.mark)
                end;
                let child = ebox#coerce in
                child#misc#connect#destroy ~callback:(fun () -> gutter_icons <- List.remove_assoc ym gutter_icons);
                self#add_child_in_window ~child ~which_window:`LEFT ~x ~y;
                mark.Gutter.icon_obj <- Some child;
                gutter_icons <- (ym, child) :: gutter_icons;
                child
              | Some child -> self#move_child ~child ~x ~y; child
            in
            self#gutter_icons_same_pos child x y ym;
          end gutter.Gutter.markers;
        (*end ()*)
      with ex -> eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())
    (*end ()*)

  method private scan_indent (start : GText.iter) (stop : GText.iter) =
    let data = ref [] in
    let iter = ref (start#set_line_index 0) in
    let stop = stop#set_line_index 0 in
    while !iter#compare stop < 0 do
      if List.mem !iter#char blanks then (iter := !iter#forward_find_char not_blank);
      let indent = !iter#line_index / buffer#tab_width in
      data := (!iter#line, indent) :: !data;
      iter := !iter#forward_line;
    done;
    !data

  method paint_indent_lines (drawable : GDraw.drawable) start stop y0 h0 =
    let indents = self#scan_indent start stop in
    let count = ref buffer#line_count in
    let prev = ref 0 in
    let lines = ref [] in
    let add_segment ll x y1 y2 =
      try
        let segs = List.assoc x !ll in
        begin
          match !segs with
            | (y3, y4) :: tl when y2 = y3 ->
              segs := (y1, y4) :: tl;
            | _ -> segs := (y1, y2) :: !segs;
        end
      with Not_found -> (ll := (x, ref [y1, y2]) :: !ll)
    in
    let hline = ref 0 in
    let draw line indent =
      let iter = buffer#get_iter (`LINE line) in
      for i = 1 to indent - 1 do
        let y1, h1 = view#get_line_yrange iter in
        let y1 = y1 - y0 in
        let y2 = y1 + h1 in
        hline := max h1 !hline;
        let x = approx_char_width * (i * buffer#tab_width) -
            (match hadjustment with Some adj -> int_of_float adj#value | _ -> 0) in
        add_segment lines x y1 y2;
      done;
    in
    List.iter begin fun (line, indent) ->
      if !count > line then begin
        while !count > line do
          draw !count !prev;
          decr count;
        done
      end;
      draw line indent;
      prev := indent;
      decr count;
    end indents;
    (*hline := !hline * 2;*)
    (* Draw lines *)
    let lines1, lines2 = List.partition (fun (x, ll) -> x mod (2 * buffer#tab_width * approx_char_width) = 0) !lines in
    drawable#set_foreground Oe_config.indent_lines_solid_color;
    drawable#set_line_attributes ~width:1 ~style:`SOLID ();
    List.iter begin fun (x, xlines) ->
      List.iter begin fun (y1, y2) ->
        if y2 - y1 > !hline then (drawable#line ~x ~y:y1 ~x ~y:y2)
      end !xlines
    end lines2;
    Gdk.GC.set_dashes drawable#gc ~offset:1 [1; 2];
    List.iter begin fun (x, xlines) ->
      List.iter begin fun (y1, y2) ->
        if y2 - y1 > !hline then begin
          drawable#set_foreground base_color;
          drawable#set_line_attributes ~width:1 ~style:`SOLID ();
          drawable#line ~x ~y:y1 ~x ~y:y2;
          drawable#set_foreground Oe_config.indent_lines_dashed_color;
          drawable#set_line_attributes ~width:1 ~style:`ON_OFF_DASH ();
          drawable#line ~x ~y:y1 ~x ~y:y2
        end
      end !xlines
    end lines1;

  method private expose ev =
    match self#get_window `TEXT with
      | Some window ->
        let vrect = self#visible_rect in
        let h0 = Gdk.Rectangle.height vrect in
        let w0 = Gdk.Rectangle.width vrect in
        let y0 = Gdk.Rectangle.y vrect in
        let start, _ = self#get_line_at_y y0 in
        let stop, _ = self#get_line_at_y (y0 + h0) in
        visible_height#set h0;
        let adjust = Oe_config.current_line_border_adjust in
        let drawable = new GDraw.drawable window in
        (* Indentation guidelines *)
        if show_indent_lines && not self#show_whitespace_chars then (self#paint_indent_lines drawable) start stop y0 h0;
        (*  *)
        if Oe_config.ocamldoc_paragraph_bgcolor_enabled then
          (self#draw_paragraph_border drawable start stop y0 h0 w0);
        (* Current line border *)
        begin
          match highlight_current_line with
            | Some color
              when Oe_config.current_line_border_enabled && self#misc#get_flag `HAS_FOCUS (*&& not self#buffer#has_selection*) ->
                let iter = buffer#get_iter `INSERT in
                let y, h = view#get_line_yrange iter in
                let y = y - y0 in
                drawable#set_line_attributes ~width:1 ~style:`ON_OFF_DASH ();
                drawable#set_foreground current_line_border_color;
                Gdk.GC.set_dashes drawable#gc ~offset:1 [1; 2];
                drawable#rectangle ~x:self#left_margin ~y
                  ~width:(w0 - adjust - self#left_margin)
                  ~height:(h - adjust) ~filled:false ();
            | _ -> ()
        end;
        (* Gutter border *)
        begin
          match self#get_window `LEFT with
            | Some window ->
              let drawable = new GDraw.drawable window in
              drawable#set_line_attributes ~style:`SOLID ();
              drawable#set_foreground gutter.Gutter.border_color;
              drawable#line ~x:(gutter.Gutter.size - 1) ~y:0 ~x:(gutter.Gutter.size - 1) ~y:h0;
              if gutter.Gutter.fold_size > 0 then
                drawable#line ~x:(gutter.Gutter.size - 2 - gutter.Gutter.fold_size) ~y:0 ~x:(gutter.Gutter.size - 2 - gutter.Gutter.fold_size) ~y:h0;
            | _ -> ()
        end;
        (* Right margin line *)
        begin
          match visible_right_margin with
            | Some (column, color) ->
              let x = approx_char_width * column -
                (match hadjustment with Some adj -> int_of_float adj#value | _ -> 0) in
              drawable#set_line_attributes ~style:`SOLID ();
              drawable#set_foreground color;
              drawable#line ~x ~y:0 ~x ~y:h0;
            | _ -> ()
        end;
        (* Border around matching delimiters *)
        begin
          match current_matching_tag_bounds with
            | (lstart, lstop) :: (rstart, rstop) :: [] ->
              drawable#set_foreground Oe_config.matching_delim_border_color;
              drawable#set_line_attributes ~width:1 ~style:`SOLID ();
              let hadjust = match hadjustment with Some adj -> int_of_float adj#value | _ -> 0 in
              let draw start stop =
                let start = Gtk_util.get_iter_mark buffer start in
                let stop = Gtk_util.get_iter_mark buffer stop in
                let yl1, hl1 = view#get_line_yrange start in
                let yl1 = yl1 - y0 in
                let x_chars, lines_displayed = self#count_displayed_line (stop#set_line_index 0) start in
                let x = approx_char_width * x_chars - hadjust in
                let width_chars = stop#line_index - start#line_index in
                let width = approx_char_width * width_chars in
                let pango = self#misc#pango_context in
                let metrics = pango#get_metrics() in
                let height = (metrics#ascent + metrics#descent) / Pango.scale -  1 in
                let y =
                  if lines_displayed > 0
                  then yl1 + ((lines_displayed - 1) * (hl1 / lines_displayed))
                  else yl1
                in
                drawable#rectangle ~x ~y ~width ~height ();
              in
              draw lstart lstop;
              draw rstart rstop;
            | _ -> ()
        end;
        (* Dot leaders *)
        if Oe_config.dot_leaders_enabled && not self#show_whitespace_chars then ((self#draw_dot_leaders y0 drawable) ev);
        (* Whitespace characters *)
        if self#show_whitespace_chars then ((*Prf.crono Prf.prf_draw_white_spces*) (self#draw_whitespace_chars y0 drawable) ev);
        false
      | _ -> false

  method private count_displayed_line start stop =
    let iter = ref start in
    let x_chars = ref 0 in
    let lines_displayed = ref 1 in
    while not (!iter#equal stop) do
      if !iter#char = 9 then begin
        x_chars := ((!x_chars / 8) * 8 + 8);
      end else (incr x_chars);
      iter := !iter#forward_char;
      if self#starts_display_line !iter then (x_chars := 0; incr lines_displayed)
    done;
    !x_chars, !lines_displayed

  method private draw_whitespace_chars y0 drawable ev =
    let expose_area = GdkEvent.Expose.area ev in
    let ya          = y0 + Gdk.Rectangle.y expose_area in
    let top, _      = self#get_line_at_y ya in
    let bottom, _   = self#get_line_at_y (ya + (Gdk.Rectangle.height expose_area)) in
    let iter        = ref top in
    let pango       = self#misc#pango_context in
    let layout      = pango#create_layout in
    let x_adj       = match hadjustment with Some adj -> int_of_float adj#value | _ -> 0 in
    let draw iter text =
      let rect = self#get_iter_location iter in
      let x = Gdk.Rectangle.x rect - x_adj in
      let y = Gdk.Rectangle.y rect - y0 in
      Pango.Layout.set_text layout text;
      drawable#put_layout ~x ~y ~fore:self#base_color layout;
      drawable#put_layout ~x ~y ~fore:Oe_config.indent_lines_dashed_color layout;
    in
    while !iter#compare bottom < 0 do
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

  method private draw_dot_leaders y0 drawable ev =
    let expose_area = GdkEvent.Expose.area ev in
    let ya = y0 + Gdk.Rectangle.y expose_area in
    let top, _ = self#get_line_at_y ya in
    let bottom, _ = self#get_line_at_y (ya + (Gdk.Rectangle.height expose_area)) in
    Gdk.GC.set_fill drawable#gc `SOLID;
    drawable#set_line_attributes ~style:Oe_config.dash_style ();
    drawable#set_foreground (`COLOR (Preferences.tag_color "lident"));
    Alignment.iter ~start:top ~stop:bottom begin fun _ start stop _ ->
      if stop#line_index - start#line_index > 2 then begin
        let start = start#forward_char in
        let x_adj = match hadjustment with Some adj -> int_of_float adj#value | _ -> 0 in
        let x1 = approx_char_width * start#line_index - x_adj in
        let x2 = approx_char_width * stop#line_index - x_adj in
        let y, h = self#get_line_yrange start in
        let y = y - y0 + h - 3 (*(min 3 (h / 5))*) in
        Gdk.GC.set_dashes drawable#gc ~offset:(x2 (*- x1*)) [1; approx_char_width - 1];  (*6*)
        drawable#line ~x:x2 ~y ~x:x1 ~y;
      end
    end

  method private draw_paragraph_border drawable start stop y0 h0 w0 =
    let table = new GText.tag_table self#buffer#tag_table in
    match table#lookup "ocamldoc-paragraph" with
      | Some tag ->
        begin
          let tag = new GText.tag tag in
          let start = ref (start#set_line_index 0) in
          let stop = stop#forward_line#set_line_index 0 in
          match !Preferences.preferences.Preferences.pref_ocamldoc_paragraph_bgcolor_1 with
            | Some color ->
              let color = `NAME color in
              drawable#set_foreground (Color.set_value 0.95 color);
              drawable#set_line_attributes ~width:1 ~style:`SOLID ();
              let hadjust = match hadjustment with Some adj -> int_of_float adj#value | _ -> 0 in
              while !start#forward_line#compare stop <= 0 && not (!start#equal self#buffer#end_iter) do
                if !start#has_tag tag then begin
                  let x = 0 - hadjust + self#left_margin in
                  let y1, _ = view#get_line_yrange !start in
                  let y = y1 - y0 in
                  let width = w0 + hadjust - self#left_margin - 2 in
                  let stop_tag = !start#forward_to_tag_toggle (Some tag) in
                  let y2, h = view#get_line_yrange stop_tag in
                  let height = y2 - y0 + h - y in
                  drawable#rectangle ~x ~y ~width ~height ();
                  start := stop_tag#set_line_index 0;
                end;
                start := !start#forward_line;
              done
            | _ -> ()
        end
      | _ -> ()

  initializer
    (** Realize *)
    ignore (self#misc#connect#after#realize ~callback:begin fun () ->
      (* Gutter colors must be set when the text view is realized *)
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
      (* Change the bg color of the gutter on screen *)
      view#misc#modify_bg [`NORMAL, self#gutter.Gutter.bg_color];
      realized <- true;
    end);
    (** Margin and line spacings *)
    self#set_left_margin 1;
    self#set_pixels_above_lines Oe_config.pixels_above_lines;
    self#set_pixels_below_lines Oe_config.pixels_below_lines;
    (** Select lines from gutter *)
    self#event#connect#after#button_press ~callback:begin fun ev ->
      let window = GdkEvent.get_window ev in
      match self#get_window `LEFT with
        | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
          let y0 = Gdk.Rectangle.y self#visible_rect in
          let y = GdkEvent.Button.y ev in
          let start = fst (self#get_line_at_y ((int_of_float y) + y0)) in
          gutter.Gutter.start_selection <- Some start;
          buffer#select_range start start#forward_line;
          true
        | _ -> false
    end;
    self#event#connect#after#motion_notify ~callback:begin fun ev ->
      let window = GdkEvent.get_window ev in
      match self#get_window `LEFT with
        | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
          let y0 = Gdk.Rectangle.y self#visible_rect in
          let y = GdkEvent.Motion.y ev in
          Gaux.may gutter.Gutter.start_selection ~f:begin fun start ->
            let stop = fst (self#get_line_at_y ((int_of_float y) + y0)) in
            buffer#select_range start stop#forward_line;
            view#scroll_to_iter stop#forward_line;
          end;
          true
        | _ -> false
    end;
    self#event#connect#after#button_release ~callback:begin fun ev ->
      let window = GdkEvent.get_window ev in
      match self#get_window `LEFT with
        | Some w when (Gobject.get_oid w) = (Gobject.get_oid window) ->
          gutter.Gutter.start_selection <- None;
          true
        | _ -> false
    end;
    (** Expose *)
    signal_expose <- Some (self#event#connect#after#expose
      ~callback:(fun ev -> try self#expose ev with ex -> eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace()); false));
    (** Highlight current line *)
    ignore (self#buffer#connect#after#mark_set ~callback:begin fun iter mark ->
      match highlight_current_line with
        | Some _ ->
          let ins = self#buffer#get_iter `INSERT in
          if ins#equal iter then (self#paint_current_line_background ins);
        | _ -> ()
    end);
    ignore (self#buffer#connect#after#delete_range ~callback:begin fun ~start ~stop ->
      (*self#paint_current_line_background ();*)
      Gtk_util.idle_add ~prio:300 self#paint_gutter;
    end);
    ignore (self#buffer#connect#after#insert_text ~callback:begin fun _ _ ->
      (*self#paint_current_line_background ();
      self#matching_delim_remove_tag();*)
      Gtk_util.idle_add ~prio:300 self#paint_gutter;
    end);
    (** Refresh gutter and right margin line when scrolling *)
    self#connect#set_scroll_adjustments ~callback:begin fun h v ->
      hadjustment <- h;
      vadjustment <- v;
      match h, v with
        | (Some h), (Some v) ->
          (* Redraw the entire window on horizontal scroll to refresh right margin *)
          ignore (h#connect#after#value_changed
            ~callback:(fun () -> GtkBase.Widget.queue_draw self#as_widget));
          (* Update gutter on vertical scroll changed *)
          ignore (v#connect#after#value_changed ~callback:begin fun () ->
            Gtk_util.idle_add self#paint_gutter;
          end);
        | _ -> ()
    end;
    ignore (visible_height#connect#changed ~callback:(fun _ -> self#paint_gutter()));
    (** Smart keys *)
    ignore (self#event#connect#key_press ~callback:begin fun ev ->
      let state = GdkEvent.Key.state ev in
      let key = GdkEvent.Key.keyval ev in
      if key = _ISO_Left_Tab then (self#tbuffer#indent ~dir:`BACKWARD (); true)
      else if key = _Tab then begin
        self#tbuffer#indent ();
        true
      end
      else if (state = [] || state = [`SHIFT]) && key = _Home then begin
        let iter = self#buffer#get_iter `INSERT in
        if iter#chars_in_line > 1 then begin
          if smart_home then begin
            let prev = iter#copy in
            let where = iter#set_line_offset 0 in
            let where = if Glib.Unichar.isspace where#char then begin
              let first_non_blank = where#forward_find_char not_blank in
              if first_non_blank#line <> iter#line then begin
                if iter#line_index > 0 then where else iter#forward_to_line_end
              end else first_non_blank
            end else where in
            self#buffer#move_mark `INSERT ~where;
            if state <> [`SHIFT] then self#buffer#move_mark `SEL_BOUND ~where;
            prev#compare where <> 0
          end else if iter#starts_line && (not smart_home) then begin
            let where = iter#forward_find_char not_blank in
            self#buffer#move_mark `INSERT ~where;
            if state <> [`SHIFT] then self#buffer#move_mark `SEL_BOUND ~where;
            true
          end else false;
        end else false
      end
      else if (state = [] || state = [`SHIFT]) && key = _End then begin
        let backward_non_blank iter =
          let rec f it =
            let stop = it#backward_char in
            if List.for_all ((<>) (it#get_text ~stop)) [" "; "\t"; "\r"] then it
            else f stop
          in
          f iter
        in
        let iter = self#buffer#get_iter `INSERT in
        if iter#ends_line then begin
          let where = backward_non_blank iter in
          self#buffer#move_mark `INSERT ~where;
          if state <> [`SHIFT] then self#buffer#move_mark `SEL_BOUND ~where;
          true
        end else if not iter#ends_line && smart_end then begin
          let prev = iter#copy in
          let where = backward_non_blank iter#forward_to_line_end in
          self#buffer#move_mark `INSERT ~where;
          if state <> [`SHIFT] then self#buffer#move_mark `SEL_BOUND ~where;
          prev#compare where <> 0
        end else false
      end
      else if state = [`CONTROL] && key = _Up then (self#scroll `UP; true)
      else if state = [`CONTROL] && key = _Down then (self#scroll `DOWN; true)
      else false;
    end);
(*    (** Paste clipboard *)
    ignore (self#connect#paste_clipboard ~callback:begin fun () ->
      match (GData.clipboard (Gdk.Atom.clipboard))#text with
        | None -> ()
        | Some s ->
          if self#editable then begin
            GtkSignal.stop_emit();
            self#buffer#delete_interactive ~start:(self#buffer#get_iter `INSERT) ~stop:(self#buffer#get_iter `SEL_BOUND) ();
            ignore (self#buffer#insert_interactive s);
          end
    end);
*)end














