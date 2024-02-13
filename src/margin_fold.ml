open Margin
open Merlin_j
open GUtil
open Preferences
open Printf
open Miscellanea

module Icons = struct
  let expander_open = "\u{f107}"
  let expander_closed = "\u{f105}"
end

let counter = ref 0

let mk_polygon n r =
  let pi = 3.14189 in
  let m = float n in
  Array.make n r
  |> Array.mapi begin fun i r ->
    let i = float i in
    r *. cos (2. *. pi *. i /. m) |> int_of_float,
    r *. sin (2. *. pi *. i /. m) |> int_of_float
  end

let dot = mk_polygon 4 2.3 |> Array.to_list

class expander ~(view : Ocaml_text.view) ~start ~stop ~tag_highlight ~tag_invisible ?packing () =
  let ebox = GBin.event_box ?packing () in
  let label = Gtk_util.label_icon ~width:30 (sprintf "<span size='x-small'>%d</span>%s" !counter Icons.expander_open) ~packing:ebox#add in
  let buffer = view#buffer in
  let id = !counter in
  let mark =
    let m = buffer#create_mark ~name:(sprintf "fold-%d" id) start in (* mark placed in the initializer *)
    GtkText.Mark.set_visible m true;
    `MARK m
  in
  object (self)
    inherit GObj.widget ebox#as_widget
    val mutable is_valid = true
    val mutable is_expanded = true
    val mutable hash = 0

    (** The end of the folding point which is also the end of the invisible region
        ({i body}) and the end of the highlighted region when the pointer is over
        the expander ({i head} + {i body}). *)
    val mutable foot = start (* dummy initial value *)

    (** The start *)
    val mutable start = start

    val toggled = new toggled()
    val refresh_needed = new refresh_needed()

    initializer
      incr counter;
      self#relocate start stop;
      hash <- self#hash;
      self#place_mark();
      view#add_child_in_window ~child:self#coerce ~which_window:`LEFT ~x:0 ~y:0;
      ebox#misc#set_property "visible-window" (`BOOL true);
      ebox#event#connect#button_press ~callback:begin fun ev ->
        if is_expanded then self#collapse() else self#expand();
        false
      end |> ignore;
      (* Preview of the fold region when the mouse is over the expander. *)
      ebox#event#connect#enter_notify ~callback:begin fun ev ->
        Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `HAND2);
        if is_expanded then buffer#apply_tag tag_highlight ~start:self#head ~stop:foot;
        false
      end |> ignore;
      ebox#event#connect#leave_notify ~callback:begin fun ev ->
        Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `ARROW);
        buffer#remove_tag tag_highlight ~start:self#head ~stop:foot;
        false
      end |> ignore;
      self#misc#connect#destroy ~callback:begin fun () ->
        buffer#delete_mark mark
      end |> ignore;
      (*      view#obuffer#undo#connect#after#undo ~callback:begin fun ~name ->
              let iter = buffer#get_iter `INSERT in
              let is_invisible = iter#tags |> List.exists (fun t -> t#get_oid = tag_invisible#get_oid) in
              Printf.printf "UNDO %b %d:%d\n%!" is_invisible (iter#line + 1) iter#line_offset;
              if self#is_collapsed && is_invisible then Gmisclib.Idle.add ~prio:300 self#expand
              end |> ignore;*)

    method private place_mark () = buffer#move_mark mark ~where:self#folding_point#forward_to_line_end

    method private hash =
      buffer#get_text ~start ~stop:self#body () |> Hashtbl.hash

    method relocate (istart : GText.iter) (istop : GText.iter) =
      start <- istart;
      foot <- istop#forward_line#set_line_index 0;
      self#place_mark();
      if hash <> 0 && self#hash <> hash then begin
        Gmisclib.Idle.add begin fun () ->
          self#expand();
          is_valid <- false;
          self#misc#hide();
          refresh_needed#call()
        end
      end

    method is_valid = is_valid
    method id = id

    (** The start of the region to highlight when the mouse pointer is over the expander. *)
    method private head = start#set_line_offset 0

    (** The iter where the {i folding point} starts. It is between head and body. *)
    method folding_point = start

    (** The start of the region to be made invisible when the expander is collapsed. *)
    method body = buffer#get_iter mark

    method is_expanded = is_expanded
    method is_collapsed = not is_expanded
    method body_contains iter = self#body#compare iter <= 0 && iter#compare foot <= 0

    (** An expander is visible if its start position does not have a tag with
        the invisible property, that is, if it is not contained in the body of
        another collapsed expander. *)
    method is_visible =
      self#misc#get_flag `VISIBLE &&
      (self#body#set_line_offset 0)#tags |> List.for_all (fun t -> t#get_oid <> tag_invisible#get_oid)

    method expand () =
      let was_collapsed = self#is_collapsed in
      Printf.kprintf label#set_label "<span size='x-small'>%d</span>%s" id Icons.expander_open;
      Gmisclib.Idle.add ~prio:200 begin fun () ->
        self#show_region();
        if was_collapsed then toggled#call true;
        GtkBase.Widget.queue_draw view#as_widget (* Updates ellipsis *)
      end;
      is_expanded <- true;

    method collapse () =
      let iter = buffer#get_iter `INSERT in
      if iter#compare self#body > 0 && iter#compare foot <= 0 then
        buffer#move_mark `INSERT ~where:self#body;
      let was_expanded = is_expanded in
      buffer#remove_tag tag_highlight ~start:self#head ~stop:foot;
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        self#hide_region();
        if was_expanded then toggled#call false;
      end;
      Printf.kprintf label#set_label "<span size='x-small'>%d</span>%s" id Icons.expander_closed;
      is_expanded <- false;

    method hide_region () =
      buffer#apply_tag tag_invisible ~start:self#body ~stop:foot;

    method show_region () =
      buffer#remove_tag tag_invisible ~start:self#body ~stop:foot;

    method show top left =
      let yl, _ = view#get_line_yrange start in
      let y = yl - top + view#pixels_above_lines in
      view#move_child ~child:self#coerce ~x:left ~y;
      self#misc#show()

    method connect = new signals ~toggled ~refresh_needed
  end

and toggled () = object inherit [bool] signal () end
and refresh_needed () = object inherit [unit] signal () end
and signals ~toggled ~refresh_needed =
  object
    inherit ml_signals [toggled#disconnect; refresh_needed#disconnect]
    method toggled = toggled#connect ~after
    method refresh_needed = refresh_needed#connect ~after
  end


class margin_fold (view : Ocaml_text.view) =
  let size = 30(*13*) in
  let spacing = 5 in
  let buffer = view#obuffer in
  let tag_highlight = buffer#create_tag ~name:"fold-highlight"
      [ `PARAGRAPH_BACKGROUND (?? Oe_config.code_folding_highlight_color) ] in
  let tag_invisible = buffer#create_tag ~name:"fold-invisible"
      [ `INVISIBLE true ] in
  let merlin func =
    let filename = match buffer#file with Some file -> file#filename | _ -> "" in
    let source_code = buffer#get_text () in
    func ~filename ~source_code
  in
  let rec flatten level (ol : Merlin_j.outline list) =
    match ol with
    | [] -> []
    | hd :: tl ->
        hd.ol_level <- level;
        List.rev_append (hd :: (flatten level tl)) (flatten (level + 1) hd.ol_children)
        (*hd :: (flatten (List.rev_append hd.ol_children tl))*)
  in
  let rec walk f parent (ol : Merlin_j.outline list) =
    match ol with
    | [] -> ()
    | hd :: tl ->
        let walk_children = f parent hd in
        if walk_children then walk f (Some hd) hd.ol_children;
        walk f parent tl
  in
  let char_width =
    let desc =
      (* TODO Update on preferences change *)
      Preferences.preferences#get.Settings_t.editor_base_font
      |> GPango.font_description
    in
    GPango.to_pixels (view#misc#pango_context#get_metrics ~desc ())#approx_digit_width in
  object (self)
    inherit margin()

    (** The time the folding points currently in cache were calculated. *)
    val mutable last_outline_time = 0.0

    (** The margin is refresh pending when it is requested to be drawn but cannot be
        drawn because the buffer has changed and folding points currently
        cached have become invalid. The drawing of the margin is therefore
        postponed until the folding points are refreshed. *)
    val mutable is_refresh_pending = false

    (** The currently cached folding points. *)
    val mutable outline = []

    val mutable expanders : expander list = []
    val expander_toggled = new expander_toggled()
    val synchronized = new synchronized()
    method index = 30
    method size = size

    method is_refresh_pending = is_refresh_pending
    method clear_refresh_pending () = is_refresh_pending <- false
    method is_changed_after_last_outline = last_outline_time < view#tbuffer#last_edit_time

    method sync_outline_time () =
      last_outline_time <- Unix.gettimeofday();
      synchronized#call();

    method draw ~view ~top ~left ~height ~start ~stop =
      if not self#is_changed_after_last_outline then begin
        let buffer_start_line = start#line in
        let buffer_stop_line = stop#line in
        let methods = ref [] in
        expanders <-
          List.filter_map (fun ex ->
              ex#misc#hide();
              if not ex#is_valid then (ex#destroy(); None)
              else Some ex
            ) expanders;
        outline
        |> walk begin fun parent ol ->
          let fold_start_line = ol.ol_start.line - 1 in
          let is_folding_point =
            ol.ol_kind = "Method"
            || buffer_start_line <= fold_start_line
               && fold_start_line <= buffer_stop_line
               && ol.ol_start.line <> ol.ol_stop.line
          in
          if is_folding_point then begin
            self#draw_expander ol top left;
            if ol.ol_kind = "Method" then begin
              ol.ol_parent <- parent;
              methods := ol :: !methods
            end
          end;
          true
        end None;
        !methods
        |> Xlist.group_by (fun ol -> ol.ol_parent)
        |> List.iter begin fun (parent, meths) ->
          match parent with
          | Some parent  ->
              ({ parent with ol_start = parent.ol_stop } :: meths)
              |> List.sort (fun m1 m2 -> Stdlib.compare m1.ol_start m2.ol_start)
              |> Xlist.pairwise
              |> List.iter (fun (ol1, ol2) -> ol1.ol_stop <- ol2.ol_start);
          | _ -> ()
        end
        |> ignore
      end else is_refresh_pending <- true

    method private draw_expander ol top left =
      (*Printf.printf "draw_expander %d:%d -- %d:%d [%d] [%s %d]\n%!"
        ol.ol_start.line ol.ol_start.col ol.ol_stop.line ol.ol_stop.col
        (Thread.self() |> Thread.id) ol.ol_kind ol.ol_level;*)
      let start = buffer#get_iter (`LINECHAR (ol.ol_start.line - 1, ol.ol_start.col))  in
      let stop =
        if ol.ol_kind = "Method" then
          let stop = buffer#get_iter (`LINECHAR (ol.ol_stop.line - 1, ol.ol_stop.col)) in
          (stop#set_line_offset 0)#backward_find_char Text_util.not_blank;
        else
          buffer#get_iter (`LINECHAR (ol.ol_stop.line - 1, ol.ol_stop.col)) in
      (*(*if ol.ol_kind = "Method" then begin
        Printf.printf "%S: %d:%d - %d:%d -- %d:%d\n%!"
          (buffer#get_text ~start ~stop:start#forward_to_line_end ())
          ol.ol_start.line ol.ol_start.col
          ol.ol_stop.line ol.ol_stop.col
          (start#line + 1) start#line_offset;*)
        end;*)
      let expander =
        match expanders |> List.find_opt (fun exp -> exp#folding_point#equal start) with
        | None ->
            let expander = new expander ~start ~stop ~tag_highlight ~tag_invisible ~view () in
            expanders <- expander :: expanders;
            (*Printf.printf "NEW %d-%d\n%S\n%!" (start#line + 1) (stop#line + 1) (String.sub key 0 (String.index key '\n'));*)
            expander#show_region();
            expander#connect#toggled ~callback:(fun _ -> expander_toggled#call expander) |> ignore;
            expander#connect#refresh_needed ~callback:(fun () -> is_refresh_pending <- true) |> ignore;
            expander
        | Some expander ->
            expander#relocate start stop;
            expander
      in
      (* Hide expanders inside invisible regions *)
      if List.exists (fun t -> t#get_oid = tag_invisible#get_oid) start#tags
      then expander#misc#hide()
      else expander#show top left;

    method draw_ellipsis _ =
      match view#get_window `TEXT with
      | Some window ->
          let line_width = 1 in
          let drawable = new GDraw.drawable window in
          drawable#set_line_attributes ~width:line_width ~style:`SOLID ();
          drawable#set_foreground (`NAME "#ff0000");
          let vrect = view#visible_rect in
          let y0 = Gdk.Rectangle.y vrect in
          expanders
          |> List.iter begin fun expander ->
            if expander#is_collapsed && expander#is_visible then
              let start = expander#body in
              let y, height = view#get_line_yrange start in
              let x_chars = start#line_index in
              let x = (x_chars + 1) * char_width in
              let y = y - y0 + 1 in
              let height = height - 4 in (* do not overlap current line border *)
              let width = height * 8 / 5 in
              drawable#rectangle ~x ~y ~filled:false ~width ~height ();
              let h3 = height / 3 in
              let x = x + width / 2 in
              let y = y + h3 + h3 in
              dot |> List.map (fun (xd, yd) -> x + xd - h3, y + yd) |> drawable#polygon ~filled:true;
              dot |> List.map (fun (xd, yd) -> x + xd,      y + yd) |> drawable#polygon ~filled:true;
              dot |> List.map (fun (xd, yd) -> x + xd + h3, y + yd) |> drawable#polygon ~filled:true;
          end
      | _ -> ()

    method amend_nested_collapsed (expander : expander) =
      if expander#is_expanded then begin
        List.iter begin fun exp ->
          (* When the outer expander is expanded, all nested ones are also
             expanded, because the tag_highlight is removed everywhere, but
             the expander state remains "collapsed". *)
          if exp#is_collapsed && expander#body_contains exp#body
          then exp#hide_region()
        end expanders
      end

    method private invoke_merlin () =
      if self#is_changed_after_last_outline then begin
        merlin@@Merlin.outline begin fun (ol : Merlin_j.outline list) ->
          GtkThread.async begin fun () ->
            self#sync_outline_time();
            outline <- (*flatten 0*) ol;
          end ()
        end;
      end;
      true

    val mutable timer_id = None

    method private start_timer () =
      match timer_id with
      | None ->
          self#invoke_merlin() |> ignore;
          timer_id <- Some (GMain.Timeout.add ~ms:100 ~callback:self#invoke_merlin);
      | _ -> ()

    method private stop_timer() =
      Option.iter GMain.Timeout.remove timer_id;
      timer_id <- None

    method connect = new margin_signals ~expander_toggled ~synchronized

    initializer
      view#event#connect#focus_in ~callback:(fun _ -> self#start_timer(); false) |> ignore;
      view#event#connect#focus_out ~callback:(fun _ -> self#stop_timer(); false) |> ignore;
      view#event#connect#expose ~callback:(fun ev -> self#draw_ellipsis ev; false) |> ignore;
      buffer#connect#mark_set ~callback:begin fun iter mark ->
        (* It would be better to have the undo manager tell you when to open
           the expander instead of checking every single cursor movement... *)
        match GtkText.Mark.get_name mark with
        | Some "insert" ->
            expanders
            |> List.iter begin fun exp ->
              if exp#is_collapsed && iter#line > exp#body#line && exp#body_contains iter
              then (*Gmisclib.Idle.add*) exp#expand()
            end
        | _ -> ()
      end |> ignore;
      self#start_timer()
  end

and expander_toggled () = object inherit [expander] signal () end
and synchronized () = object inherit [unit] signal () end
and margin_signals ~expander_toggled ~synchronized =
  object
    inherit ml_signals [expander_toggled#disconnect; synchronized#disconnect]
    method expander_toggled = expander_toggled#connect ~after
    method synchronized = synchronized#connect ~after
  end

let init_page (page : Editor_page.page) =
  try
    let margin = new margin_fold page#ocaml_view in
    page#view#margin#add (margin :> Margin.margin);
    margin#connect#synchronized ~callback:begin fun () ->
      if margin#is_refresh_pending then begin
        Gmisclib.Idle.add ~prio:100 page#view#draw_gutter;
        margin#clear_refresh_pending()
      end
    end |> ignore;
    margin#connect#expander_toggled ~callback:begin fun expander ->
      margin#amend_nested_collapsed expander;
      Gmisclib.Idle.add ~prio:300 (fun () -> page#view#draw_gutter())
    end |> ignore;
  with ex ->
    Printf.eprintf "File \"margin_fold.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())

let init_editor editor =
  editor#connect#add_page ~callback:init_page |> ignore;
  (*editor#connect#remove_page ~callback:begin fun page ->
    ()
    end |> ignore*)

