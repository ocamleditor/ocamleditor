open Margin
open Merlin_j
open GUtil
module ColorOps = Color
open Preferences
open Printf
open Utils
open Settings_j

module Log = Common.Log.Make(struct let prefix = "FOLD" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

module Icons = struct
  let expander_open = "\u{f107}"
  let expander_closed = "\u{f105}"
end

let counter = ref 0
let is_debug = false

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

class expander ~(view : Ocaml_text.view) ~tag_highlight ~tag_invisible ?packing () =
  let ebox = GBin.event_box ?packing () in
  let markup = if is_debug then sprintf "<span size='x-small'>%d</span>%s" !counter Icons.expander_open else Icons.expander_open in
  let label = Gtk_util.label_icon markup ~packing:ebox#add in
  let buffer = view#buffer in
  let id = !counter in
  let mark =
    let m = buffer#create_mark ~name:(sprintf "fold-%d" id) buffer#start_iter in (* mark placed in the initializer *)
    GtkText.Mark.set_visible m is_debug;
    `MARK m
  in
  let mark_foot =
    let m = buffer#create_mark ~name:(sprintf "fold-foot-%d" id) buffer#start_iter in (* mark placed in the initializer *)
    GtkText.Mark.set_visible m is_debug;
    `MARK m
  in
  object (self)
    inherit GObj.widget ebox#as_widget

    (** The expander is invalid if the text it contains has changed and, as
        such, has already been destroyed *)
    val mutable is_valid = true
    val mutable is_expanded = true
    val mutable is_definition = false
    val mutable hash = 0

    val toggled = new toggled()
    val refresh_needed = new refresh_needed()
    val mutable contains_mark_occurrence = false

    initializer
      incr counter;
      view#add_child_in_window ~child:self#coerce ~which_window:`LEFT ~x:0 ~y:0;
      ebox#misc#set_property "visible-window" (`BOOL is_debug);
      ebox#event#connect#button_press ~callback:begin fun ev ->
        if is_expanded then self#collapse() else self#expand();
        false
      end |> ignore;
      (* Preview of the fold region when the mouse is over the expander. *)
      ebox#event#connect#enter_notify ~callback:begin fun ev ->
        Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `HAND2);
        if is_expanded then buffer#apply_tag tag_highlight ~start:self#head ~stop:self#foot;
        false
      end |> ignore;
      ebox#event#connect#leave_notify ~callback:begin fun ev ->
        Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `ARROW);
        if is_expanded then
          (* FIX add tag_highlight again to the nested expanders *)
          buffer#remove_tag tag_highlight ~start:self#head ~stop:self#foot;
        false
      end |> ignore;
      self#misc#connect#destroy ~callback:begin fun () ->
        buffer#delete_mark mark;
        buffer#delete_mark mark_foot
      end |> ignore

    method private hash =
      buffer#get_text ~start:self#head  ~stop:self#body () |> Hashtbl.hash

    method set_is_definition x = is_definition <- x
    method is_definition = is_definition

    method relocate (istart : GText.iter) (istop : GText.iter) =
      buffer#move_mark mark ~where:istart;
      let where =
        (* Handles definitions like "type...{...\n} and ..." or ";;" *)
        let limit = istop#forward_to_line_end in
        let istop =
          match istop#forward_search ~limit ";;" with
          | Some (_, it) -> it
          | _ -> istop
        in
        let it = istop#forward_find_char ~limit Text_util.not_blank in
        if it#ends_line then istop#forward_line#set_line_offset 0 else it
      in
      buffer#move_mark mark_foot ~where;
      if hash <> 0 && self#hash <> hash then begin
        Gmisclib.Idle.add begin fun () ->
          self#expand();
          is_valid <- false;
          self#misc#hide();
          self#destroy();
          refresh_needed#call()
        end
      end else hash <- self#hash

    method is_valid = is_valid

    method id = id

    (** The iter where the {i folding point} starts. It is between head and body. *)
    method folding_point = buffer#get_iter mark

    (** The start of the region to highlight when the mouse pointer is over the expander. *)
    method private head = self#body#set_line_offset 0

    (** The start of the region to be made invisible when the expander is collapsed. *)
    method body = self#folding_point#forward_to_line_end
    method foot = buffer#get_iter mark_foot

    method is_expanded = is_expanded
    method is_collapsed = not is_expanded
    method body_contains iter = self#body#compare iter <= 0 && iter#compare self#foot < 0

    (** An expander is visible if its start position does not have a tag with
        the invisible property, that is, if it is not contained in the body of
        another collapsed expander. *)
    method is_visible =
      self#misc#get_flag `VISIBLE &&
      (self#body#set_line_offset 0)#tags |> List.for_all (fun t -> t#get_oid <> tag_invisible#get_oid)

    method expand () =
      let was_collapsed = self#is_collapsed in
      label#set_label
        (if is_debug then sprintf "<span size='x-small'>%d</span>%s" id Icons.expander_open
         else sprintf "<big>%s</big>" Icons.expander_open);
      self#show_region();
      is_expanded <- true;
      if was_collapsed then toggled#call true;
      GtkBase.Widget.queue_draw view#as_widget; (* Updates ellipsis *)

    method collapse () =
      let iter = buffer#get_iter `INSERT in
      if iter#compare self#body > 0 && iter#compare self#foot <= 0 then
        buffer#place_cursor ~where:self#body;
      let was_expanded = is_expanded in
      Gmisclib.Idle.add begin fun () ->
        buffer#remove_tag tag_highlight ~start:self#head ~stop:self#foot;
        self#hide_region();
        if was_expanded then toggled#call false;
      end;
      label#set_label
        (if is_debug then sprintf "<span size='x-small'>%d</span>%s" id Icons.expander_closed
         else sprintf "<big>%s</big>" Icons.expander_closed);
      is_expanded <- false;

    method hide_region () =
      buffer#apply_tag tag_invisible ~start:self#body ~stop:self#foot;
      buffer#apply_tag tag_highlight ~start:self#head ~stop:self#head#forward_to_line_end;

    method show_region () =
      buffer#remove_tag tag_invisible ~start:self#body ~stop:self#foot;
      buffer#remove_tag tag_highlight ~start:self#head ~stop:self#head#forward_to_line_end;

    method show top left height =
      let yl, _ = view#get_line_yrange self#head in
      let y = yl - top + view#pixels_above_lines in
      if y >= 0 && y <= height then begin
        view#move_child ~child:self#coerce ~x:left ~y;
        self#misc#show()
      end else self#misc#hide()

    method set_contains_mark_occurrence value = contains_mark_occurrence <- value

    method contains_mark_occurrence = contains_mark_occurrence

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
  let size = if is_debug then 30 else 13 in
  let spacing = 5 in
  let buffer = view#obuffer in
  let add_tag name properties =
    match GtkText.TagTable.lookup buffer#tag_table name with
    | Some tag -> new GText.tag tag
    | _ -> buffer#create_tag ~name properties
  in
  let color_expander = Oe_config.code_folding_expander_color in
  let color_occurrences = `NAME ?? (Preferences.preferences#get.editor_mark_occurrences_bg_color) in
  let tag_highlight = add_tag Oe_config.code_folding_tag_highlight_name
      [ `PARAGRAPH_BACKGROUND (?? Oe_config.code_folding_highlight_color) ] in
  let tag_invisible = add_tag Oe_config.code_folding_tag_invisible_name
      [ `INVISIBLE true ] in
  let merlin text func =
    let filename = match buffer#file with Some file -> file#filename | _ -> "" in
    func ~filename ~buffer:text
  in
  let rec walk f parent (ol : Merlin_j.outline list) =
    match ol with
    | [] -> ()
    | hd :: tl ->
        f parent hd;
        walk f (Some hd) hd.ol_children;
        walk f parent tl
  in
  let char_width =
    let desc =
      (* TODO Update on preferences change *)
      Preferences.preferences#get.Settings_t.editor_base_font
      |> GPango.font_description
    in
    GPango.to_pixels (view#misc#pango_context#get_metrics ~desc ())#approx_digit_width
  in
  let rec skip_comments_backward (comments : (int * int * bool) list) (start : GText.iter) =
    let it = (start#backward_find_char Text_util.not_blank)#forward_char in
    let offset = it#offset in
    match comments |> List.find_opt (fun (_, e, _) -> offset = e) with
    | Some (b, e, _) -> skip_comments_backward comments (buffer#get_iter (`OFFSET b))
    | _ -> buffer#get_iter (`OFFSET offset)
  in
  let is_drawable buffer_start_line buffer_stop_line =
    fun [@ inline] ol ->
      (*let fold_start_line = ol.ol_start.line - 1 in
        buffer_start_line <= fold_start_line &&
        fold_start_line <= buffer_stop_line &&*)
      ol.ol_stop.line - ol.ol_start.line > 2
  in
  object (self)
    inherit margin()

    (** The time the folding points currently in cache were calculated. *)
    val mutable last_outline_time = 0.0

    (** The margin is refresh pending when it is requested to be drawn but cannot be
        drawn because the buffer has changed and folding points currently
        cached have become invalid. The drawing of the margin is therefore
        postponed until the folding points are back in sync. *)
    val mutable is_refresh_pending = false

    (** The currently cached folding points. *)
    val mutable outline = []

    val mutable comments = []

    val mutable expanders : expander list = []
    val expander_toggled = new expander_toggled()
    val synchronized = new synchronized()
    val mutable signals = []

    method kind = FOLDING
    method index = 30
    method size = size

    method is_refresh_pending = is_refresh_pending
    method is_changed_after_last_outline = last_outline_time < view#tbuffer#last_edit_time

    method sync_outline_time () = last_outline_time <- Unix.gettimeofday()

    method draw ~view ~top ~left ~height ~start ~stop =
      if not self#is_changed_after_last_outline then begin
        Log.println `DEBUG "draw (is_changed_after_last_outline)";
        match expanders with
        (* Separate the case in which markers and expanders need to be
           reconstructed from the case in which only the positions within the visible
           area need to be updated (for example in the case of scrolling). *)
        | _ when expanders = [] || is_refresh_pending ->
            let is_drawable = is_drawable 0 0 (*start#line stop#line*) in (* dummy values *)
            let methods = ref [] in
            expanders <-
              List.filter_map begin fun ex ->
                ex#misc#hide();
                if ex#is_valid then Some ex else None
              end expanders;
            Log.println `DEBUG "draw 1.0";
            outline
            |> walk begin fun parent ol ->
              let is_folding_point = ol.ol_kind = "Method" || is_drawable ol in
              if is_folding_point then begin
                if ol.ol_kind = "Method" then begin
                  ol.ol_parent <- parent;
                  methods := ol :: !methods
                end else
                  self#draw_expander ol top left height;
              end
            end None;
            Log.println `DEBUG "draw 1.1";
            !methods
            |> List.filter (fun ol -> ol.ol_parent <> None)
            |> ListExt.group_by (fun ol -> ol.ol_parent)
            |> List.iter begin fun (parent, meths) ->
              match parent with
              | Some parent  ->
                  ({ parent with ol_start = parent.ol_stop } :: meths)
                  |> List.sort (fun m1 m2 -> Stdlib.compare m1.ol_start m2.ol_start)
                  |> ListExt.pairwise
                  |> List.iter (fun (ol1, ol2) -> ol1.ol_stop <- ol2.ol_start);
              | _ -> ()
            end
            |> ignore;
            Log.println `DEBUG "draw 1.2";
            !methods |> List.iter (fun ol -> if is_drawable ol then self#draw_expander ol top left height);
            is_refresh_pending <- false;
            Log.println `DEBUG "draw 1.3";
        | _ ->
            Log.println `DEBUG "draw 2, expanders length: %d" (List.length expanders);
            expanders |> List.iter (fun ex -> ex#show top left height);
            Log.println `DEBUG "draw 2.1";
      end else
        is_refresh_pending <- true

    method private draw_expander ol top left height =
      Log.println `DEBUG "draw 3.0 %s %b" ol.ol_kind self#is_changed_after_last_outline;
      let start = buffer#get_iter (`LINECHAR (ol.ol_start.line - 1, ol.ol_start.col)) in
      Log.println `DEBUG "draw 3.1";
      let stop =
        if ol.ol_kind = "Method" then
          (* TODO: Still crashes on the following line, even when is_changed_after_last_outline is false. *)
          let stop = buffer#get_iter (`LINECHAR (ol.ol_stop.line - 1, ol.ol_stop.col)) in
          Log.println `DEBUG "skip_comments_backward";
          skip_comments_backward comments (stop#set_line_offset 0);
        else begin
          Log.print `DEBUG "draw_expander: %d,%d " (ol.ol_stop.line - 1) ol.ol_stop.col;
          let iter = buffer#get_iter (`LINECHAR (ol.ol_stop.line - 1, ol.ol_stop.col)) in
          Log.println `DEBUG "OK";
          iter
        end
      in
      (*(*if ol.ol_kind = "Method" then begin
        Printf.printf "%S: %d:%d - %d:%d -- %d:%d\n%!"
          (buffer#get_text ~start ~stop:start#forward_to_line_end ())
          ol.ol_start.line ol.ol_start.col
          ol.ol_stop.line ol.ol_stop.col
          (start#line + 1) start#line_offset;*)
        end;*)
      (*if stop#line > start#line then begin*)
      let expander =
        match expanders |> List.find_opt (fun exp -> exp#folding_point#equal start) with
        | None ->
            let expander = new expander ~tag_highlight ~tag_invisible ~view () in
            expander#relocate start stop;
            expanders <- expander :: expanders;
            expander#show_region();
            expander#connect#toggled ~callback:(fun _ -> expander_toggled#call expander) |> ignore;
            expander#connect#refresh_needed ~callback:(fun () -> is_refresh_pending <- true) |> ignore;
            expander
        | Some expander ->
            (*expander#relocate start stop;*)
            expander
      in
      expander#set_is_definition (ol.ol_kind = "Value" || ol.ol_kind = "Method");
      (* Hide expanders inside invisible regions *)
      if List.exists (fun t -> t#get_oid = tag_invisible#get_oid) start#tags
      then expander#misc#hide()
      else expander#show top left height
    (*end*)

    method draw_ellipsis _ =
      match view#get_window `TEXT with
      | Some window ->
          let line_width = 1 in
          let drawable = new GDraw.drawable window in
          drawable#set_line_attributes ~width:line_width ~style:`SOLID ();
          drawable#set_foreground color_expander;
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
              if expander#contains_mark_occurrence then begin
                drawable#set_foreground color_occurrences;
                drawable#rectangle ~x ~y ~filled:true ~width ~height ();
                drawable#set_foreground color_expander;
              end;
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
      self#iter_expanders begin fun exp ->
        (* When the outer expander is expanded, all nested ones are also
           expanded, because the tag_highlight is removed everywhere, but
           the expander state remains "collapsed". *)
        if exp#is_collapsed && expander#body_contains exp#body
        then exp#hide_region()
      end

    method iter_expanders func = List.iter func expanders

    method private invoke_merlin () =
      if self#is_changed_after_last_outline then begin
        let source_code = buffer#get_text () in
        self#sync_outline_time();
        (merlin source_code)@@Merlin.outline
        |> Async.start_with_continuation begin function
        | Merlin.Ok (ol : Merlin_j.outline list) ->
            Log.println `DEBUG "STARTING GtkThread.sync";
            GtkThread.sync begin fun () ->
              Log.println `DEBUG "BEGIN GtkThread.sync";
              if not self#is_changed_after_last_outline then begin
                Log.println `DEBUG "update outline";
                outline <- ol;
                comments <-
                  Comments.scan_locale (Glib.Convert.convert_with_fallback ~fallback:""
                                          ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset source_code);
                synchronized#call();
              end else Log.println `WARN "*** outline not updated ***";
              Log.println `DEBUG "END GtkThread.sync";
            end ();
        | Merlin.Failure _ | Merlin.Error _ -> ()
        end
      end;
      true

    val mutable timer_id = None

    method private start_timer () =
      match timer_id with
      | None ->
          self#invoke_merlin() |> ignore;
          timer_id <- Some (GMain.Timeout.add ~ms:300 ~callback:self#invoke_merlin);
      | _ -> ()

    method private stop_timer() =
      Option.iter GMain.Timeout.remove timer_id;
      timer_id <- None

    method private connect_signals () =
      signals <- [
        `VIEW (view#event#connect#focus_in ~callback:(fun _ -> self#start_timer(); false));
        `VIEW (view#event#connect#focus_out ~callback:(fun _ -> self#stop_timer(); false));
        `VIEW (view#event#connect#expose ~callback:(fun ev -> self#draw_ellipsis ev; false));
        `BUFFER (buffer#connect#mark_set ~callback:begin fun _ mark ->
            match GtkText.Mark.get_name mark with
            | Some "insert" ->
                expanders
                |> List.iter begin fun exp ->
                  if exp#is_collapsed then begin
                    let iter = buffer#get_iter `INSERT in
                    if iter#line > exp#body#line && exp#body_contains iter
                    then exp#expand()
                  end
                end
            | _ -> ()
          end);
      ]

    method private disconnect_signals () =
      signals |> List.iter (function
          | `VIEW sign -> GtkSignal.disconnect view#as_view sign
          | `BUFFER sign -> GtkSignal.disconnect buffer#as_buffer sign);
      signals <- [];

    method enable () =
      self#set_is_visible true;
      self#connect_signals();
      self#start_timer();

    method disable () =
      self#set_is_visible false;
      expanders |> List.iter begin fun exp ->
        exp#expand();
        exp#destroy()
      end;
      self#stop_timer();
      expanders <- [];
      outline <- [];
      comments <- [];
      last_outline_time <- 0.0;
      is_refresh_pending <- false;
      self#disconnect_signals();

    method private configure is_enabled =
      if is_enabled then self#enable() else self#disable()

    method connect = new margin_signals ~expander_toggled ~synchronized

    initializer
      self#set_is_visible Preferences.preferences#get.editor_code_folding_enabled;
      Preferences.preferences#connect#changed ~callback:begin fun pref ->
        self#configure pref.editor_code_folding_enabled
      end |> ignore;
      self#configure self#is_visible;
  end

and expander_toggled () = object inherit [expander] signal () end
and synchronized () = object inherit [unit] signal () end
and margin_signals ~expander_toggled ~synchronized =
  object
    inherit ml_signals [expander_toggled#disconnect; synchronized#disconnect]
    method expander_toggled = expander_toggled#connect ~after
    method synchronized = synchronized#connect ~after
  end

let pages : (int * margin_fold) list ref = ref []

let init_page (page : Editor_page.page) =
  try
    page#view#margin#list |> List.find_opt (fun m -> m#kind = FOLDING)
    |> begin function
    | None ->
        let margin = new margin_fold page#ocaml_view in
        page#view#margin#add (margin :> Margin.margin);
        margin#connect#synchronized ~callback:begin fun () ->
          if margin#is_refresh_pending then begin
            (*Gmisclib.Idle.add ~prio:100 begin fun () ->*)
            Log.println `DEBUG "synchronized: begin draw_gutter";
            page#view#draw_gutter(); (* triggers draw *)
            Log.println `DEBUG "synchronized: end draw_gutter";
            (*end;*)
          end
        end |> ignore;
        margin#connect#expander_toggled ~callback:begin fun expander ->
          if expander#is_expanded then margin#amend_nested_collapsed expander;
          Gmisclib.Idle.add ~prio:300 (fun () -> page#view#draw_gutter())
        end |> ignore;
        page#misc#connect#destroy ~callback:begin fun () ->
          pages := List.filter begin fun (oid, margin) ->
              page#view#margin#remove (margin :> Margin.margin);
              oid <> page#misc#get_oid
            end !pages
        end |> ignore;
        pages := (page#misc#get_oid, margin) :: !pages;
        (* Highlight expanders that contain marked occurrences *)
        page#view#mark_occurrences_manager#connect#mark_set ~callback:begin fun () ->
          margin#iter_expanders begin fun exp ->
            exp#set_contains_mark_occurrence false;
            if not exp#is_expanded then begin
              if page#view#mark_occurrences_manager#table
                 |> List.exists (fun (m1, _) ->
                     exp#body_contains (page#buffer#get_iter_at_mark m1))
              then exp#set_contains_mark_occurrence true;
            end
          end
        end |> ignore;
        (* Remove highlights from all expanders when there are no marked occurrences *)
        page#view#mark_occurrences_manager#connect#mark_set ~callback:begin fun () ->
          match page#view#mark_occurrences_manager#table with
          | [] -> GtkBase.Widget.queue_draw page#view#as_widget
          | _ -> ()
        end |> ignore;
        (* Disable while page is loading. *)
        page#connect#load ~callback:begin
          let old_is_visible = ref margin#is_visible in
          function
          | `Begin ->
              old_is_visible := margin#is_visible;
              margin#disable()
          | `End when !old_is_visible ->
              Gmisclib.Idle.add ~prio:300 begin fun () ->
                margin#enable();
                page#view#draw_gutter();
                GtkBase.Widget.queue_draw page#view#as_widget;
              end
          | _ -> ()
        end |> ignore;
    | _ -> ()
    end
  with ex ->
    Printf.eprintf "File \"margin_fold.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())

let iter_page_expanders (page : Editor_page.page) callback =
  match !pages |> List.assoc_opt page#misc#get_oid with
  | Some margin -> margin#iter_expanders callback
  | _ -> ()

let collapse_to_definitions (page : Editor_page.page) =
  iter_page_expanders page (fun exp -> if exp#is_definition then exp#collapse())

let expand_all (page : Editor_page.page) =
  iter_page_expanders page (fun exp -> exp#expand())

let init_editor editor =
  editor#connect#add_page ~callback:init_page |> ignore;
  (*  editor#connect#remove_page ~callback:begin fun page ->
      pages := List.filter begin fun (oid, margin) ->
          page#view#margin#remove (margin :> Margin.margin);
          oid <> page#misc#get_oid
        end !pages
      end |> ignore*)

