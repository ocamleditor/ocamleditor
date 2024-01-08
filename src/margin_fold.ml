open Margin
open Merlin_j
open GUtil
open Preferences

module Icons = struct
  let expander_open = "\u{f107}"
  let expander_closed = "\u{f105}"
end

class expander ~(view : Ocaml_text.view) ~start ~stop ~tag_highlight ~tag_invisible ?packing () =
  let ebox = GBin.event_box ?packing () in
  let label = Gtk_util.label_icon Icons.expander_open ~packing:ebox#add in
  let buffer = view#buffer in
  let get_geometry start stop =
    start#set_line_offset 0,
    start#forward_line#set_line_offset 0,
    stop#forward_line#set_line_index 0
  in
  let start_highlight, start_invisible, stop = get_geometry start stop in
  object (self)
    inherit GObj.widget ebox#as_widget
    val mutable is_expanded = true
    val mutable in_use = true
    val mutable start_highlight = start_highlight
    val mutable start_invisible = start_invisible
    val mutable start = start
    val mutable stop = stop
    val toggled = new toggled()

    initializer
      ebox#misc#set_property "visible-window" (`BOOL false);
      ebox#event#connect#button_press ~callback:begin fun ev ->
        if is_expanded then self#collapse() else self#expand();
        false
      end |> ignore;
      ebox#event#connect#enter_notify ~callback:begin fun ev ->
        Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `HAND2);
        (*Printf.printf "-->%d,%d -- %d,%d\n%!" (start_highlight#line + 1) start_highlight#line_offset (stop#line + 1) stop#line_offset;*)
        if is_expanded then buffer#apply_tag tag_highlight ~start:start_highlight ~stop;
        false
      end |> ignore;
      ebox#event#connect#leave_notify ~callback:begin fun ev ->
        Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `ARROW);
        buffer#remove_tag tag_highlight ~start:start_highlight ~stop;
        false
      end |> ignore;

    method private set_bounds (istart : GText.iter) (istop : GText.iter) =
      let start_highlight', start_invisible', stop' = get_geometry istart istop in
      start_highlight <- start_highlight';
      start_invisible <- start_invisible';
      start <- istart;
      stop <- stop'

    method bounds() = start, stop
    method is_expanded = is_expanded
    method in_use = in_use

    method abandon() =
      ebox#misc#hide();
      in_use <- false

    method regain start stop =
      self#set_bounds start stop;
      in_use <- true

    method expand () =
      let was_collapsed = not is_expanded in
      label#set_text Icons.expander_open;
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        buffer#remove_tag tag_invisible ~start:start_invisible ~stop;
        if was_collapsed then toggled#call true
      end;
      is_expanded <- true;

    method collapse () =
      let iter = buffer#get_iter `INSERT in
      if iter#compare start >= 0 && iter#compare stop <= 0 then
        buffer#move_mark `INSERT ~where:start#backward_line#forward_to_line_end;
      let was_expanded = is_expanded in
      buffer#remove_tag tag_highlight ~start:start_highlight ~stop;
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        buffer#apply_tag tag_invisible ~start:start_invisible ~stop;
        if was_expanded then toggled#call false;
      end;
      label#set_text Icons.expander_closed;
      is_expanded <- false;

    method hide_region () =
      Printf.printf "hide_region %d %d \n%!" (start_invisible#line + 1) (stop#line + 1);
      buffer#apply_tag tag_invisible ~start:start_invisible ~stop;

    method show_region () =
      Printf.printf "show_region %d-%d\n%!" (start#line + 1) (stop#line + 1);
      buffer#remove_tag tag_invisible ~start ~stop;

    method connect = new signals ~toggled
  end

and toggled () = object inherit [bool] signal () end
and signals ~toggled =
  object
    inherit ml_signals [toggled#disconnect]
    method toggled = toggled#connect ~after
  end


class margin_fold (view : Ocaml_text.view) =
  let size = 13 in
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
  object (self)
    inherit margin()
    val mutable last_outline_time = 0.0
    val mutable is_pending = false
    val mutable outline = []
    val mutable expanders : (string, expander) Hashtbl.t = Hashtbl.create 7
    val expander_toggled = new expander_toggled()
    val synchronized = new synchronized()
    method index = 30
    method size = size

    method is_pending = is_pending
    method clear_pending () = is_pending <- false
    method is_changed_after_last_outline = last_outline_time < view#tbuffer#last_edit_time
    method sync_outline_time () =
      last_outline_time <- Unix.gettimeofday();
      (*Printf.printf "sync [%d]\n%!" (Thread.self() |> Thread.id);*)
      synchronized#call();

    method draw ~view ~top ~left ~height ~start ~stop =
      (*Printf.printf "is_changed_after_last_outline = %b\n%!" self#is_changed_after_last_outline;*)
      if not self#is_changed_after_last_outline then begin
        let buffer_start_line = start#line in
        let buffer_stop_line = stop#line in
        Hashtbl.iter (fun _ ex -> ex#abandon()) expanders;
        outline
        |> List.iter begin fun ol ->
          let fold_start_line = ol.ol_start.line - 1 in
          let is_folding_point =
            buffer_start_line <= fold_start_line && fold_start_line <= buffer_stop_line
            && (ol.ol_kind = "Method" || ol.ol_start.line <> ol.ol_stop.line)
          in
          if is_folding_point then self#draw_expander ol top left
        end;
        Hashtbl.iter begin fun key ex ->
          if not ex#in_use then begin
            ex#destroy();
            Hashtbl.remove expanders key
          end
        end expanders
      end else is_pending <- true

    method private draw_expander ol top left =
      (*Printf.printf "draw_expander %d:%d -- %d:%d [%d] [%s %d]\n%!"
        ol.ol_start.line ol.ol_start.col ol.ol_stop.line ol.ol_stop.col
        (Thread.self() |> Thread.id) ol.ol_kind ol.ol_level;*)
      let start = buffer#get_iter (`LINECHAR (ol.ol_start.line - 1, ol.ol_start.col))  in
      let stop = buffer#get_iter (`LINECHAR (ol.ol_stop.line - 1, ol.ol_stop.col)) in
      (*Printf.printf "%S: %d:%d - %d:%d -- %d:%d\n%!"
        (buffer#get_text ~start ~stop:start#forward_to_line_end ())
        ol.ol_start.line ol.ol_start.col ol.ol_stop.line ol.ol_stop.col
        start#line start#line_offset;*)
      let yl, _ = view#get_line_yrange start in
      let y = yl - top + view#pixels_above_lines in
      let key = buffer#get_text ~start ~stop ~visible:false () (*|> Hashtbl.hash*) in
      let expander =
        match Hashtbl.find_opt expanders key with
        | None ->
            let expander = new expander ~start ~stop ~tag_highlight ~tag_invisible ~view () in
            Hashtbl.replace expanders key expander;
            view#add_child_in_window ~child:expander#coerce ~which_window:`LEFT ~x:left ~y;
            expander#show_region();
            expander#connect#toggled ~callback:(fun _ -> expander_toggled#call expander) |> ignore;
            expander
        | Some expander ->
            view#move_child ~child:expander#coerce ~x:left ~y;
            expander#regain start stop;
            expander
      in
      if List.exists (fun t -> t#get_oid = tag_invisible#get_oid) start#tags
      then expander#misc#hide()
      else expander#misc#show();

    method amend_nested_collapsed (expander : expander) =
      (* TODO more precise start *)
      if expander#is_expanded then begin
        let start, stop = expander#bounds() in
        Hashtbl.iter begin fun _ exp ->
          let s, _ = exp#bounds() in
          (* When the outer expander is expanded, all nested ones are also
             expanded, because the tag_highlight is removed everywhere, but
             the expander state remains "collapsed". *)
          if not exp#is_expanded && start#compare s < 0 && s#compare stop < 0
          then exp#hide_region();
        end expanders
      end

    method private invoke_merlin () =
      if self#is_changed_after_last_outline then begin
        merlin@@Merlin.outline begin fun (ol : Merlin_j.outline list) ->
          self#sync_outline_time();
          outline <- flatten 0 ol;
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
      if margin#is_pending then begin
        Printf.printf "pending\n%!" ;
        Gmisclib.Idle.add ~prio:300 page#view#draw_gutter;
        margin#clear_pending()
      end
    end |> ignore;
    margin#connect#expander_toggled ~callback:begin fun expander ->
      margin#amend_nested_collapsed expander;
      Gmisclib.Idle.add ~prio:300 begin fun () ->
        Printf.printf "draw_gutter\n%!" ;
        page#view#draw_gutter();
      end
    end |> ignore;
  with ex ->
    Printf.eprintf "File \"margin_fold.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace())

let init_editor editor =
  editor#connect#add_page ~callback:init_page |> ignore;
  (*editor#connect#remove_page ~callback:begin fun page ->
    ()
    end |> ignore*)

