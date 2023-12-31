open Margin
open Merlin_j
open GUtil

type fold = {
  is_collapsed : bool;
}

module Icons = struct
  let expander_open = "\u{f107}"
  let expander_closed = "\u{f105}"
end

class expander ~(view : Ocaml_text.view) ~tag_highlight ~tag_invisible ?packing () =
  let ebox = GBin.event_box ?packing () in
  let label = Gtk_util.label_icon Icons.expander_open ~packing:ebox#add in
  let buffer = view#buffer in
  object (self)
    inherit GObj.widget ebox#as_widget
    val mutable is_expanded = true
    val mutable start : GText.iter = buffer#get_iter `START
    val mutable stop : GText.iter = buffer#get_iter `START
    val expanded = new expanded()
    val collapsed = new collapsed()
    val toggled = new toggled()

    initializer
      ebox#misc#set_property "visible-window" (`BOOL false);
      ebox#event#connect#button_press ~callback:begin fun ev ->
        if is_expanded then self#collapse() else self#expand();
        false
      end |> ignore;
      ebox#event#connect#enter_notify ~callback:begin fun ev ->
        Printf.printf "-->%d,%d -- %d,%d\n%!" (start#line + 1) start#line_offset (stop#line + 1) stop#line_offset;
        if is_expanded then buffer#apply_tag tag_highlight ~start ~stop;
        false
      end |> ignore;
      ebox#event#connect#leave_notify ~callback:begin fun ev ->
        buffer#remove_tag tag_highlight ~start ~stop;
        false
      end |> ignore;

    method set_bounds istart istop =
      start <- istart;
      stop <- istop

    method expand () =
      let start = start#forward_line#set_line_offset 0 in
      buffer#remove_tag tag_invisible ~start ~stop;
      label#set_text Icons.expander_open;
      let was_collapsed = not is_expanded in
      is_expanded <- true;
      if was_collapsed then begin
        toggled#call true;
        expanded#call()
      end

    method collapse () =
      let iter = buffer#get_iter `INSERT in
      let start = start#forward_line#set_line_offset 0 in
      if iter#compare start >= 0 && iter#compare stop <= 0 then begin
        buffer#move_mark `INSERT ~where:start#backward_line#forward_to_line_end#backward_char
      end;
      buffer#apply_tag tag_invisible ~start ~stop;
      label#set_text Icons.expander_closed;
      let was_expanded = is_expanded in
      is_expanded <- false;
      if was_expanded then begin
        toggled#call false;
        collapsed#call();
      end

    method connect = new signals ~expanded ~collapsed ~toggled
  end

and expanded () = object inherit [unit] signal () end
and collapsed () = object inherit [unit] signal () end
and toggled () = object inherit [bool] signal () end
and signals ~expanded ~collapsed ~toggled =
  object
    inherit ml_signals [expanded#disconnect; collapsed#disconnect; toggled#disconnect]
    method expanded = expanded#connect ~after
    method collapsed = collapsed#connect ~after
    method toggled = toggled#connect ~after
  end


class margin_fold (view : Ocaml_text.view) =
  let size = 13 in
  let spacing = 5 in
  let buffer = view#obuffer in
  let tag_highlight = buffer#create_tag ~name:"fold-highlight" [ `PARAGRAPH_BACKGROUND "#505050" ] in
  let tag_invisible = buffer#create_tag ~name:"fold-invisible" [ `INVISIBLE true ] in
  let merlin func =
    let filename = match buffer#file with Some file -> file#filename | _ -> "" in
    let source_code = buffer#get_text () in
    func ~filename ~source_code
  in
  let rec flatten (ol : Merlin_j.outline list) =
    match ol with
    | [] -> []
    | hd :: tl -> hd :: (flatten (List.rev_append hd.ol_children tl))
  in
  object (self)
    inherit margin()
    val mutable last_hash = -1
    val mutable last_outline_time = 0.0
    val mutable timer_is_active = true
    val mutable outline = []
    val mutable expanders : (int, expander) Hashtbl.t = Hashtbl.create 7
    val expander_toggled = new expander_toggled()
    method index = 30
    method size = size

    method draw ~view ~top ~left ~height ~start ~stop =
      let buffer_start_line = start#line in
      let buffer_stop_line = stop#line in
      Hashtbl.iter (fun _ ex -> ex#misc#hide()) expanders;
      outline
      |> List.iter begin fun ol ->
        let fold_start_line = ol.ol_start.line - 1 in
        let is_folding_point =
          buffer_start_line <= fold_start_line && fold_start_line <= buffer_stop_line
          && (ol.ol_kind = "Method" || ol.ol_start.line <> ol.ol_stop.line)
        in
        if is_folding_point then self#draw_expander ol top left
      end

    method private draw_expander ol top left =
      Printf.printf "draw_expander %d:%d -- %d:%d\n%!"
        ol.ol_start.line ol.ol_start.col ol.ol_stop.line ol.ol_stop.col;
      let start = buffer#get_iter (`LINECHAR (ol.ol_start.line - 1, ol.ol_start.col))  in
      let stop = buffer#get_iter (`LINECHAR (ol.ol_stop.line - 1, ol.ol_stop.col)) in
      Printf.printf "%S: %d:%d - %d:%d\n%!"
        (buffer#get_text ~start ~stop:start#forward_to_line_end ())
        ol.ol_start.line ol.ol_start.col ol.ol_stop.line ol.ol_stop.col;
      let yl, _ = view#get_line_yrange start in
      let y = yl - top + view#pixels_above_lines in
      let expander = new expander ~tag_highlight ~tag_invisible ~view () in
      let key = buffer#get_text ~start ~stop () |> Hashtbl.hash in
      let expander =
        match Hashtbl.find_opt expanders key with
        | None ->
            Hashtbl.add expanders key expander;
            view#add_child_in_window ~child:expander#coerce ~which_window:`LEFT ~x:left ~y;
            expander#set_bounds start stop;
            expander#expand();
            expander#connect#toggled ~callback:expander_toggled#call |> ignore;
            expander
        | Some expander ->
            view#move_child ~child:expander#coerce ~x:left ~y;
            expander#set_bounds start stop;
            expander
      in
      if List.exists (fun t -> t#get_oid = tag_invisible#get_oid) start#tags
      then expander#misc#hide()
      else expander#misc#show();

    method private invoke_merlin () =
      merlin@@Merlin.outline begin fun (ol : Merlin_j.outline list) ->
        let hash = Hashtbl.hash ol in
        if hash <> last_hash then begin
          last_hash <- hash;
          outline <- flatten ol
        end;
      end;
      timer_is_active

    initializer
      self#invoke_merlin() |> ignore;
      GMain.Timeout.add ~ms:1000 ~callback:self#invoke_merlin |> ignore;

    method connect = new margin_signals ~expander_toggled
  end

and expander_toggled () = object inherit [bool] signal () end
and margin_signals ~expander_toggled =
  object
    inherit ml_signals [expander_toggled#disconnect]
    method expander_toggled = expander_toggled#connect ~after
  end

let init_page (page : Editor_page.page) =
  page#view#misc#connect#realize ~callback:begin fun _ ->
    try
      let margin = new margin_fold page#ocaml_view in
      page#view#margin#add (margin :> Margin.margin);
      margin#connect#expander_toggled ~callback:(fun _ ->
          Gmisclib.Idle.add ~prio:300 page#view#draw_gutter) |> ignore;
    with ex ->
      Printf.eprintf "File \"margin_fold.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  end |> ignore

let init_editor editor =
  Printexc.record_backtrace true;
  editor#connect#add_page ~callback:init_page |> ignore;
  (*editor#connect#remove_page ~callback:begin fun page ->
    ()
    end |> ignore*)

