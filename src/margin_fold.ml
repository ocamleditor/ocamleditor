open Margin
open Merlin_j

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
    val mutable bounds = 0, 0

    initializer
      ebox#misc#set_property "visible-window" (`BOOL false);
      ebox#event#connect#button_press ~callback:begin fun ev ->
        if is_expanded then self#collapse() else self#expand();
        false
      end |> ignore;
      ebox#event#connect#enter_notify ~callback:begin fun ev ->
        if is_expanded then begin
          let lstart, lstop = bounds in
          let start = buffer#get_iter (`LINECHAR (lstart - 1, 0)) in
          let stop = buffer#get_iter (`LINECHAR (lstop - 1, 0)) in
          buffer#apply_tag tag_highlight ~start ~stop;
        end;
        false
      end |> ignore;
      ebox#event#connect#leave_notify ~callback:begin fun ev ->
        let lstart, lstop = bounds in
        let start = buffer#get_iter (`LINECHAR (lstart - 1, 0)) in
        let stop = buffer#get_iter (`LINECHAR (lstop - 1, 0)) in
        buffer#remove_tag tag_highlight ~start ~stop;
        false
      end |> ignore;

    method set_bounds start stop = bounds <- start, stop

    method expand () =
      let lstart, lstop = bounds in
      let start = buffer#get_iter (`LINECHAR (lstart, 0)) in
      let stop = buffer#get_iter (`LINECHAR (lstop - 1, 0)) in
      buffer#remove_tag tag_invisible ~start ~stop;
      (*      let m1 = `MARK (buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Code_folding.fold_offset1")*) start) in
              let m2 = `MARK (buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Code_folding.fold_offset2")*) stop) in*)
      label#set_text Icons.expander_open;
      is_expanded <- true

    method collapse () =
      let lstart, lstop = bounds in
      let start = buffer#get_iter (`LINECHAR (lstart, 0)) in
      let stop = buffer#get_iter (`LINECHAR (lstop - 1, 0)) in
      buffer#apply_tag tag_invisible ~start ~stop;
      label#set_text Icons.expander_closed;
      is_expanded <- false

    method reset () = bounds <- 0, 0
  end

class margin_fold (view : Ocaml_text.view) =
  let size = 13 in
  let spacing = 5 in
  let buffer = view#obuffer in
  let tag_highlight = view#buffer#create_tag [ `PARAGRAPH_BACKGROUND "#505050" ] in
  let tag_invisible = view#buffer#create_tag [ `INVISIBLE true ] in
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
    val mutable timer_is_active = true
    val mutable outline = []
    val labels = Line_num_labl.create()
    method index = 30
    method size = size

    method private place_expanders ol top left =
      let iter = buffer#get_iter (`LINECHAR (ol.ol_start.line - 1, 0)) in
      let yl, hl = view#get_line_yrange iter in
      let y = yl - top + view#pixels_above_lines in
      let expander =
        match Line_num_labl.get labels with
        | Some expander ->
            view#move_child ~child:expander#coerce ~x:left ~y;
            expander
        | _ ->
            let expander = new expander ~tag_highlight ~tag_invisible ~view () in
            view#add_child_in_window ~child:expander#coerce ~which_window:`LEFT ~x:left ~y;
            expander
      in
      Line_num_labl.lock labels (top, expander);
      expander#set_bounds ol.ol_start.line (ol.ol_stop.line + 1);
      expander#expand();
      expander#misc#show();

    method draw ~view ~top ~left ~height ~start ~stop =
      Line_num_labl.reset labels;
      let start_line = start#line + 1 in
      let stop_line = stop#line + 1 in
      outline
      |> List.iter begin fun ol ->
        (*Printf.printf "%s %d-%d:%d\n%!" ol.ol_kind ol.ol_start.line ol.ol_stop.line ol.ol_stop.col;*)
        if start_line <= ol.ol_start.line && ol.ol_start.line <= stop_line
        then self#place_expanders ol top left
      end

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

  end

let init_page (page : Editor_page.page) =
  page#view#misc#connect#realize ~callback:begin fun _ ->
    try
      let margin = new margin_fold page#ocaml_view in
      page#view#margin#add (margin :> Margin.margin);
    with ex ->
      Printf.eprintf "File \"margin_fold.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  end |> ignore

let init_editor editor =
  Printexc.record_backtrace true;
  editor#connect#add_page ~callback:init_page |> ignore;
  editor#connect#remove_page ~callback:begin fun page ->
    ()
  end |> ignore

