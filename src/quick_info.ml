open Merlin_j

let merlin (buffer : Ocaml_text.buffer) func =
  let filename = match buffer#file with Some file -> file#filename | _ -> "" in
  let source_code = buffer#get_text () in
  func ~filename ~source_code

type t = {
  view : Ocaml_text.view;
  mutable timer_id : GMain.Timeout.id option;
  mutable prev_x : int;
  mutable prev_y : int;
  mutable current_area : (int * int * int * int) option;
  mutable is_active : bool;
  mutable is_idle : bool;
  mutable window : GWindow.window option;
}

let delay_idle = 2000
let delay_work = 100

let (@<=) (left, top, right, bottom) (x, y) =
  left <= x && x <= right && top <= y && y <= bottom

let reset qi n =
  Printf.printf "RESET %s\n%!" n;
  qi.window |> Option.iter begin fun w ->
    Gmisclib.Idle.add w#destroy;
    qi.current_area <- None;
    qi.window <- None
  end

let stop qi =
  if qi.is_active then begin
    qi.is_active <- false;
    qi.timer_id |> Option.iter GMain.Timeout.remove;
    reset qi "stop";
    Printf.printf "Quick_info timer STOP \n%!"
  end else
    Printf.printf "Quick_info already STOPPED \n%!"

let (!=) (p1 : Merlin_j.pos) (p2 : Merlin_j.pos) =
  p1.col <> p2.col || p1.line <> p2.line

let get_area qi start stop =
  let rstart = qi.view#get_iter_location start in
  let rstop = qi.view#get_iter_location stop in
  let xstart, ystart = qi.view#buffer_to_window_coords ~tag:`WIDGET
      ~x:(Gdk.Rectangle.x rstart) ~y:(Gdk.Rectangle.y rstart) in
  let xstop, ystop = qi.view#buffer_to_window_coords ~tag:`WIDGET
      ~x:(Gdk.Rectangle.x rstop) ~y:(Gdk.Rectangle.y rstop + Gdk.Rectangle.height rstop) in
  xstart, ystart, xstop, ystop

let display qi markup start stop =
  let xstart, ystart, xstop, ystop = get_area qi start stop in
  qi.current_area <- Some (xstart, ystart, xstop, ystop);
  let open Preferences in
  let label = GMisc.label ~xpad:5 ~ypad:5 ~xalign:0.0 ~yalign:0.0 ~markup () in
  let x, y =
    let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
    let win = (match qi.view#get_window `WIDGET with None -> assert false | Some w -> w) in
    let px, py = Gdk.Window.get_pointer_location win in
    pX - px + xstart, pY - py + ystop
  in
  let win = Gtk_util.window_tooltip label#coerce ~fade:false ~x ~y ~show:false () in
  qi.window <- Some win;
  Gmisclib.Idle.add ~prio:300 begin fun () ->
    win#show();
    let r = label#misc#allocation in
    if r.height > 200 then begin
      let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC () in
      let vp = GBin.viewport ~packing:sw#add () in
      sw#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
      vp#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
      label#misc#reparent vp#coerce;
      win#destroy();
      let win = Gtk_util.window_tooltip sw#coerce ~fade:false ~x ~y ~width:700 ~height:300 ~show:false () in
      qi.window <- Some win;
      win#present()
    end else win#present()
  end

let build_content qi (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  let contains_type_vars = String.contains entry.typ '\'' in
  let is_module = String.starts_with ~prefix:"(" entry.typ in
  let text =
    Printf.sprintf "%s%s" entry.typ
      (if contains_type_vars || is_module
       then entry2 |> Option.fold ~none:"" ~some:(fun (x : type_enclosing_value) -> "\n" ^ x.typ)
       else "")
  in
  Print_type.markup2 text

let process_type qi (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  reset qi "open";
  let markup = build_content qi entry entry2 in
  let start = qi.view#obuffer#get_iter (`LINECHAR (entry.start.line - 1, entry.start.col)) in
  let stop = qi.view#obuffer#get_iter (`LINECHAR (entry.stop.line - 1, entry.stop.col)) in
  display qi markup start stop

let invoke_merlin qi iter =
  let position = iter#line + 1, iter#line_index in
  (merlin qi.view#obuffer)@@Merlin.type_enclosing ~position begin fun types ->
    match types with
    | [] -> reset qi "no-type"
    | fst :: snd :: _ -> process_type qi fst (Some snd)
    | fst :: _ -> process_type qi fst None
  end

let is_iter_in_comment (buffer : Ocaml_text.buffer) iter =
  Comments.enclosing
    (Comments.scan (Glib.Convert.convert_with_fallback ~fallback:"" ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset
                      (buffer#get_text ()))) iter#offset

let get_typeable_iter_at_coords qi x y =
  let bx, by = qi.view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
  let iter = qi.view#get_iter_at_location ~x:bx ~y:by in
  if iter#ends_line
  || Glib.Unichar.isspace iter#char
  || (match is_iter_in_comment qi.view#obuffer iter with None -> false | _ -> true)
  then None else Some iter

let process_location ?(do_reset=false) ?(invoke=true) qi x y =
  if do_reset then begin
    qi.prev_x <- x;
    qi.prev_y <- y;
  end;
  match qi.current_area with
  | Some area when area @<= (x, y) -> ()
  | _ ->
      begin
        match get_typeable_iter_at_coords qi x y with
        | Some iter when invoke -> invoke_merlin qi iter
        | Some _ -> ()
        | _ when do_reset -> reset qi "not-typeable"
        | _ -> ()
      end

let work qi x y root_window =
  if x = qi.prev_x && y = qi.prev_y then begin
    match qi.window with
    | Some _ -> ()
    | _ -> process_location qi x y
  end else begin
    match qi.window with
    | Some win ->
        let r = win#misc#allocation in
        let wx, wy = Gdk.Window.get_position win#misc#window in
        let px, py = Gdk.Window.get_pointer_location root_window in
        let is_mouse_over =
          wx <= px && px <= wx + r.width && wy <= py && py <= wy + r.height
        in
        if is_mouse_over then begin
          win#event#connect#button_press ~callback:begin fun _ ->
            qi.is_active <- false;
            (*win#misc#modify_bg [`NORMAL, `COLOR (win#misc#style#fg `SELECTED)];*)
            win#misc#modify_bg [`NORMAL, `COLOR (Preferences.editor_tag_bg_color "selection")];
            true
          end |> ignore
        end else
          process_location ~do_reset:true qi x y
    | _ ->
        process_location ~do_reset:true ~invoke:false qi x y
  end;
  qi.is_active

let rec start qi =
  if not qi.is_active then begin
    Printexc.record_backtrace true;
    Printf.printf "Quick_info timer START %s \n%!" (if qi.is_idle then "IDLE" else "WORKING");
    qi.is_active <- true;
    let root_window = Gdk.Window.root_parent () in
    match qi.view#get_window `WIDGET with
    | None -> assert false
    | Some view_window ->
        let ms = if qi.is_idle then delay_idle else delay_work in
        qi.timer_id <- Some begin
            GMain.Timeout.add ~ms ~callback:begin fun () ->
              try
                let x, y = Gdk.Window.get_pointer_location view_window in
                let r = qi.view#misc#allocation in
                if x >= 0 && y >= 0 && x <= r.width && y <= r.height then begin
                  if qi.is_idle then begin
                    stop qi;
                    qi.is_idle <- false;
                    start qi;
                    false
                  end else
                    work qi x y root_window
                end else begin
                  if qi.is_idle then qi.is_active else begin
                    stop qi;
                    qi.is_idle <- true;
                    start qi;
                    false
                  end
                end;
              with ex ->
                Printf.eprintf "File \"quick_info.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
                qi.is_active
            end
          end
  end

let create (view : Ocaml_text.view) =
  let qi =
    {
      view = view;
      timer_id = None;
      prev_x = 0;
      prev_y = 0;
      current_area = None;
      is_active = false;
      is_idle = false;
      window = None
    }
  in
  view#event#connect#button_press ~callback:begin fun _ ->
    start qi;
    false
  end |> ignore;
  qi
