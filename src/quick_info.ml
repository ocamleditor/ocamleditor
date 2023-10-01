open Merlin_j

let merlin (buffer : Text.buffer) func =
  let filename = match buffer#file with Some file -> file#filename | _ -> "" in
  let source_code = buffer#get_text () in
  func ~filename ~source_code

type t = {
  view : Ocaml_text.view;
  mutable prev_x : int;
  mutable prev_y : int;
  mutable prev_start : pos;
  mutable prev_stop : pos;
  mutable is_active : bool;
  mutable window : GWindow.window option;
}

let reset qi n =
  Printf.printf "RESET %s\n%!" n;
  qi.prev_start <- { line = 0; col = 0 };
  qi.prev_stop <- { line = 0; col = 0 };
  match qi.window with
  | Some w ->
      Gmisclib.Idle.add w#destroy;
      qi.window <- None;
  | _ -> qi.window <- None

let stop qi =
  if qi.is_active then begin
    reset qi "stop";
    qi.is_active <- false;
    Printf.printf "Quick_info timer STOP \n%!"
  end else
    Printf.printf "Quick_info already STOPPED \n%!"

let (==) (p1 : Merlin_j.pos) (p2 : Merlin_j.pos) =
  p1.col = p2.col && p1.line = p2.line

let process_type qi (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  if qi.prev_start == entry.start && qi.prev_stop == entry.stop then begin
    ()
  end else begin
    Printf.printf "WINDOW %d,%d - %d,%d\n%!" entry.start.line entry.start.col entry.stop.line entry.stop.col;
    reset qi "open";
    qi.prev_start <- entry.start;
    qi.prev_stop <- entry.stop;
    let contains_type_vars = String.contains entry.typ '\'' in
    let is_module = String.starts_with ~prefix:"(" entry.typ in
    let text =
      Printf.sprintf "%s%s" entry.typ
        (if contains_type_vars || is_module then entry2 |> Option.fold ~none:"" ~some:(fun (x : type_enclosing_value) -> "\n" ^ x.typ) else "")
    in
    let markup = Print_type.markup2 text in
    let label = GMisc.label ~xpad:5 ~ypad:5 ~xalign:0.0 ~yalign:0.0 ~markup () in
    let x, y =
      match qi.view#get_window `WIDGET with
      | Some _ -> Gdk.Window.get_pointer_location (Gdk.Window.root_parent ())
      | _ -> assert false
    in
    let win = Gtk_util.window_tooltip label#coerce ~fade:false ~x ~y ~kind:`TOPLEVEL ~type_hint:`NORMAL ~show:false () in
    qi.window <- Some win;
    win#set_opacity 0.0;
    Gmisclib.Idle.add ~prio:200 begin fun () ->
      win#show();
      let r = label#misc#allocation in
      if r.height > 200 then begin
        let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC () in
        let vp = GBin.viewport ~packing:sw#add () in
        label#misc#reparent vp#coerce;
        win#destroy();
        let win = Gtk_util.window_tooltip sw#coerce ~fade:false ~x ~y ~width:700 ~height:300 ~kind:`TOPLEVEL ~type_hint:`NORMAL ~show:false () in
        qi.window <- Some win;
        win#present()
      end else begin
        win#present();
      end
    end
  end

let invoke_merlin qi iter =
  let (line, col) as position = iter#line + 1, iter#line_index in
  (merlin qi.view#obuffer#as_text_buffer)@@Merlin.type_enclosing ~position begin fun types ->
    match types with
    | [] -> reset qi "no-type"
    | fst :: snd :: _ -> process_type qi fst (Some snd)
    | fst :: _ -> process_type qi fst None
  end

let get_valid_iter qi x y =
  let bx, by = qi.view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
  let iter = qi.view#get_iter_at_location ~x:bx ~y:by in
  if iter#ends_line
  || Glib.Unichar.isspace iter#char
  || begin
    match Comments.enclosing
            (Comments.scan (Glib.Convert.convert_with_fallback ~fallback:"" ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset
                              (qi.view#buffer#get_text ()))) iter#offset
    with None -> false | _ -> true;
  end
  then false, iter else true, iter

let start qi =
  if not qi.is_active then begin
    Printexc.record_backtrace true;
    Printf.printf "Quick_info timer START \n%!" ;
    qi.is_active <- true;
    GMain.Timeout.add ~ms:200 ~callback:begin fun () ->
      begin
        try
          let x, y =
            match qi.view#get_window `WIDGET with
            | Some window -> Gdk.Window.get_pointer_location window
            | _ -> assert false
          in
          if x = qi.prev_x && y = qi.prev_y then begin
            match qi.window with
            | Some _ -> ()
            | _ ->
                begin
                  match get_valid_iter qi x y with
                  | true, iter -> invoke_merlin qi iter
                  | _ -> ()
                end
          end else begin
            match qi.window with
            | Some win ->
                let r = win#misc#allocation in
                let wx, wy = Gdk.Window.get_position win#misc#window in
                let px, py = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
                let is_hover =
                  px >= wx && px <= wx + r.width && py >= wy - 13 && py <= wy + r.height
                in
                if is_hover then begin
                  win#event#connect#button_press ~callback:begin fun _ ->
                    qi.is_active <- false;
                    (*win#misc#modify_bg [`NORMAL, `COLOR (win#misc#style#fg `SELECTED)];*)
                    win#misc#modify_bg [`NORMAL, `COLOR (Preferences.editor_tag_bg_color "selection")];
                    true
                  end |> ignore;
                end else begin
                  qi.prev_x <- x;
                  qi.prev_y <- y;
                  begin
                    match get_valid_iter qi x y with
                    | true, iter -> invoke_merlin qi iter
                    | false, iter ->
                        Printf.printf "ITER %S\n%!" (Glib.Utf8.from_unichar iter#char);
                        reset qi "move-out"
                  end
                end
            | _ ->
                begin
                  qi.prev_x <- x;
                  qi.prev_y <- y;
                  begin
                    match get_valid_iter qi x y with
                    | true, iter -> invoke_merlin qi iter
                    | false, iter ->
                        Printf.printf "ITER %S\n%!" (Glib.Utf8.from_unichar iter#char);
                        reset qi "move-out"
                  end
                end;
          end;
        with ex -> Printf.eprintf "File \"quick_info.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      end;
      qi.is_active
    end
  end |> ignore

let create (view : Ocaml_text.view) =
  let qi =
    {
      view = view;
      prev_x = 0;
      prev_y = 0;
      prev_start = { line = 0; col = 0 };
      prev_stop = { line = 0; col = 0 };
      is_active = false;
      window = None
    }
  in
  view#event#connect#button_press ~callback:begin fun _ ->
    start qi;
    false
  end |> ignore;
  qi
