open Merlin_j

let merlin (buffer : Ocaml_text.buffer) func =
  let filename = match buffer#file with Some file -> file#filename | _ -> "" in
  let source_code = buffer#get_text () in
  func ~filename ~source_code

type t = {
  view : Ocaml_text.view;
  tag : GText.tag;
  mutable timer_id : GMain.Timeout.id option;
  mutable prev_x : int;
  mutable prev_y : int;
  mutable current_area : (int * int * int * int) option;
  mutable is_active : bool;
  mutable is_idle : bool;
  mutable is_suspended : bool;
  mutable window : GWindow.window option;
  mutable merlin : (filename:string -> source_code:string -> unit) -> unit;
}

let delay_idle = 2000
let delay_work = 100

let (@<=) (left, top, right, bottom) (x, y) =
  left <= x && x <= right && top <= y && y <= bottom

let reset qi n =
  (*Printf.printf "RESET %s\n%!" n;*)
  qi.window |> Option.iter begin fun w ->
    Gmisclib.Idle.add w#destroy;
    qi.view#buffer#remove_tag qi.tag ~start:qi.view#buffer#start_iter ~stop:qi.view#buffer#end_iter;
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

let display qi start stop =
  let xstart, ystart, xstop, ystop = get_area qi start stop in
  qi.current_area <- Some (xstart, ystart, xstop, ystop);
  let open Preferences in
  let open Settings_j in
  let vbox = GPack.vbox ~spacing:2 () in
  let label_typ = GMisc.label ~xpad:5 ~ypad:5 ~xalign:0.0 ~yalign:0.0 ~packing:vbox#add () in
  let label_doc = GMisc.label ~xpad:5 ~ypad:5 ~xalign:0.0 ~yalign:0.0 ~line_wrap:true ~packing:vbox#add () in
  label_typ#set_use_markup true;
  label_doc#set_use_markup true;
  let x, y =
    let _, lh = qi.view#get_line_yrange start in
    let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
    let win = (match qi.view#get_window `WIDGET with None -> assert false | Some w -> w) in
    let px, py = Gdk.Window.get_pointer_location win in
    pX (*- px + xstart*), pY - py + ystart + lh
  in
  let win = Gtk_util.window_tooltip vbox#coerce ~fade:false ~x ~y ~show:false () in
  qi.window <- Some win;
  Gmisclib.Idle.add ~prio:300 begin fun () ->
    win#show();
    let r = vbox#misc#allocation in
    if r.Gtk.height > 200 then begin
      let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC () in
      let vp = GBin.viewport ~packing:sw#add () in
      sw#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
      vp#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
      vbox#misc#reparent vp#coerce;
      win#destroy();
      let win = Gtk_util.window_tooltip sw#coerce ~fade:false ~x ~y ~width:700 ~height:300 ~show:false () in
      qi.window <- Some win;
      win#present()
    end else win#present()
  end;
  qi.view#buffer#apply_tag qi.tag ~start ~stop;
  label_typ, label_doc

let build_content qi (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  (* TODO .... *)
  let contains_type_vars = String.contains entry.Merlin_t.te_type '\'' in
  let is_module = String.starts_with ~prefix:"(" entry.te_type in
  Printf.sprintf "%s%s" entry.te_type
    (if contains_type_vars || is_module
     then entry2 |> Option.fold ~none:"" ~some:(fun (x : type_enclosing_value) -> "\n" ^ x.te_type)
     else "")

let process_type qi position (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  reset qi "open";
  let typ = build_content qi entry entry2 in
  let markup = Markup.type_info typ in
  let start = qi.view#obuffer#get_iter (`LINECHAR (entry.te_start.line - 1, entry.te_start.col)) in
  let stop = qi.view#obuffer#get_iter (`LINECHAR (entry.te_stop.line - 1, entry.te_stop.col)) in
  let label_typ, label_doc = display qi start stop in
  (*<span size='small' font_family='%s'>%s</span>*)
  label_typ#set_label markup;
  let markup_odoc = new Markup.odoc() in
  qi.merlin@@Merlin.document ~position begin fun doc ->
    let markup = markup_odoc#convert doc in
    label_doc#set_label markup;
  end

let invoke_merlin qi iter =
  let position = iter#line + 1, iter#line_index in
  qi.merlin@@Merlin.type_enclosing ~position begin fun types ->
    match types with
    | [] -> reset qi "no-type"
    | fst :: snd :: _ -> process_type qi position fst (Some snd)
    | fst :: _ -> process_type qi position fst None
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
          wx <= px && px <= wx + r.Gtk.width && wy <= py && py <= wy + r.Gtk.height
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

let rec restart qi ~idle =
  stop qi;
  qi.is_idle <- idle;
  start qi

and start qi =
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
                if x >= 0 && y >= 0 && x <= r.Gtk.width && y <= r.Gtk.height then begin
                  if qi.is_idle then begin
                    restart qi ~idle:false;
                    false
                  end else if qi.is_suspended then qi.is_active else
                    work qi x y root_window
                end else begin
                  if qi.is_idle then qi.is_active else begin
                    restart qi ~idle:true;
                    false
                  end
                end
              with ex ->
                Printf.eprintf "File \"quick_info.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
                qi.is_active
            end
          end
  end

let create (view : Ocaml_text.view) =
  let open Preferences in
  let bg_color = ?? (Preferences.preferences#get.editor_bg_color_popup) in
  let qi =
    {
      view = view;
      tag = view#buffer#create_tag ~name:"quick-info" [`BACKGROUND bg_color];
      timer_id = None;
      prev_x = 0;
      prev_y = 0;
      current_area = None;
      is_active = false;
      is_idle = false;
      is_suspended = false;
      window = None;
      merlin = merlin view#obuffer;
    }
  in
  view#event#connect#key_press ~callback:begin fun _ ->
    if not qi.is_suspended then begin
      reset qi "key-press";
      restart qi ~idle:true;
      qi.is_suspended <- true;
    end;
    false
  end |> ignore;
  view#event#connect#motion_notify ~callback:begin fun _ ->
    qi.is_suspended <- false;
    false
  end |> ignore;
  view#event#connect#button_press ~callback:begin fun _ ->
    reset qi "button-press";
    restart qi ~idle:true;
    false
  end |> ignore;
  qi
