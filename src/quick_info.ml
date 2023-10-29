open Merlin_j

module Log = Common.Log.Make(struct let prefix = "QUICK-INFO" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

let merlin (buffer : Ocaml_text.buffer) func =
  let filename = match buffer#file with Some file -> file#filename | _ -> "" in
  let source_code = buffer#get_text () in
  func ~filename ~source_code

(** The type of quick info. *)
type t = {
  view : Ocaml_text.view;
  filename : string;
  tag : GText.tag;
  mutable timer_id : GMain.Timeout.id option;
  mutable prev_x : int;
  mutable prev_y : int;
  mutable current_area : (int * int * int * int) option;
  (** It is the area of the editor view that contains the expression for which
      quick info is currently shown. *)

  mutable is_active : bool;
  (** Indicates whether the timer is running. [start] and [stop] change this flag. *)

  mutable is_idle : bool;
  (** When the timer is idle it runs at low speed and does nothing other than
      check whether the the mouse moves within the editor view and, in this case,
      restarts in working (i.e. not idle) mode. *)

  mutable is_suspended : bool;
  (** When timer is suspended it runs at working speed and does nothing.
      The timer is suspended on key-press event in order to prevent quick info
      popups from appearing as you type.
      It can be reactivated by pressing Esc or the mouse button, or by moving
      the mouse or restarting the timer. *)

  mutable show_at : (int * int) option;
  (** Contains the location to display quick information when you want to
      override the default location, which is the mouse pointer. *)

  mutable window : GWindow.window option;
  mutable merlin : (filename:string -> source_code:string -> unit) -> unit;
}

let delay_idle = 2000
let delay_work = 100

let (@<=) (left, top, right, bottom) (x, y) =
  left <= x && x <= right && top <= y && y <= bottom

let suspend qi =
  if not qi.is_suspended then Log.println `INFO "Suspended  %s" qi.filename;
  qi.is_suspended <- true

let resume qi =
  if qi.is_suspended then Log.println `INFO "Resumed %s%!" qi.filename;
  qi.is_suspended <- false

(** Hides the quick info popup and the expression highlighting. *)
let hide qi n =
  (*Log.println `DEBUG "HIDE %s\n%!" n;*)
  qi.view#buffer#remove_tag qi.tag ~start:qi.view#buffer#start_iter ~stop:qi.view#buffer#end_iter;
  qi.current_area <- None;
  qi.window |> Option.iter begin fun w ->
    Gmisclib.Idle.add w#destroy;
    qi.window <- None
  end

(** Optionally hides quick info (default is hide), stops the timer and clears
    its status. *)
let stop ?(do_hide=true) qi =
  if do_hide then hide qi "stop";
  if qi.is_active then begin
    qi.is_active <- false;
    resume qi;
    qi.timer_id |> Option.iter GMain.Timeout.remove;
    Log.println `INFO "STOP (%s)%!" qi.filename
  end else
    Log.println `INFO "already STOPPED (%s)%!" qi.filename

(** Stops the timer to clear the quick info status but does not hide quick info
    on the screen. *)
let pin_window qi (win : GWindow.window) =
  win#event#connect#button_press ~callback:begin fun _ ->
    stop ~do_hide:false qi;
    win#misc#modify_bg [`NORMAL, `COLOR (Preferences.editor_tag_bg_color "selection")];
    true
  end |> ignore

let (!=) (p1 : Merlin_j.pos) (p2 : Merlin_j.pos) =
  p1.col <> p2.col || p1.line <> p2.line

(** Returns the left, top, right and bottom bounds of the expression area, in
    window coordinates. *)
let get_area qi start stop =
  let rstart = qi.view#get_iter_location start in
  let rstop = qi.view#get_iter_location stop in
  let xstart, ystart = qi.view#buffer_to_window_coords ~tag:`WIDGET
      ~x:(Gdk.Rectangle.x rstart) ~y:(Gdk.Rectangle.y rstart) in
  let xstop, ystop = qi.view#buffer_to_window_coords ~tag:`WIDGET
      ~x:(Gdk.Rectangle.x rstop) ~y:(Gdk.Rectangle.y rstop + Gdk.Rectangle.height rstop) in
  xstart, ystart, xstop, ystop

(** Displays the quick info popup window.  *)
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
    let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
    let win = (match qi.view#get_window `WIDGET with None -> assert false | Some w -> w) in
    let px, py = Gdk.Window.get_pointer_location win in
    match qi.show_at with
    | Some (x, y) ->
        qi.show_at <- None;
        pX - px + x, pY - py + y
    | _ ->
        let ly, lh = qi.view#get_line_yrange start in
        pX (*- px + xstart*), pY - py + ystart + lh
  in
  (* TODO Can this be optimized? *)
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
    end else win#present();
    qi.window |> Option.iter (pin_window qi);
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
  hide qi "open";
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
    | [] -> hide qi "no-type"
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

let process_location ?(invoke=true) qi x y =
  qi.prev_x <- x;
  qi.prev_y <- y;
  match qi.current_area with
  | Some area when area @<= (x, y) -> ()
  | _ ->
      begin
        match get_typeable_iter_at_coords qi x y with
        | Some iter when invoke -> invoke_merlin qi iter
        | Some _ -> ()
        | _ -> hide qi "not-typeable"
      end

(** Displays quick info about the expression at the specified iter.
    Stops the timer to avoid interference with the mouse movements.
    The timer will be restarted by pressing Esc or the mouse button. *)
let at_iter qi iter =
  stop qi;
  let rect = qi.view#get_iter_location iter in
  let x = Gdk.Rectangle.x rect in
  let y = Gdk.Rectangle.y rect in
  let x, y = qi.view#buffer_to_window_coords ~x ~y ~tag:`WIDGET in
  qi.show_at <- Some (x, y + Gdk.Rectangle.height rect);
  process_location ~invoke:true qi x y

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
        if is_mouse_over then () else process_location qi x y
    | _ ->
        process_location ~invoke:false qi x y
  end;
  qi.is_active

(** Restart the timer, possibly in idle mode. *)
let rec restart qi ~idle =
  stop qi;
  qi.is_idle <- idle;
  start qi

and start qi =
  if
    not qi.is_active &&
    Preferences.preferences#get.Settings_j.editor_quick_info_enabled &&
    qi.view#misc#get_flag `VISIBLE
  then begin
    Printexc.record_backtrace true;
    Log.println `INFO "START %s (%s)%!" (if qi.is_idle then "IDLE" else "WORKING") qi.filename;
    qi.is_active <- true;
    let root_window = Gdk.Window.root_parent () in
    match qi.view#get_window `WIDGET with
    | None -> ()
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
                  end else if qi.is_suspended then qi.is_active
                  else
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
  let filename = match view#obuffer#file with Some file -> file#filename | _ -> "" in
  let qi =
    {
      view = view;
      filename = filename;
      tag = view#buffer#create_tag ~name:"quick-info" [`BACKGROUND bg_color];
      timer_id = None;
      prev_x = 0;
      prev_y = 0;
      current_area = None;
      is_active = false;
      is_idle = false;
      is_suspended = false;
      window = None;
      show_at = None;
      merlin = merlin view#obuffer;
    }
  in
  view#event#connect#key_press ~callback:begin fun ev ->
    if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then begin
      hide qi "Escape";
      restart qi ~idle:false;
    end else begin
      if not qi.is_suspended then begin
        hide qi "key-press";
        suspend qi;
      end;
    end;
    false
  end |> ignore;
  view#event#connect#motion_notify ~callback:begin fun _ ->
    if qi.is_suspended then resume qi;
    false
  end |> ignore;
  view#event#connect#button_press ~callback:begin fun _ ->
    hide qi "button-press";
    restart qi ~idle:true; (* restart idle to avoid redisplay popup *)
    false
  end |> ignore;
  view#event#connect#focus_in ~callback:begin fun _ ->
    Log.println `DEBUG "focus_int %s" qi.filename;
    start qi;
    false
  end |> ignore;
  view#event#connect#focus_out ~callback:begin fun _ ->
    Log.println `DEBUG "focus_out %s" qi.filename;
    stop qi;
    false
  end |> ignore;
  Preferences.preferences#connect#changed ~callback:begin fun pref ->
    match pref.Settings_j.editor_quick_info_enabled with
    | true -> start qi
    | false -> stop qi
  end |> ignore;
  qi
