open Merlin_j
open Printf

module Log = Common.Log.Make(struct let prefix = "QUICK-INFO" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

let merlin (buffer : Ocaml_text.buffer) func =
  let filename = match buffer#file with Some file -> file#filename | _ -> "" in
  let source_code = buffer#get_text () in
  func ~filename ~source_code

module SignalId = struct
  let create () = ref None
  let save cell id = cell := Some id
  let disconnect cell widget =
    match !cell with
    | Some id -> cell := None; GtkSignal.disconnect widget id
    | None -> ()
end

module Lock = struct
  let index = Mutex.create()
  let wininfo = Mutex.create()
  let mutex mx f x =
    Mutex.lock mx;
    try
      let res = f x in
      Mutex.unlock mx;
      res
    with ex ->
      Mutex.unlock mx;
      raise ex
end

(** [!!mutex f x] applies [f] to [x] inside a critical section locked by
    [mutex] and returns its result. *)
let (!!) mx f x = Lock.mutex mx f x

let (<<-) x f = f x and (->>) f x = fun mx -> Lock.mutex mx f x

let index = ref 10_000
let new_index () =
  !!Lock.index begin fun () ->
    index := !index + 1;
    !index
  end ()

(** The type of quick info. *)
type t = {
  mutable markup_odoc : Markup.odoc;
  view : Ocaml_text.view;
  filename : string;
  tag : GText.tag;
  mutable is_active : bool;
  mutable current_x : int;
  mutable current_y : int;
  mutable show_at : (int * int) option;
  (** Contains the location to display quick information when you want to
      override the default location, which is the mouse pointer. *)

  mutable windows : wininfo list;
  mutable merlin : (filename:string -> source_code:string -> unit) -> unit;
}

and wininfo = {
  window : GWindow.window;
  mutable area : ((int * int * int * int) * (GText.iter * GText.iter)) option;
  (** It is the area of the editor that contains the expression for which quick
      info is currently shown. *)

  mutable is_pinned : bool;
  mutable index : int;
}

let (@<=) (left, top, right, bottom) (x, y) =
  left <= x && x <= right && top <= y && y <= bottom

(** Returns the last open quick info window. This function is not thread safe. *)
let get_current_window_unsafe qi =
  match qi.windows with
  | wininfo :: _ -> Some wininfo
  | _ -> None

(** Returns the last open quick info window. *)
let get_current_window qi = !!Lock.wininfo get_current_window_unsafe qi

let remove_highlight qi wi =
  match wi.area with
  | Some (_, (start, stop)) ->
      qi.view#buffer#remove_tag qi.tag ~start ~stop;
      wi.area <- None
  | _ -> ()

let add_wininfo qi =
  !!Lock.wininfo (fun wi -> qi.windows <- wi :: qi.windows)

(** Starts a timer that closes the specified quick-info window and removes
    expression highlighting in the editor. An exception is the case
    in which the window is pinned when the timer expires.
    Delayed close allows the user to move the mouse pointer over the window
    to pin it before it closes. *)
let remove_wininfo qi wi =
  GMain.Timeout.add ~ms:200 ~callback:begin fun () ->
    if not wi.is_pinned then begin
      !!Lock.wininfo begin fun () ->
        if not wi.is_pinned then remove_highlight qi wi;
        qi.windows <- qi.windows |> List.filter (fun x -> x.window#misc#get_oid <> wi.window#misc#get_oid);
      end ();
      wi.window#destroy();
    end;
    false
  end |> ignore

(** [is_pinned qi] is [true] iff the last open quick info window is pinned. *)
let is_pinned qi =
  match get_current_window qi with Some wi -> wi.is_pinned | _ -> false

(** Immediately hides the last open quick-info window and all the others
    previously open and in timed closure. The pinned window is an exception
    and is not hidden. *)
let hide qi =
  !!Lock.wininfo begin fun () ->
    qi.windows |> List.iter begin fun wi ->
      if not wi.is_pinned then begin
        remove_highlight qi wi;
        wi.window#misc#hide()
      end
    end;
  end ()

let unpin qi =
  !!Lock.wininfo (List.iter (fun w -> w.is_pinned <- false)) qi.windows;
  hide qi

(** Closes the last quick information opened and all other timed closing windows. *)
let close qi cause =
  (*if cause <> "" then Log.println `DEBUG "CLOSE %s" cause;*)
  qi.windows |> List.iter (remove_wininfo qi)

let make_pinnable wininfo =
  wininfo.window#event#connect#button_press ~callback:begin fun _ ->
    wininfo.is_pinned <- true;
    wininfo.window#misc#modify_bg [`NORMAL, `COLOR (Preferences.editor_tag_bg_color "selection")];
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
  let open Preferences in
  let open Settings_j in
  let vbox = GPack.vbox ~spacing:2 () in
  (*let label_index = GMisc.label ~xpad:5 ~ypad:5 ~xalign:0.0 ~yalign:0.0 ~packing:vbox#add () in*)
  let label_typ = GMisc.label ~xpad:5 ~ypad:5 ~xalign:0.0 ~yalign:0.0 ~packing:vbox#add () in
  let label_doc = GMisc.label ~xpad:5 ~ypad:5 ~xalign:0.0 ~yalign:0.0 ~line_wrap:true ~packing:vbox#add () in
  label_typ#set_use_markup true;
  label_doc#set_use_markup true;
  label_doc#misc#modify_font_by_name preferences#get.editor_completion_font;
  kprintf label_typ#misc#modify_font_by_name "%s %s" preferences#get.editor_base_font qi.markup_odoc#code_font_size;
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
  let area = Some ((xstart, ystart, xstop, ystop), (start, stop)) in
  let window = Gtk_util.window_tooltip vbox#coerce ~fade:false ~x ~y ~show:false () in
  let wininfo = {
    window;
    area;
    is_pinned = false;
    index = new_index();
  } in
  add_wininfo qi wininfo;
  (*kprintf label_index#set_label "%d" wininfo.index;*)
  make_pinnable wininfo;
  Gmisclib.Idle.add begin fun () ->
    window#present();
    let r = vbox#misc#allocation in
    if r.Gtk.height > 200 then begin
      let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC () in
      let vp = GBin.viewport ~packing:sw#add () in
      sw#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
      vp#misc#modify_bg [`NORMAL, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)];
      vbox#misc#reparent vp#coerce;
      hide qi;
      close qi "";
      let window = Gtk_util.window_tooltip sw#coerce ~fade:false ~x ~y ~width:700 ~height:300 ~show:false () in
      let wininfo = {
        window;
        area;
        is_pinned = false;
        index = new_index();
      } in
      add_wininfo qi wininfo;
      (*kprintf label_index#set_label "%d" wininfo.index;*)
      make_pinnable wininfo;
      window#present()
    end;
    qi.view#buffer#apply_tag qi.tag ~start ~stop;
  end;
  label_typ, label_doc

let build_content qi (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  (* TODO .... *)
  let contains_type_vars = String.contains entry.Merlin_t.te_type '\'' in
  let is_module = String.starts_with ~prefix:"(" entry.te_type in
  Printf.sprintf "%s%s" entry.te_type
    (if contains_type_vars || is_module
     then entry2 |> Option.fold ~none:"" ~some:(fun (x : type_enclosing_value) -> "\n" ^ x.te_type)
     else "")

(** Opens a new quick information window with the information received from merlin.
    This function is applied in a separate thread from the main one. *)
let spawn_window qi position (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  if qi.view#misc#get_flag `HAS_FOCUS then begin
    let typ = build_content qi entry entry2 in
    let markup = Markup.type_info typ in
    (*let markup = sprintf "<span size='%s'>%s</span>" markup_odoc#code_font_size (Markup.type_info typ) in
      Printf.printf "%s\n%!" markup;*)
    let start = qi.view#obuffer#get_iter (`LINECHAR (entry.te_start.line - 1, entry.te_start.col)) in
    let stop = qi.view#obuffer#get_iter (`LINECHAR (entry.te_stop.line - 1, entry.te_stop.col)) in
    let label_typ, label_doc = display qi start stop in
    (*<span size='small' font_family='%s'>%s</span>*)
    label_typ#set_label markup;
    qi.merlin@@Merlin.document ~position begin fun doc ->
      let markup = qi.markup_odoc#convert doc in
      label_doc#set_label markup;
    end
  end

let invoke_merlin qi iter ~continue_with =
  let position = iter#line + 1, iter#line_index in
  qi.merlin@@Merlin.type_enclosing ~position begin fun types ->
    match types with
    | [] -> close qi "no-type"
    | fst :: snd :: _ -> continue_with position fst (Some snd)
    | fst :: _ -> continue_with position fst None
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

let process_location qi x y =
  let current_window = get_current_window qi in
  let current_area = Option.bind current_window (fun x -> x.area) in
  match current_area with
  | Some (area, _) when area @<= (x, y) -> ()
  | _ when is_pinned qi -> ()
  | _ ->
      let is_immobile = x = qi.current_x && y = qi.current_y in
      qi.current_x <- x;
      qi.current_y <- y;
      let is_mouse_over =
        match current_window with
        | Some wi ->
            begin
              try
                let root_window = Gdk.Window.root_parent () in
                let r = wi.window#misc#allocation in
                let wx, wy = Gdk.Window.get_position wi.window#misc#window in
                let px, py = Gdk.Window.get_pointer_location root_window in
                wx <= px && px <= wx + r.Gtk.width && wy <= py && py <= wy + r.Gtk.height
              with Gpointer.Null -> false
            end
        | _ -> false
      in
      if is_mouse_over then ()
      else if is_immobile then begin
        match get_typeable_iter_at_coords qi x y with
        | Some iter ->
            hide qi;
            close qi "before-invoke-merlin";
            invoke_merlin qi iter ~continue_with:(spawn_window qi);
        | _ -> close qi "not-typeable"
      end else close qi ""

(** Displays quick info about the expression at the specified iter. *)
let at_iter (qi : t) (iter : GText.iter) () = ()
(*  stop qi;
    let rect = qi.view#get_iter_location iter in
    let x = Gdk.Rectangle.x rect in
    let y = Gdk.Rectangle.y rect in
    let x, y = qi.view#buffer_to_window_coords ~x ~y ~tag:`WIDGET in
    qi.show_at <- Some (x, y + Gdk.Rectangle.height rect);
    process_location ~invoke:true qi x y*)

let query_tooltip qi ~x ~y ~kbd _ =
  (*Log.println `DEBUG "%d %d %f" x y (Unix.gettimeofday());*)
  begin
    try process_location qi x y;
    with ex ->
      Printf.eprintf "File \"quick_info.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  end;
  false

let set_active qi value =
  qi.is_active <- value;
  if not qi.is_active then hide qi

let create (view : Ocaml_text.view) =
  let open Preferences in
  let bg_color = ?? (Preferences.preferences#get.editor_bg_color_popup) in
  let filename = match view#obuffer#file with Some file -> file#filename | _ -> "" in
  let qi =
    {
      markup_odoc = new Markup.odoc();
      is_active = true;
      view = view;
      filename = filename;
      tag = view#buffer#create_tag ~name:"quick-info" [`BACKGROUND bg_color];
      current_x = 0;
      current_y = 0;
      show_at = None;
      windows = [];
      merlin = merlin view#obuffer;
    }
  in
  let motion_notify = SignalId.create() in
  view#event#connect#key_press ~callback:begin fun ev ->
    view#misc#set_has_tooltip false;
    view#event#connect#motion_notify ~callback:begin fun _ ->
      view#misc#set_has_tooltip qi.is_active;
      SignalId.disconnect motion_notify view#as_widget;
      false
    end |> SignalId.save motion_notify;
    hide qi;
    close qi "key-press";
    false
  end |> ignore;
  view#event#connect#button_press ~callback:begin fun _ ->
    view#misc#set_has_tooltip false;
    unpin qi;
    close qi "button-press";
    Gmisclib.Idle.add ~prio:300 (fun () -> view#misc#set_has_tooltip qi.is_active);
    false
  end |> ignore;
  view#event#connect#scroll ~callback:begin fun _ ->
    unpin qi;
    hide qi;
    close qi "scroll";
    false
  end |> ignore;
  view#event#connect#focus_in ~callback:begin fun _ ->
    GMain.Timeout.add ~ms:500 ~callback:begin fun () ->
      view#misc#set_has_tooltip qi.is_active;
      false
    end |> ignore;
    false
  end |> ignore;
  view#event#connect#focus_out ~callback:begin fun _ ->
    view#misc#set_has_tooltip false;
    unpin qi;
    close qi "focus_out";
    false
  end |> ignore;
  view#misc#set_has_tooltip qi.is_active;
  view#misc#connect#query_tooltip ~callback:(query_tooltip qi) |> ignore;
  Preferences.preferences#connect#changed ~callback:begin fun pref ->
    qi.view#misc#set_has_tooltip (qi.is_active && pref.Settings_j.editor_quick_info_enabled);
    qi.markup_odoc <- new Markup.odoc()
  end |> ignore;
  qi
