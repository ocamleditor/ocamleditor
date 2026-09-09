open Merlin_j
open Printf
open Utils
open Oe

module Log = Common.Log.Make(struct let prefix = "QUICK-INFO" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

type effct = Motion | Fade

let effct : effct option = None

let merlin (buffer : Ocaml_text.buffer) func cont =
  let filename = match buffer#file with Some file -> file#filename | _ -> "" in
  let buffer = buffer#get_text () in
  (Merlin.as_cps func ~filename ~buffer) cont

module SignalId = struct
  let create () = ref None
  let save cell id = cell := Some id
  let disconnect cell widget =
    match !cell with
    | Some id -> cell := None; GtkSignal.disconnect widget id
    | None -> ()
end

type t = {
  mutable markup_odoc : Markup.odoc;
  view : Ocaml_text.view;
  filename : string;
  tag : GText.tag;
  mutable is_active : bool;
  mutable show_at : (int * int) option;
}

type global_window = {
  g_window : GWindow.window;
  g_fullname : GMisc.label;
  g_typexpr : GMisc.label;
  g_vars : GMisc.label;
  g_doc : GMisc.label;
  mutable g_pos : (int * int);
  mutable g_range : (GText.view * GText.tag * Gtk.text_mark * Gtk.text_mark) option;
  mutable g_is_pointer_over : bool;
  mutable g_is_pinned : bool;
  mutable g_timer : Glib.Timeout.id option;
}

let global_window = ref None

let create_global_window () =
  let g_window = GWindow.window
      ~decorated:false
      ~modal:false
      ~border_width:0
      ~deletable:true
      ~resizable:true
      ~kind:`POPUP
      ~type_hint:`NORMAL
      ~focus_on_map:false
      ~show:false ()
  in
  Gmisclib.Util.esc_destroy_window g_window;
  g_window#set_skip_pager_hint true;
  g_window#set_skip_taskbar_hint true;
  g_window#set_urgency_hint false;
  g_window#set_accept_focus false;
  g_window#misc#set_can_focus false;
  (*let sw = GBin.scrolled_window ~packing:g_window#add () in*)
  let vbox = GPack.vbox ~border_width:5 ~spacing:5 ~packing:g_window#add () in
  let g_fullname =
    GMisc.label ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~line_wrap:false ~packing:(vbox#pack ~expand:false) () in
  let g_typexpr =
    GMisc.label ~xpad:10 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~line_wrap:false ~packing:(vbox#pack ~expand:false) () in
  let g_vars =
    GMisc.label ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~packing:(vbox#pack ~expand:false) ~show:false () in
  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false) () in
  let g_doc =
    GMisc.label ~xpad:0 ~ypad:0 ~xalign:0.0 ~yalign:0.0 ~line_wrap:true ~packing:(vbox#pack ~expand:false) () in
  g_typexpr#set_use_markup true;
  g_fullname#set_use_markup true;
  g_vars#set_use_markup true;
  g_doc#set_use_markup true;
  let open Preferences in
  let open Settings_j in
  g_doc#misc#modify_font_by_name preferences#get.editor_completion_font;
  g_vars#misc#modify_font_by_name preferences#get.editor_completion_font;
  g_typexpr#misc#modify_font_by_name preferences#get.editor_completion_font;
  g_fullname#misc#modify_font_by_name preferences#get.editor_base_font;
  let gw =
    {
      g_window; g_fullname; g_typexpr; g_vars; g_doc;
      g_range = None; g_pos = (0, 0); g_timer = None;
      g_is_pointer_over = false; g_is_pinned = false }
  in
  g_window#event#connect#enter_notify ~callback:begin fun _ ->
    gw.g_is_pointer_over <- true;
    (*Log.println `DEBUG "ENTER %d\n%!" wininfo.index;*)
    false
  end |> ignore;
  g_window#event#connect#leave_notify ~callback:begin fun ev ->
    let detail = GdkEvent.Crossing.detail ev in
    gw.g_is_pointer_over <- detail = `INFERIOR;
    (*if not wininfo.is_pointer_over then
      Log.println `DEBUG "LEAVE %d\n%!" wininfo.index;*)
    false
  end |> ignore;
  g_window#event#connect#button_press ~callback:begin fun _ ->
    gw.g_is_pinned <- true;
    g_window#misc#style_context#add_class "dialog-window";
    true
  end |> ignore;
  global_window := Some gw

let remove_highlight gw =
  match gw.g_range with
  | Some (view, tag, m_start, m_stop) ->
      Fun.protect begin fun () ->
        let start =
          match Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer m_start with
          | Some it -> new GText.iter it
          | _ -> view#buffer#start_iter
        in
        let stop =
          match Gmisclib.Util.get_iter_at_mark_opt view#buffer#as_buffer m_stop with
          | Some it -> new GText.iter it
          | _ -> view#buffer#end_iter
        in
        view#buffer#remove_tag tag ~start ~stop;
      end ~finally:begin fun () ->
        gw.g_range <- None;
        if not (GtkText.Mark.get_deleted m_start) then view#buffer#delete_mark (`MARK m_start);
        if not (GtkText.Mark.get_deleted m_stop) then view#buffer#delete_mark (`MARK m_stop);
      end
  | _ -> ()

let reset ?(hide=true) ?(ms=0) () =
  let callback () =
    !global_window |> Option.iter begin fun gw ->
      if gw.g_range <> None && not gw.g_is_pointer_over then begin
        remove_highlight gw;
        if hide then gw.g_window#misc#hide();
        gw.g_fullname#set_label "";
        gw.g_typexpr#set_label "";
        gw.g_vars#set_label "";
        gw.g_doc#set_label "";
        gw.g_fullname#misc#hide();
        gw.g_vars#misc#hide();
        gw.g_doc#misc#hide();
        gw.g_range <- None;
        gw.g_is_pointer_over <- false;
        gw.g_is_pinned <- false;
        gw.g_window#resize ~width:400 ~height:1;
      end
    end;
    false
  in
  if ms > 0 then GMain.Timeout.add ~ms ~callback |> ignore
  else callback () |> ignore

let unpin () =
  !global_window |> Option.iter begin fun gw ->
    gw.g_is_pinned <- false;
    gw.g_window#misc#style_context#remove_class "dialog-window";
  end

let motion_effect gw x y k =
  let x0, y0 = gw.g_pos in
  let dx = x - x0 in
  let dy = y - y0 in
  if abs dx > 100 || abs dy > 50 || not gw.g_window#misc#visible then begin
    gw.g_window#present();
    gw.g_window#move ~x:x ~y:y;
    k();
    None
  end else begin
    gw.g_window#present();
    let duration_steps = 10 in
    let step = ref 0 in
    let update () =
      if !step < duration_steps then begin
        incr step;
        let t = float_of_int !step /. float_of_int duration_steps in
        let x = float_of_int x0 +. t *. (float_of_int x -. float_of_int x0) |> int_of_float in
        let y = float_of_int y0 +. t *. (float_of_int y -. float_of_int y0) |> int_of_float in
        Gmisclib.Idle.add ~prio:300 (fun () -> gw.g_window#move ~x ~y);
        true
      end else begin
        gw.g_window#move ~x:x ~y:y;
        k();
        false
      end
    in
    Some (Glib.Timeout.add ~ms:15 ~callback:update)
  end

let fade_effect gw x y k =
  gw.g_window#set_opacity 0.0;
  gw.g_window#present();
  gw.g_window#move ~x ~y;
  gw.g_pos <- (x, y);
  let duration_steps = 10 in
  let step = ref 0 in
  let update () =
    if !step < duration_steps then begin
      incr step;
      let opa = float !step /. float duration_steps in
      Gmisclib.Idle.add ~prio:300 (fun () -> gw.g_window#set_opacity opa);
      true
    end else begin
      gw.g_window#set_opacity 1.0;
      k();
      false
    end
  in
  Some (Glib.Timeout.add ~ms:40 ~callback:update)

let (!=) (p1 : Merlin_j.pos) (p2 : Merlin_j.pos) =
  p1.col <> p2.col || p1.line <> p2.line

let display qi start stop =
  let rstart = qi.view#get_iter_location start in
  let _, ystart = qi.view#buffer_to_window_coords ~tag:`WIDGET
      ~x:(Gdk.Rectangle.x rstart) ~y:(Gdk.Rectangle.y rstart) in
  let x, y =
    let pX, pY = Gdk.Window.get_pointer_location (Window.root_window qi.view) in
    let win = (match qi.view#get_window `WIDGET with None -> assert false | Some w -> w) in
    let px, py = Gdk.Window.get_pointer_location win in
    match qi.show_at with
    | Some (x, y) ->
        qi.show_at <- None;
        pX - px + x, pY - py + y
    | _ ->
        let _, lh = qi.view#get_line_yrange start in
        pX (*- px + xstart*) - 13, pY - py + ystart + lh - qi.view#pixels_below_lines
  in
  !global_window |> Option.iter begin fun gw ->
    gw.g_range <-
      Some (qi.view#as_gtext_view,
            qi.tag,
            qi.view#buffer#create_mark ~name:"qi-start" start,
            qi.view#buffer#create_mark ~name:"qi-stop" stop);
    begin
      match gw.g_range with
      | Some (view, tag, m_start, m_stop) ->
          begin
            try
              let start = Gmisclib.Util.get_iter_at_mark_safe view#buffer#as_buffer m_start in
              let stop = Gmisclib.Util.get_iter_at_mark_safe view#buffer#as_buffer m_stop in
              GtkText.Buffer.apply_tag view#buffer#as_buffer tag#as_tag start stop
            with Gmisclib_util.Mark_deleted -> Log.println `WARN "Mark_deleted"
          end;
      | _ -> ()
    end;
    Gaux.may (GWindow.toplevel qi.view) ~f:(fun x -> gw.g_window#set_transient_for x#as_window);
    gw.g_timer |> Option.iter GMain.Timeout.remove;
    match effct with
    | None ->
        gw.g_window#move ~x ~y;
        gw.g_window#present();
    | Some Motion ->
        gw.g_timer <- motion_effect gw x y begin fun () ->
            gw.g_pos <- (x, y);
            gw.g_timer <- None
          end;
    | Some Fade ->
        gw.g_timer |> Option.iter GMain.Timeout.remove;
        gw.g_timer <- fade_effect gw x y (fun () -> gw.g_timer <- None);
  end

let get_iter_at_line buffer pos =
  let ln = pos.line - 1 in
  if pos.line < 0 || ln > buffer#end_iter#line then raise (Invalid_linechar pos);
  buffer#get_iter (`LINE ln)

let get_iter_at_linechar buffer pos =
  let it = get_iter_at_line buffer pos in
  if pos.col >= it#chars_in_line then raise (Invalid_linechar pos);
  it#set_line_offset pos.col (*buffer#get_iter (`LINECHAR (pos.line - 1, pos.col))*)

let get_type_info qi (entry : type_enclosing_value) (entry2 : type_enclosing_value option) =
  (*Printf.printf "merlin(1): %s\n%!" entry.Merlin_t.te_type;
    Printf.printf "merlin(2): %s\n%!" (match entry2 with Some e -> e.Merlin_t.te_type | _ -> "NONE");*)
  let tail, (type_expr, type_params) =
    let tail, (type_expr, varmap) =
      match entry2 with
      | Some entry2 ->
          begin
            try
              entry.Merlin_t.te_tail, Type_expr.find_substitutions entry.Merlin_t.te_type entry2.Merlin_t.te_type
            with Syntaxerr.Error _ -> entry.Merlin_t.te_tail, (entry.Merlin_t.te_type, [])
          end;
      | _ -> entry.Merlin_t.te_tail, (entry.Merlin_t.te_type, [])
    in
    let info = varmap |> List.map (fun (n, v) -> sprintf "  %s is %s" (Markup.type_info n) (Markup.type_info v)) in
    tail, (type_expr,
           if info <> [] then "In this context\n" ^ (info |> String.concat "\n") else "")
  in
  let tail_info =
    match tail with
    | Merlin_t.No -> ""
    | Merlin_t.Position -> "\nTail Position"
    | Merlin_t.Call -> "\nTail Call"
  in
  let start = get_iter_at_linechar qi.view#buffer entry.te_start in
  let stop = get_iter_at_linechar qi.view#buffer entry.te_stop in
  start, stop, tail_info, Markup.type_info type_expr, type_params

let merlin_type qi position k =
  (*let l, c = position in Log.println `DEBUG "%s %d,%d" __FUNCTION__ l c;*)
  merlin qi.view#obuffer @@ Merlin.type_enclosing ~position () |=> begin function
    | Merlin.Ok types ->
        begin
          match types with
          | [] -> ()
          | fst :: snd :: _ -> GtkThread.async k (get_type_info qi fst (Some snd))
          | fst :: _ -> GtkThread.async k (get_type_info qi fst None)
        end;
    | Merlin.Failure _ | Merlin.Error _ -> ()
    end

let merlin_doc qi position k =
  merlin qi.view#obuffer @@ Merlin.document ~position () |=> begin function
    | Merlin.Ok doc -> GtkThread.async k doc
    | Merlin.Failure msg | Merlin.Error msg -> ()
    end

let ocp_index_fullname qi start stop k =
  let ident = qi.view#obuffer#get_text ~start ~stop () in
  let context = qi.filename, Some (start#line + 1), Some (start#line_offset + 1) in
  Ocp_index.fullname_async ~context ident
  |> Async.start_with_continuation ~name:__FUNCTION__ begin fun fullname ->
    try
      match fullname with
      | Some fullname -> GtkThread.async k fullname
      | None -> GtkThread.async k ""
    with ex ->
      Log.println `ERROR "%s\n\t%s\n%s" __FUNCTION__ (Printexc.to_string ex) (Printexc.get_backtrace())
  end

let collect_info qi (iter : GText.iter) ~continue_with =
  if qi.view#has_focus then begin
    !global_window |> Option.iter begin fun gw ->
      let position = iter#line + 1, iter#line_index in
      merlin_type qi position begin fun (start, stop, tail_info, type_expr, type_params) ->
        (* ocp-index needs start and stop from merlin *)
        ocp_index_fullname qi start stop begin fun fullname ->
          if fullname <> "" then begin
            gw.g_fullname#set_label (Markup.type_info fullname);
            gw.g_fullname#misc#show();
          end else gw.g_fullname#misc#hide();
          continue_with start stop;
        end;
        gw.g_typexpr#set_label (sprintf "%s%s" type_expr tail_info);
        if type_params <> "" then begin
          gw.g_vars#set_label type_params;
          gw.g_vars#misc#show();
        end;
        merlin_doc qi position begin fun doc ->
          let markup = qi.markup_odoc#convert doc in
          gw.g_doc#misc#show();
          gw.g_doc#set_label markup
        end;
      end;
    end
  end

let is_iter_in_comment (buffer : Ocaml_text.buffer) iter =
  Comments.enclosing (Comments.scan (buffer#get_text ())) iter#offset

let get_typeable_iter_at_coords qi iter =
  if iter#ends_line
  || Glib.Unichar.isspace iter#char
  || (match is_iter_in_comment qi.view#obuffer iter with None -> false | _ -> true)
  then None else Some iter

let in_range buffer iter ~start ~stop =
  try
    let start = Gmisclib.Util.get_iter_at_mark_safe buffer start in
    let stop = Gmisclib.Util.get_iter_at_mark_safe buffer stop in
    GtkText.Iter.in_range iter start stop
  with Gmisclib_util.Mark_deleted -> false

let process_location qi x y =
  !global_window |> Option.iter begin fun gw ->
    let current_range = Option.bind !global_window (fun gw -> gw.g_range) in
    let bx, by = qi.view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
    if bx > 0 then begin
      let iter = qi.view#get_iter_at_location ~x:bx ~y:by in
      match current_range with
      | Some (_, _, start, stop) when in_range qi.view#buffer#as_buffer iter#as_iter ~start ~stop -> ()
      | _ when gw.g_is_pinned -> ()
      (*| _ when qi.view#buffer#has_selection -> ()*)
      | _ ->
          begin
            match get_typeable_iter_at_coords qi iter with
            | Some iter ->
                reset ~hide:(effct = None) ();
                collect_info qi iter ~continue_with:(display qi)
            | _ -> reset ~ms:100 ();
          end
    end
  end

let at_iter qi (iter : GText.iter) () =
  let rect = qi.view#get_iter_location iter in
  let x = Gdk.Rectangle.x rect in
  let y = Gdk.Rectangle.y rect in
  let x, y = qi.view#buffer_to_window_coords ~x ~y ~tag:`WIDGET in
  qi.show_at <- Some (x, y + Gdk.Rectangle.height rect);
  process_location qi x y

let debouncer = Debouncer.create ~ms:50

let query_tooltip qi ~x ~y ~kbd _ =
  (*Log.println `DEBUG "%d %d %f" x y (Unix.gettimeofday());*)
  begin
    try Debouncer.schedule debouncer (fun () -> process_location qi x y);
    with ex ->
      Printf.eprintf "File \"quick_info.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  end;
  false

let set_active qi value = qi.is_active <- value;;

let connect_to_view qi (view : Ocaml_text.view) =
  let motion_notify = SignalId.create() in
  view#event#connect#key_press ~callback:begin fun ev ->
    view#misc#set_has_tooltip false;
    view#event#connect#motion_notify ~callback:begin fun _ ->
      view#misc#set_has_tooltip qi.is_active;
      SignalId.disconnect motion_notify view#as_widget;
      false
    end |> SignalId.save motion_notify;
    reset();
    false
  end |> ignore;
  view#event#connect#button_press ~callback:begin fun _ ->
    view#misc#set_has_tooltip false;
    unpin ();
    reset ();
    Gmisclib.Idle.add ~prio:300 (fun () -> view#misc#set_has_tooltip qi.is_active);
    false
  end |> ignore;
  view#event#connect#scroll ~callback:begin fun _ ->
    GMain.Timeout.add ~ms:200 ~callback:begin fun () ->
      unpin ();
      reset ();
      false
    end  |> ignore;
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
    unpin ();
    reset ();
    false
  end |> ignore;
  view#event#connect#leave_notify ~callback:begin fun _ ->
    Gmisclib.Idle.add ~prio:300 reset;
    false
  end |> ignore;
  view#misc#set_has_tooltip qi.is_active;
  view#misc#connect#query_tooltip ~callback:(query_tooltip qi) |> ignore

let create (view : Ocaml_text.view) =
  if !global_window = None then create_global_window();
  let open Preferences in
  let bg_color = ?? (Preferences.preferences#get.Settings_t.editor_bg_color_popup) in
  let filename = match view#obuffer#file with Some file -> file#filename | _ -> "" in
  let qi =
    {
      markup_odoc = new Markup.odoc();
      is_active = true;
      view = view;
      filename = filename;
      tag = view#buffer#create_tag ~name:"quick-info" [`BACKGROUND bg_color];
      show_at = None;
    }
  in
  Preferences.preferences#connect#changed ~callback:begin fun pref ->
    qi.view#misc#set_has_tooltip (qi.is_active && pref.Settings_j.editor_quick_info_enabled);
    qi.markup_odoc <- new Markup.odoc()
  end |> ignore;
  if view#obuffer#is_ocaml_file filename then
    connect_to_view qi view;
  qi
