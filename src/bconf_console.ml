(*

  OCamlEditor
  Copyright (C) 2010, 2011 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)

open Printf
open Miscellanea
open Bconf

let re_error_line = Str.regexp_case_fold
  ".*\"\\(.+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):?"

let re_assert_failure = Str.regexp ".*\\(Assert_failure(\"\\(.+\\)\", \\([0-9]+\\), \\([0-9]+\\))\\)"

class view ~project ~(editor : Editor.editor) ?(task_kind=(`OTHER : Task.kind)) ~(vbox:GPack.box) ~task ~working_func () =
  let tooltips = GData.tooltips () in
  let toolbar = GButton.toolbar ~orientation:`VERTICAL ~style:`ICONS ~packing:(vbox#pack ~expand:false) () in
  let _ = toolbar#set_icon_size `MENU in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:vbox#add () in
  let button_clear = GButton.tool_button ~packing:toolbar#insert () in
  let _ = tooltips#set_tip ~text:"Clean Messages" button_clear#coerce in
  let _ = button_clear#set_icon_widget (Icons.create Icons.clear_16)#coerce in
  let button_wrap = GButton.toggle_tool_button ~packing:toolbar#insert () in
  let _ = tooltips#set_tip ~text:"Word Wrap" button_wrap#coerce in
  let _ = button_wrap#set_icon_widget (Icons.create Icons.wrap_lines_16)#coerce in
  let _ = GButton.separator_tool_item ~packing:toolbar#insert () in
  let button_stop = GButton.tool_button ~stock:`STOP ~packing:toolbar#insert () in
  let _ = tooltips#set_tip ~text:"Kill Process" button_stop#coerce in
  let button_run = GButton.tool_button ~packing:toolbar#insert () in
  let _ = button_run#set_icon_widget begin match task_kind with
    | `OTHER | `RUN ->
      tooltips#set_tip ~text:"Start" button_run#coerce;
      (Icons.create Icons.start_16);
    | `CLEAN | `CLEANALL->
      tooltips#set_tip ~text:task.Task.name button_run#coerce;
      (Icons.create Icons.clear_build_16);
    | `ANNOT | `COMPILE ->
      tooltips#set_tip ~text:task.Task.name button_run#coerce;
      (Icons.create Icons.build_16);
  end#coerce in
  let view = GText.view ~editable:(task_kind = `RUN) ~cursor_visible:true ~packing:sw#add () in
  let _ = view#set_right_margin 2 in
  let _ = view#set_left_margin 2 in
  let _ = button_wrap#connect#toggled ~callback:begin fun () ->
    view#set_wrap_mode (if button_wrap#get_active then `WORD else `NONE)
  end in
  let _ = button_clear#connect#clicked ~callback:(fun () -> view#buffer#set_text "") in
  let _ = button_wrap#set_active true in
object (self)
  val mutable seq_tag_location = 0
  val mutable tag_locations = [];
  val mutable process = None
  val mutable signal_enabled = true
  val m_write = Mutex.create ()
  val mutable process_outchan = None
  val mutable thread_run = None
  val mutable has_errors = false
  val mutable killed = false
  val mutable task = task
  val mutable button_run_signals = []
  val mutable current_run_cb = None
  val mutable current_use_thread = None
  method button_run = button_run
  method set_task x = task <- x
  method set_has_errors x = has_errors <- x
  method has_errors = has_errors
  method killed = killed
  method set_working : bool -> unit = fun x -> working_func (not x)
  method buffer = view#buffer
  method view : GText.view = view
  method active = process <> None
  method clear () =
    let buffer = view#buffer in
    buffer#delete ~start:(buffer#get_iter `START) ~stop:(buffer#get_iter `END)

  method close () =
    button_stop#misc#set_sensitive false;
    button_run#misc#set_sensitive true;
    self#view#set_editable false;
    process <- None;

  method stop () =
    match process with None -> () | Some proc ->
      Process.kill proc;
      killed <- true;
      self#close()

  method restart () =
    self#stop();
    self#run ?run_cb:current_run_cb ?use_thread:current_use_thread ();

  method iter_chan f inchan =
    set_binary_mode_in inchan false;
    try while true do f inchan done with End_of_file -> ()

  method run ?(run_cb : (unit -> unit) option) ?(use_thread=true) () =
    current_run_cb <- run_cb;
    current_use_thread <- Some use_thread;
    let callback () =
      match process with
        | None ->
          if use_thread then begin
            let th = Thread.create (fun () -> self#do_run task) () in
            thread_run <- Some th;
            Some th
          end else begin
            self#do_run task;
            None
          end
        | Some _ ->
          GtkThread2.async Messages.messages#present vbox#coerce;
          if use_thread then
            (match thread_run with None -> assert false | Some th -> Some th)
          else None
    in
    (* Disconnect previous callback assiciated with button_run and connect with a new one. *)
    GtkThread2.sync begin fun () ->
      begin
        try
          let sid = List.assoc button_run#misc#get_oid button_run_signals in
          GtkSignal.disconnect button_run#as_widget sid;
          ignore (List.remove_assoc button_run#misc#get_oid button_run_signals);
        with Not_found -> ()
      end;
      let sid = button_run#connect#clicked ~callback:begin fun () ->
        match task_kind, run_cb with
          | `RUN, Some f -> ignore (f ())
          | _ -> ignore (callback())
      end in
      button_run_signals <- (button_run#misc#get_oid, sid) :: button_run_signals;
    end ();
    callback()

  method private do_run task =
    let finally () =
      GtkThread2.async begin fun () ->
        self#close();
        if has_errors then begin
          (*play "error.wav";*)
          view#scroll_to_mark (`NAME "first_error_line");
        end else begin
          (*play "success.wav";*)
          ignore (view#scroll_to_mark `INSERT);
        end;
        self#set_working false;
      end ()
    in
    if task_kind = `COMPILE && Oe_config.save_all_before_compiling then (editor#save_all());
    has_errors <- false;
    GtkThread2.async begin fun () ->
      (try view#buffer#delete_mark (`NAME "first_error_line");
      with GText.No_such_mark("first_error_line") -> ());
      tag_locations <- [];
      view#buffer#remove_all_tags ~start:(view#buffer#get_iter `START) ~stop:(view#buffer#get_iter `END);
    end ();
    (** Process instantiation *)
    let proc, start_proc = Task.prepare task in
    (** Print command line *)
    GtkThread2.async begin fun () ->
      self#view#set_editable true;
      button_run#misc#set_sensitive false;
      button_stop#misc#set_sensitive true;
      self#set_working true;
      Mutex.lock m_write;
      signal_enabled <- false;
      self#clear();
(*      kprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "Environment: %s" (String.concat "; " task.Task.env);
      kprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "\nWorking directory: %s\n" task.Task.dir;
      kprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "Command:\n%s\n" (Process.cmd_line proc);*)
      kprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "%s\n" (Miscellanea.expand (project.Project.autocomp_compiler ^ " -v"));
      kprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "%s\n" (Process.cmd_line proc);
      signal_enabled <- true;
      Mutex.unlock m_write;
      Messages.messages#present vbox#coerce;
    end ();
    (** Process start *)
    start_proc();
    process <- Some proc;
    try
      let inchan, outchan, errchan = Process.channels proc in
      process_outchan <- Some outchan;
      (** Print the PID of the process *)
      GtkThread2.async begin fun () ->
        kprintf (view#buffer#insert ~tag_names:["bold"; "output"]) "PID=%d\n\n" (Process.getpid proc);
      end ();
      (** Thread looping over the standard output of the process *)
      let th_in = Thread.create (self#iter_chan begin fun ic ->
        let line = input_line ic in
        let line = if Glib.Utf8.validate line then line else Convert.to_utf8 line in
        GtkThread2.async begin fun line ->
          Mutex.lock m_write;
          signal_enabled <- false;
          view#buffer#insert ~iter:(self#buffer#get_iter `END) ~tag_names:["output"] (line ^ "\n");
          view#scroll_to_mark `INSERT;
          signal_enabled <- true;
          Mutex.unlock m_write;
        end line;
      end) inchan in
      (** Thread looping over the standard error of the process *)
      let first_error_line = ref false in
      let pending = ref None in
      let th_err = Thread.create (self#iter_chan begin fun ic ->
        begin
          let line = input_line ic in
          let line = if Glib.Utf8.validate line then line else Convert.to_utf8 line in
          let error_line = try Str.string_before line 6 = "Error:"
            with Invalid_argument("String.sub") -> false in
          let warning_line = try Str.string_before line 7 = "Warning"
            with Invalid_argument("String.sub") -> false in
          has_errors <- has_errors || error_line;
          let tag = if warning_line then "warning" else "error" in
          let location_line = Str.string_match re_error_line line 0 in
          let location_line = location_line || (Str.string_match re_assert_failure line 0) in
          GtkThread2.async begin fun line ->
            Mutex.lock m_write;
            signal_enabled <- false;
            (** Links *)
            if location_line then begin
              let tag_location_name = sprintf "loc-%d" seq_tag_location in
              let tag_location = view#buffer#create_tag ~name:tag_location_name [] in
              tag_locations <- (tag_location#get_oid, (tag_location, line)) :: tag_locations;
              seq_tag_location <- seq_tag_location + 1;
              pending := Some (line, tag_location_name);
              view#buffer#insert ~iter:(self#buffer#get_iter `END) ~tag_names:["error"; tag_location_name] (line ^ "\n");
            end else begin
              let _pt = match !pending with None -> []
                | Some (pl, pt) ->
                  let start = (self#buffer#get_iter `END)#backward_line in
                  view#buffer#apply_tag_by_name tag ~start ~stop:(self#buffer#get_iter `END);
                  if not !first_error_line && tag = "error" then begin
                    view#buffer#create_mark ~name:"first_error_line" start;
                    first_error_line := true;
                  end;
                  (*pending := None;*)
                  [pt]
              in
              view#buffer#insert ~iter:(self#buffer#get_iter `END) ~tag_names:([tag] (*@ pt*)) (line ^ "\n");
            end;
            view#scroll_to_mark `INSERT;
            signal_enabled <- true;
            Mutex.unlock m_write;
          end line;
        end;
      end) errchan in
      Thread.join th_in;
      Thread.join th_err;
      begin
        try ignore (Process.close proc);
        with ex -> (printf "%s\n%!" (Printexc.to_string ex))
      end;
      finally()
    with Process.Not_started -> (finally())

  initializer
    ignore (Messages.messages#connect#remove_page ~callback:begin fun child ->
      if child#misc#get_oid = vbox#misc#get_oid then begin
        match process with None -> () | Some _ ->
          Dialog.process_still_active ~name:task.Task.name
            ~ok:self#stop ~cancel:(fun () -> raise Messages.Cancel_process_termination) ()
      end
    end);
    (** Input of the user redirected to the outchan of the process *)
    if task_kind = `RUN then begin
      ignore (view#buffer#connect#after#insert_text ~callback:begin fun it txt ->
        if process <> None && signal_enabled then (GtkThread2.sync begin fun () ->
          let start = it#backward_chars (String.length txt) in
          view#buffer#apply_tag_by_name "input" ~start ~stop:(view#buffer#get_iter `INSERT);
          match process_outchan with None -> ()
            | Some ochan ->
              output_string ochan (Glib.Convert.convert_with_fallback ~fallback:"?"
                ~from_codeset:"utf8" ~to_codeset:Oe_config.ocaml_codeset txt);
              flush ochan;
        end ())
      end);
    end;
    (** Locations links are activated by button_press *)
    let _ = view#event#connect#button_press ~callback:begin fun ev ->
      let x = int_of_float (GdkEvent.Button.x ev) in
      let y = int_of_float (GdkEvent.Button.y ev) in
      let x, y = view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
      let iter = view#get_iter_at_location ~x ~y in
      let find t = List.assoc t#get_oid tag_locations in
      let line = ref "" in
      List.iter (fun t -> try line := (snd (find t)) with Not_found -> ()) iter#tags;
      let group = if Str.string_match re_error_line !line 0 then 4 - 3
        else if Str.string_match re_assert_failure !line 0 then 2
        else 0 in
      if group > 0 then begin
        let basename = Str.matched_group group !line in  (* 2 *)
        if basename <> "_none_" then begin
          let linenum = int_of_string (Str.matched_group (group + 1) !line) in (* 3 *)
          let start = int_of_string (Str.matched_group (group + 2) !line) in  (* 4 *)
          let len = (* Length from the start of line *)
            try int_of_string (Str.matched_group (group + 3) !line)
            with Invalid_argument "Str.matched_group" -> start
          in
          let parent = project.Project.root // Project.src in
          let filename = parent // basename in
          editor#open_file ~active:true ~offset:0 filename;
          match editor#get_page (Editor_types.File (File.create ~parent basename ())) with
            | None -> false
            | Some page ->
              editor#goto_view page#view;
              let buf = (page#buffer :> Text.buffer) in
              let lines = buf#line_count in
              if linenum < lines - 1 then begin
                let it = buf#get_iter (`LINE (linenum - 1)) in
                let it = it#set_line_index 0 in
                let line = it#get_text ~stop:it#forward_to_line_end in
                let c = Convert.offset_from_pos line ~pos:start in
                let where = it#forward_chars c in
                let len = Convert.offset_from_pos (buf#get_text ~start:it ~stop:buf#end_iter ()) ~pos:len in
                buf#select_range where (it#forward_chars len);
                if (buf#get_iter `INSERT)#compare (buf#get_iter `SEL_BOUND) = 0 then (ignore(buf#select_word()));
                ignore (page#view#scroll_lazy where);
                page#view#misc#grab_focus();
                true
          end else false;
        end else false;
      end else false
    end in
    (** Change the cursor and underline the links *)
    let _ = view#event#connect#motion_notify ~callback:begin fun ev ->
      let x = int_of_float (GdkEvent.Motion.x ev) in
      let y = int_of_float (GdkEvent.Motion.y ev) in
      let x, y = view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
      let iter = view#get_iter_at_location ~x ~y in
      List.iter (fun (_, (t, _)) -> t#set_properties [`UNDERLINE `NONE]) tag_locations;
      Gaux.may (view#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `ARROW));
      List.iter begin fun t ->
        try
          let _ = List.assoc t#get_oid tag_locations in
          let start = iter#backward_to_tag_toggle (Some t) in
          let stop = iter#forward_to_tag_toggle (Some t) in
          t#set_properties [`UNDERLINE `LOW];
          Gaux.may (view#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (!Gtk_util.cursor `HAND1));
        with Not_found -> () (* The cursor is not inside a tag_location *)
      end iter#tags;
      false
    end in
    (*  *)
(*    button_run#connect#clicked ~callback:(fun () -> ignore (self#run ()));*)
    button_stop#connect#clicked ~callback:self#stop;
    view#misc#modify_font_by_name !Preferences.preferences.Preferences.pref_output_font;
    view#misc#modify_base [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_output_bg];
    ignore (view#buffer#create_tag ~name:"input"
      [`FOREGROUND !Preferences.preferences.Preferences.pref_output_fg_stdin]);
    ignore (view#buffer#create_tag ~name:"error"
      [`FOREGROUND !Preferences.preferences.Preferences.pref_output_fg_err]);
    ignore (view#buffer#create_tag ~name:"warning"
      [`FOREGROUND !Preferences.preferences.Preferences.pref_output_fg_warn]);
    ignore (view#buffer#create_tag ~name:"output"
      [`FOREGROUND !Preferences.preferences.Preferences.pref_output_fg_stdout]);
    ignore (view#buffer#create_tag ~name:"bold" [`WEIGHT `BOLD]);
    view#misc#grab_focus()
end

let views : (string * (view * GObj.widget)) list ref = ref []

(** create *)
let create ~project ~editor task_kind task =
  let console_id = sprintf "%s %s %s" task.Task.name task.Task.cmd (String.concat " " task.Task.args) in
  try
    let (console, box) = List.assoc console_id !views in
    console#set_task task;
    console
  with Not_found -> begin
    let label_widget, finish = match task_kind with
      | `RUN (*| `OTHER*) ->
        let box = GPack.hbox ~spacing:3 () in
        let icon = (Icons.create Icons.start_16) in
        box#pack icon#coerce;
        let _ = GMisc.label ~text:task.Task.name ~packing:box#pack () in
        (Some box#coerce), Some begin fun finish ->
(*          if finish then (icon#misc#hide()) else (icon#misc#show());*)
          if finish then (icon#misc#set_sensitive false) else (icon#misc#set_sensitive true);
        end
      | _ -> None, None
    in
    let vbox = GPack.hbox () in
    let std_finish, button_close_tab =
      Messages.messages#append_page task.Task.name ?label_widget vbox#coerce in
    let finish = match finish with None -> std_finish | Some f -> f in
    let console = new view ~project ~editor ~task_kind ~task ~vbox ~working_func:finish () in
    if task_kind = `RUN then begin
      ignore (button_close_tab#connect#clicked ~callback:begin fun () ->
        if console#active then
          Dialog.process_still_active ~name:task.Task.name
            ~ok:console#stop ~cancel:GtkSignal.stop_emit ()
      end);
    end;
    vbox#connect#destroy ~callback:(fun () -> views := List.remove_assoc console_id !views);
    views := (console_id, (console, vbox#coerce)) :: !views;
    console
  end

(** exec_sync *)
let exec_sync ?run_cb ?(use_thread=true) ?(at_exit=ignore) ~project ~editor tasks =
  let f = begin fun () ->
      begin
        try
          List.iter begin fun (task_kind, task) ->
            let console = GtkThread2.sync (create ~project ~editor task_kind) task in
            begin
              match console#run ?run_cb ~use_thread:true () with
                | None -> ();
                | Some th -> Thread.join th;
            end;
            if console#has_errors || console#killed then (raise Exit)
          end tasks
        with Exit -> ()
      end;
      at_exit()
    end
  in
  if use_thread then (ignore(Thread.create f ())) else (f())


(** exec *)
let exec ~project ~editor ?use_thread task_kind bconf =
  let filter_tasks = Bconf.filter_external_tasks bconf in
  let tasks_clean () =
    (* External build tasks *)
    let et_before_clean = filter_tasks Task.Before_clean in
    let et_clean = filter_tasks Task.Clean in
    let et_clean = if et_clean = [] then [`CLEAN, begin
      let cmd, args = Bconf.create_cmd_line bconf in
      let name = sprintf "Clean \xC2\xAB%s\xC2\xBB" (Filename.basename bconf.Bconf.name) in
      Task.create ~name ~env:[] ~dir:"" ~cmd ~args:(args @ ["-clean"]) ()
    end] else et_clean in
    let et_after_clean = filter_tasks Task.After_clean in
    (* Execute sequence *)
    Project.clean_tmp project;
    et_before_clean @ et_clean @ et_after_clean;
  in
  let tasks_annot () =
    [`ANNOT, begin
      let cmd, args = Bconf.create_cmd_line bconf in
      let args = "-c"  :: "-annot" :: args in
      let name = sprintf "Compile \xC2\xAB%s\xC2\xBB" (Filename.basename bconf.Bconf.name) in
      Task.create ~name ~env:[] ~dir:"" ~cmd ~args ()
    end]
  in
  let compile_name = sprintf "Compile \xC2\xAB%s\xC2\xBB" (Filename.basename bconf.name) in
  let build_name = sprintf "Build \xC2\xAB%s\xC2\xBB" (Filename.basename bconf.name) in
  let at_exit = fun () -> editor#with_current_page (fun p -> p#compile_buffer ~commit:false ()) in
  match task_kind with
  | `CLEANALL ->
    let cmd, args = Bconf.create_cmd_line bconf in
    let task = Task.create ~name:"Clean Project" ~env:[] ~dir:"" ~cmd
      ~args:(args @ ["-clean-all"]) () in
    let console = create ~project ~editor `CLEANALL task in
    console#button_run#connect#clicked ~callback:(fun () -> ignore (console#run()));
    ignore(console#run ?use_thread ())
  | `CLEAN -> exec_sync ~project ~editor (tasks_clean ());
  | `ANNOT -> exec_sync ~project ~editor (tasks_annot ());
  | `COMPILE ->
    exec_sync ~project ~editor ~at_exit (tasks_compile ~name:build_name bconf);
  | `COMPILE_ONLY ->
    exec_sync ~project ~editor ~at_exit (tasks_compile ~flags:["-c"] ~name:compile_name bconf);
  | `RCONF rc ->
    let oebuild, args = Bconf.create_cmd_line bconf in
    let name = rc.Rconf.name in
    let prior_tasks =
      match rc.Rconf.build_task with
        | `NONE -> []
        | `CLEAN -> tasks_clean ()
        | `COMPILE -> tasks_compile ~name:build_name bconf
        | `REBUILD -> tasks_clean () @ tasks_compile ~name:build_name bconf
        | `ETASK name ->
          let etask = List.find ((=) name) bconf.Bconf.external_tasks in
          [`OTHER, etask]
    in
    let tasks = prior_tasks @ [
      `RUN, Task.create
        ~name
        ~env:rc.Rconf.env
        ~env_replace:rc.Rconf.env_replace
        ~dir:""
        ~cmd:oebuild
        ~args:(args @ (["-no-build"; "-run"; "--"] @
          (List.map Quote.arg (Cmd_line_args.parse rc.Rconf.args)))) ();
    ] in
    let rec f () = ignore (exec_sync ~run_cb:f ~project ~editor tasks) in
    f();
  | `INSTALL_LIBRARY ->
    let oebuild, args = Bconf.create_cmd_line bconf in
    let name = Filename.basename bconf.Bconf.name in
    let tasks = (*prior_tasks @*) [
      `RUN, Task.create
        ~name
        ~env:[]
        ~dir:""
        ~cmd:oebuild
        ~args:(args @ ["-install"; bconf.Bconf.lib_install_path]) ();
    ] in
    let rec f () = ignore (exec_sync ~run_cb:f ~project ~editor tasks) in
    f();
  | _ -> assert false






