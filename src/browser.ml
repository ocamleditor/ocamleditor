(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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

open Project
open Project_type
open GdkKeysyms
open Printf
open Miscellanea
open GUtil
open Oe

class browser window =
  let switch_project = new switch_project () in
  let menubar_visibility_changed = new menubar_visibility_changed () in
  let toolbar_visibility_changed = new toolbar_visibility_changed () in
  let tabbar_visibility_changed = new tabbar_visibility_changed () in
  let outline_visibility_changed = new outline_visibility_changed () in
  let vmessages_visibility_changed = new vmessages_visibility_changed () in
  let hvmessages_visibility_changed = new hvmessages_visibility_changed () in
  let project_history_changed = new project_history_changed () in
  let _ = window#add Messages.hpaned#coerce in
  let vbox = GPack.vbox ~packing:Messages.hpaned#add1 () in
  let menubarbox = GPack.hbox ~spacing:13 ~packing:vbox#pack () in
  let menubar = GMenu.menu_bar ~packing:menubarbox#add () in
  (*let messages = Messages.vmessages in*)
  let editor = new Editor.editor () in
  let toolbar = new Toolbar.toolbar ~messages:Messages.vmessages ~hmessages:Messages.hmessages ~editor () in
  let _ = vbox#pack toolbar#coerce in
  let paned = Messages.vpaned in
  let _ = vbox#add paned#coerce in
  (*  *)
  let accel_group = GtkData.AccelGroup.create () in
(*  let _ = window#add_accel_group accel_group in*)
  let get_menu_item_nav_history_backward = ref (fun () -> failwith "get_menu_item_nav_history_backward") in
  let get_menu_item_nav_history_forward = ref (fun () -> failwith "get_menu_item_nav_history_forward") in
  let get_menu_item_nav_history_last = ref (fun () -> failwith "get_menu_item_nav_history_last") in
  let get_menu_item_undo = ref (fun () -> failwith "get_menu_item_undo") in
  let get_menu_item_redo = ref (fun () -> failwith "get_menu_item_redo") in
  let window_title_menu_label = GMisc.label ~markup:"" ~packing:menubarbox#pack ~show:false () in
object (self)
  val mutable finalize = fun _ -> ()
  val mutable projects = []
  val mutable current_project = new GUtil.variable None
  val mutable menubar_visible = true;
  val mutable toolbar_visible = true;
  val mutable tabbar_visible = true;
  val mutable outline_visible = true;
  val mutable maximized_view_action = `NONE
  val mutable geometry = "";
  val mutable project_history =
    File_history.create
      ~filename:Oe_config.project_history_filename
      ~max_length:Oe_config.project_history_max_length
  val mutable dialog_project_properties = []
  val mutable menu = None
  val mutable pref_max_view_2 = false
  val mutable pref_max_view_fullscreen = false
  val mutable max_height_prev_h = -1
  val mutable max_height_prev_y = -1
  val mutable is_max_height = false
  val mutable is_fullscreen = false
  val mutable maximized_view_actions = [
    `NONE, (fun () -> {
      mva_menubar    = true; (* dummies *)
      mva_toolbar    = true;
      mva_tabbar     = true;
      mva_messages   = false;
      mva_fullscreen = false;
    });
    `FIRST, (fun () -> {
      mva_menubar    = true(*Preferences.preferences#get.Preferences.pref_max_view_1_menubar*);
      mva_toolbar    = Preferences.preferences#get.Preferences.pref_max_view_1_toolbar;
      mva_tabbar     = Preferences.preferences#get.Preferences.pref_max_view_1_tabbar;
      mva_messages   = Preferences.preferences#get.Preferences.pref_max_view_1_messages;
      mva_fullscreen = Preferences.preferences#get.Preferences.pref_max_view_1_fullscreen;
    });
    `SECOND, (fun () -> {
      mva_menubar    = true(*Preferences.preferences#get.Preferences.pref_max_view_2_menubar*);
      mva_toolbar    = Preferences.preferences#get.Preferences.pref_max_view_2_toolbar;
      mva_tabbar     = Preferences.preferences#get.Preferences.pref_max_view_2_tabbar;
      mva_messages   = Preferences.preferences#get.Preferences.pref_max_view_2_messages;
      mva_fullscreen = Preferences.preferences#get.Preferences.pref_max_view_2_fullscreen;
    })
  ];

  method editor = editor
  (*method vmessages = Messages.vmessages
  method hmessages = Messages.hmessages*)
  method shell () =
    self#with_current_project (fun project -> Ocaml_shell.append_page ~project Messages.vmessages);
    Messages.vmessages#set_visible true;

  method save_all () =
    editor#save_all();
    self#with_current_project(fun p -> Project.save ~editor p)

  method refresh () = self#with_current_project Project.refresh
  method clear_cache () = self#with_current_project begin fun project ->
    Project.clear_cache project;
    Symbol.Cache.reset ~project
 end

  method current_project = current_project

  method project_history = project_history

  method private project_write_history () =
    File_history.write project_history;
    project_history_changed#call project_history;

  method project_close () =
    self#with_current_project begin fun project ->
      Project.save ~editor project;
      self#project_write_history();
      Symbol.Cache.save ~project;
      Project.unload_path project Config.load_path;
      List.iter (fun p -> Autosave.delete ~filename:p#get_filename ()) editor#pages;
    end

  method project_open filename =
    self#project_close();
    let proj = Project.load filename in
    current_project#set (Some proj);
    Project.load_path proj Config.load_path;
    editor#set_history_switch_page_locked true;
    (*crono ~label:"close_all" *)editor#close_all ();
    editor#pack_outline (Outline.create_empty());
    self#with_current_project editor#set_project;
    Sys.chdir (proj.root // Project.src);
    window#set_title (Convert.to_utf8 proj.name);
    (File_history.add project_history) filename;
    (*crono ~label:"Symbol.Cache.load" (fun () ->*) Symbol.Cache.load ~project:proj;
    Annotation.preload ~project:proj;
    Gmisclib.Idle.add ~prio:300(*crono ~label:"Mbrowser_compl.create"*) (Mbrowser_compl.create ~project:proj);
    Autosave.recover ();
    Gmisclib.Idle.add ~prio:300(*crono ~label:"dialog_project_properties" (fun () -> *)(self#dialog_project_properties ~show:false);
    begin
      try Templ.load_custom (`project proj)
      with Templ.Error (msg, details) ->
        (Dialog.message ~title:"Warning" ~message:(sprintf "%s\n\n\"%s\"" msg details) `WARNING);
    end;
    switch_project#call();
    (* Load files *)
    let active_exists = ref false in
    let i = ref 0 in
    let _ =
      List.map (begin fun (filename, scroll_offset, offset, active) ->
        incr i;
        active_exists := !active_exists || active;
        let active = active || (List.length proj.open_files = !i && not !active_exists) in
        editor#open_file ~active ~scroll_offset ~offset filename;
      end) proj.open_files
    in
    editor#set_history_switch_page_locked false;
    proj.open_files <- [];
    proj.modified <- false;
    proj


  method dialog_project_open () =
    let project_home =
      match self#current_project#get with Some p -> p.Project_type.root | _ -> Oe_config.user_home
    in
    let pat1 = "*"^Project.extension in
    let pat2 = "*"^Project.old_extension in
    let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~width:600 ~height:600
      ~title:"Open project..." ~icon:Icons.oe ~position:`CENTER ~show:false () in
    dialog#add_filter (GFile.filter
      ~name:(sprintf "%s projects (%s)" Oe_config.title pat1) ~patterns:[pat1] ());
    dialog#add_filter (GFile.filter
      ~name:(sprintf "Old %s projects (%s)" Oe_config.title pat2) ~patterns:[pat2] ());
    dialog#add_select_button_stock `OK `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#set_select_multiple false;
    dialog#set_current_folder (Filename.dirname project_home);
    match dialog#run () with
      | `OK ->
        List.iter begin fun filename ->
          let filename, save = if filename ^^ Project.old_extension then (Filename.chop_extension filename) ^ Project.extension, true else filename, false in
          let proj = self#project_open filename in
          if save then Project.save ~editor proj
        end dialog#get_filenames;
        dialog#destroy()
      | _ -> dialog#destroy()

  method dialog_project_properties ?page_num ?(show=true) () =
    self#with_current_project begin fun project ->
      let id = Project.filename project in
      let remove_from_cache () =
        dialog_project_properties <- List.filter (fun (x, _) -> x <> id) dialog_project_properties
      in
      try
        begin
          match List.assoc id dialog_project_properties with
            | window, widget, `ICONIFIED ->
              remove_from_cache();
              dialog_project_properties <- (id, (window, widget, `VISIBLE)) :: dialog_project_properties;
              if show then begin
                Gaux.may page_num ~f:widget#goto_page;
                window#present();
              end
            | window, widget, state ->
              (*widget#reset();*)
              if show then begin
                Gaux.may page_num ~f:widget#goto_page;
                window#present();
              end
        end;
      with Not_found ->
        let window, widget = Project_properties.create ~editor ?page_num ~show () in
        ignore (window#connect#destroy ~callback:(fun _ -> remove_from_cache()));
        ignore (window#misc#connect#hide ~callback:begin fun () ->
          toolbar#update current_project#get;
          widget#reset();
        end);
        ignore (window#event#connect#delete ~callback:begin fun ev ->
          window#misc#hide();
          true
        end);
        ignore (window#event#connect#window_state ~callback:begin fun ev ->
          begin
            match GdkEvent.WindowState.new_window_state ev with
              | [`ICONIFIED] ->
                remove_from_cache();
                dialog_project_properties <- (id, (window, widget, `ICONIFIED)) :: dialog_project_properties;
              | _ ->
                remove_from_cache();
                dialog_project_properties <- (id, (window, widget, `VISIBLE)) :: dialog_project_properties;
          end;
          false
        end);
        dialog_project_properties <- (id, (window, widget, `VISIBLE)) :: dialog_project_properties;
    end

  method dialog_project_new () =
    let rec mkname n =
      if n = 100 then (failwith "dialog_project_new (mkname)");
      let name = sprintf "Untitled_%d" n in
      if not (Sys.file_exists (Oe_config.user_home // name)) then name else (mkname (n + 1))
    in
    let name = mkname 0 in
    let filename = Oe_config.user_home // name // (name^Project.extension) in
    let new_project = Project.create ~filename () in
    ignore (Project_properties.create ~show:true ~editor ~new_project ~callback:begin fun proj ->
      self#project_close();
      Project.save ~editor new_project;
      ignore (self#project_open (Project.filename proj));
    end ());

  method dialog_file_new = Dialog_file_new.show ~editor:self#editor;

  method dialog_external_tools () =
    self#with_current_project begin fun project ->
      External_tools.create
        ~get_editor:(fun () -> self#editor)
        ~get_current_project:(fun () -> project) ()
    end

  method dialog_find_file ?all () =
    self#with_current_project begin fun project ->
      let current = project.Project_type.root in
      let roots = project_history.File_history.content in (* project filenames .xml *)
      let roots = List.filter ((<>) current) (List.map Filename.dirname roots) in
      let roots = current :: roots in
      Dialog_find_file.create ?all ~roots ~editor ()
    end

  method menubar_visible = menubar_visible
  method set_menubar_visible x =
    if not x then menubarbox#misc#hide() else (menubarbox#misc#show());
    menubar_visible <- x;
    menubar_visibility_changed#call x;

  method toolbar_visible = toolbar_visible
  method set_toolbar_visible x =
    if not x then toolbar#misc#hide() else (toolbar#misc#show_all());
    toolbar_visible <- x;
    toolbar_visibility_changed#call toolbar_visible;

  method outline_visible = outline_visible
  method set_outline_visible x =
    editor#set_show_outline x;
    outline_visible <- x;
    outline_visibility_changed#call x;

  method set_tabbar_visible x =
    editor#set_show_tabs x;
    tabbar_visible <- x;
    tabbar_visibility_changed#call x;

  method set_vmessages_visible x =
    Messages.vmessages#set_visible x;
    vmessages_visibility_changed#call x;

  method set_hmessages_visible x =
    Messages.hmessages#set_visible x;
    hvmessages_visibility_changed#call x;

  method private set_geometry () =
    let alloc = window#misc#allocation in
    geometry <- sprintf "%d\n%d\n%d\n%d\n%b\n%b\n%b\n%b\n"
      (alloc.Gtk.width) (alloc.Gtk.height) (alloc.Gtk.x) (alloc.Gtk.y)
      menubar_visible editor#show_tabs toolbar_visible outline_visible;

  method set_title ed =
    let filename = match ed#get_page `ACTIVE with None -> "" | Some page -> page#get_filename in
    let text =
      (Printf.sprintf "%s • %s"
        (match self#current_project#get with Some p -> p.Project_type.name | _ -> "")
        filename
      ) in
    window_title_menu_label#set_label (sprintf "<b>%s</b>" text);
    window#set_title text

  method set_maximized_view (view : [ `FIRST | `NONE | `SECOND ]) =
    let mb = menubar_visible in
    let tb = toolbar_visible in
    let tab = tabbar_visible in
    let ms = Messages.vmessages#visible in
    let fs = is_fullscreen in
    let save_default () =
      self#set_geometry();
      maximized_view_actions <- (`NONE, (fun () -> {
        mva_menubar    = mb;
        mva_toolbar    = tb;
        mva_tabbar     = tab;
        mva_messages   = ms;
        mva_fullscreen = fs;
      })) :: (List.remove_assoc `NONE maximized_view_actions);
    in
    let reset_default () =
      let original = (List.assoc `NONE maximized_view_actions) () in
      self#set_menubar_visible original.mva_menubar;
      self#set_toolbar_visible original.mva_toolbar;
      self#set_tabbar_visible original.mva_tabbar;
      self#set_vmessages_visible original.mva_messages;
      self#set_fullscreen original.mva_fullscreen;
      maximized_view_action <- `NONE
    in
    begin
      match view with
        | `FIRST when maximized_view_action = `NONE ->
          save_default();
          let first = (List.assoc `FIRST maximized_view_actions) () in
          self#set_menubar_visible first.mva_menubar;
          self#set_toolbar_visible first.mva_toolbar;
          self#set_tabbar_visible first.mva_tabbar;
          if Messages.vmessages#visible then (self#set_vmessages_visible first.mva_messages);
          self#set_fullscreen first.mva_fullscreen;
          maximized_view_action <- `FIRST;
        | `SECOND when maximized_view_action = `NONE ->
          save_default();
          let second = (List.assoc `SECOND maximized_view_actions) () in
          self#set_menubar_visible second.mva_menubar;
          self#set_toolbar_visible second.mva_toolbar;
          self#set_tabbar_visible second.mva_tabbar;
          if Messages.vmessages#visible then (self#set_vmessages_visible second.mva_messages);
          self#set_fullscreen second.mva_fullscreen;
          maximized_view_action <- `SECOND;
        | `FIRST when maximized_view_action = `SECOND ->
          reset_default();
          self#set_maximized_view view
        | `SECOND when maximized_view_action = `FIRST ->
          reset_default();
          self#set_maximized_view view
        | `FIRST when maximized_view_action = `FIRST -> reset_default()
        | `SECOND when maximized_view_action = `SECOND -> reset_default()
        | `NONE when maximized_view_action = `FIRST -> reset_default();
        | `NONE when maximized_view_action = `SECOND -> reset_default();
        | `NONE when maximized_view_action = `NONE -> ()
    end;
    editor#with_current_page (fun p -> p#view#misc#grab_focus())

  method set_fullscreen x =
    if not is_max_height then begin
      if x && (not is_fullscreen) then begin
        pref_max_view_fullscreen <- Preferences.preferences#get.Preferences.pref_max_view_fullscreen;
        if Preferences.preferences#get.Preferences.pref_max_view_fullscreen then begin
          if Oe_config.is_win32 then (window#set_decorated false);
          window#fullscreen();
          window_title_menu_label#misc#show();
        end else (window#maximize());
      end else if (not x) && is_fullscreen then begin
        if pref_max_view_fullscreen then begin
          window#unfullscreen();
          if Oe_config.is_win32 then (window#set_decorated true);
          window_title_menu_label#misc#hide();
        end else (window#unmaximize());
      end;
      is_fullscreen <- x;
    end

(*  method set_max_height x =
    if x && (not is_max_height) then begin
      window#set_decorated false;
      let alloc = window#misc#allocation in
      let x, y = Gdk.Window.get_position window#misc#window in
      max_height_prev_y <- y;
      max_height_prev_h <- alloc.Gtk.height;
      window#move ~x ~y:0;
      window#resize ~width:(alloc.Gtk.width) ~height:(Gdk.Screen.height ());
    end else if (not x) && is_max_height then begin
      if max_height_prev_y >= 0 then begin
        let alloc = window#misc#allocation in
        let x, y = Gdk.Window.get_position window#misc#window in
        window#resize ~width:(alloc.Gtk.width) ~height:max_height_prev_h;
        window#move ~x ~y:max_height_prev_y;
        max_height_prev_y <- -1;
        max_height_prev_h <- -1;
        window#set_decorated true;
      end
    end;
    is_max_height <- x;*)

  method set_menu_item_nav_history_sensitive () =
    let back, forward, last = editor#location_history_is_empty () in
    (*Location_history.print editor#location_history;
    eprintf "%d, %d, %d, %b\n%!"
      (List.length (Location_history.get_history_backward editor#location_history))
      (List.length (Location_history.get_history_forward editor#location_history))
      (Location_history.current_index editor#location_history)
      last;*)
    (!get_menu_item_nav_history_backward())#misc#set_sensitive (not back);
    (!get_menu_item_nav_history_forward())#misc#set_sensitive (not forward);
    (!get_menu_item_nav_history_last())#misc#set_sensitive (not last);
    toolbar#tool_back#misc#set_sensitive (not back);
    toolbar#tool_forward#misc#set_sensitive (not forward);
    toolbar#tool_last_edit_loc#misc#set_sensitive (not last);

  method check_for_updates ?(verbose=true) () =
    Check_for_updates.dialog ~verbose
      ~current_version:Oe_config.version ~title:Oe_config.title ()

  method goto_location dir =
    let move = function
      | `NEXT ->
        begin
          match Location_history.next editor#location_history with
            | None -> ()
            | Some entry -> editor#location_history_goto entry
        end
      | `PREV ->
        begin
          match Location_history.previous editor#location_history with
            | None -> ()
            | Some entry -> editor#location_history_goto entry
        end
      | `LOCATION location ->
        Location_history.goto editor#location_history ~location;
        editor#location_history_goto location
      | `LAST ->
        begin
          match Location_history.goto_last_edit_location editor#location_history with
            | None -> ()
            | Some loc -> editor#location_history_goto loc
        end
    in
    begin
      match dir with
        | `LAST -> move dir
        | `LOCATION _ -> move dir
        | _ ->
          begin
            match editor#get_page `ACTIVE with
              | None -> move dir
              | Some page ->
                let iter = page#buffer#get_iter `INSERT in
                let current_editor_location = iter#offset in
                begin
                  match Location_history.current_location editor#location_history with
                    | Some cur_hist_loc when (cur_hist_loc.Location_history.filename = page#get_filename) ->
                      let already_there =
                        match cur_hist_loc.Location_history.mark with
                          | None -> current_editor_location = cur_hist_loc.Location_history.offset
                          | Some cur_hist_mark ->
                            let hmn = match GtkText.Mark.get_name cur_hist_mark with None -> assert false | Some x -> x in
                            List.exists begin fun m ->
                              match GtkText.Mark.get_name m with
                                | None -> false
                                | Some name -> hmn = name
                            end iter#marks
                      in
                      if already_there then (move dir) else (move (`LOCATION cur_hist_loc));
                    | Some chl -> move (`LOCATION chl);
                    | None -> ()
                end;
          end;
    end;
    self#set_menu_item_nav_history_sensitive();

  method build_all configs =
    let tasks = List.map begin fun bconf ->
      let cmd, args = Bconf.create_cmd_line bconf in
      let name = sprintf "Build \xC2\xAB%s\xC2\xBB" (Filename.basename bconf.Bconf.name) in
      let task = Task.create ~name ~env:[] ~dir:"" ~cmd ~args () in
      `COMPILE, task
    end configs in
    self#with_current_project (fun project -> ignore (Bconf_console.exec_sync ~editor tasks))

  method annot_type () =
    editor#with_current_page begin fun page ->
      let iter = `ITER (page#buffer#get_iter `INSERT) in
      page#annot_type#popup (*~position:`TOP_RIGHT*) iter ();
      if Preferences.preferences#get.Preferences.pref_err_tooltip then (page#error_indication#tooltip ~sticky:true iter)
    end

  method annot_type_copy () =
    editor#with_current_page begin fun page ->
      match page#annot_type#get (`ITER (page#buffer#get_iter `INSERT)) with
        | Some (_, _, type_annot) ->
          self#annot_type();
          let clipboard = GData.clipboard Gdk.Atom.clipboard in
          clipboard#set_text type_annot;
        | _ -> ()
    end

  method annot_type_set_tooltips x =
    Preferences.preferences#get.Preferences.pref_annot_type_tooltips_enabled <- x;
    Preferences.save();
    editor#with_current_page (fun page -> page#annot_type#remove_tag())

  method create_menu_history dir ~menu =
    let history = match dir with
      | `BACK -> Location_history.get_history_backward editor#location_history
      | `FORWARD -> Location_history.get_history_forward editor#location_history
    in
    try
      List.iter begin (*let i = ref 0 in*) fun entry ->
        let label = Location_history.string_of_location entry in
        let label = if entry.Location_history.kind = `EDIT then label ^ "*" else label in
        let mi = GMenu.menu_item ~label ~packing:menu#add () in
        ignore (mi#connect#activate ~callback:(fun () -> self#goto_location (`LOCATION entry)));
        (*if !i >= 30 then (raise Exit) else (incr i)*)
      end history;
      if List.length history = 0 then begin
        try
          let sep = List.hd (List.tl menu#all_children) in
          menu#remove sep
        with _ -> ()
      end
    with Exit -> ()

  method find_and_replace
      ?(find_all=false)
      ?(search_word_at_cursor=(Preferences.preferences#get.Preferences.pref_search_word_at_cursor))
      () =
    self#with_current_project begin fun project ->
      editor#with_current_page begin fun page ->
        let buffer = page#view#buffer in
        let dialog, page = Find_text_dialog.create ~buffer ~editor:editor
          ~project ~find_all ~search_word_at_cursor ()
        in
        let hbox = GPack.hbox ~spacing:3 () in
        let icon = GMisc.image ~pixbuf:Icons.search_results_16 ~packing:hbox#pack () in
        let label = GMisc.label ~packing:hbox#pack () in
        ignore (page#connect#search_started ~callback:begin fun () ->
          if page#misc#parent = None then
            (ignore (Messages.vmessages#append_page ~label_widget:hbox#coerce page#as_page));
          label#set_text page#text_to_find;
          page#present ();
        end);
        ignore (page#connect#search_finished ~callback:begin fun () ->
          page#active#set false;
          page#present();
        end);
        if not find_all then (dialog#show())
      end
    end

  method find_next () =
    editor#with_current_page begin fun page ->
      try
       Find_text_in_buffer.find Find_text.Forward
         ~view:page#view ~canceled:(fun () -> false)
      with Find_text.No_current_regexp -> self#find_and_replace()
    end

  method search_again () =
    try
      editor#with_current_page begin fun page ->
        Find_text_in_buffer.find Find_text.status.Find_text.direction
         ~view:page#view ~canceled:(fun () -> false)
      end
    with Find_text.No_current_regexp -> self#find_and_replace()

  method with_current_project f = match current_project#get with Some p -> f p | _ -> ()

  method with_default_build_config f =
    match current_project#get with
      | Some project ->
        begin
          match Project.default_build_config project with
            | Some bc -> f bc
            | _ -> ()
        end;
      | _ -> ()

  method with_default_runtime_config f =
    match current_project#get with
      | Some project ->
        begin
          match List_opt.find (fun x -> x.Rconf.default) project.Project_type.runtime with
            | Some rconf -> f rconf
            | _ ->
              self#dialog_project_properties ~page_num:2 ()
        end;
      | _ -> ()

  method window : GWindow.window = window

  method exit (editor : Editor.editor) () =
    if maximized_view_action = `NONE then (self#set_geometry());
    (* Save geometry *)
    let chan = open_out (Filename.concat Oe_config.ocamleditor_user_home "geometry") in
    fprintf chan "%s" geometry;
    close_out_noerr chan;
    window#misc#hide();
    (*  *)
    let finalize () =
      try
        ignore(Messages.vmessages#remove_all_tabs());
        (* Save project *)
        begin
          try self#with_current_project begin fun project ->
            Project.save ~editor project;
            Symbol.Cache.save ~project;
          end with Gpointer.Null -> ();
        end;
        (* Save project and file history *)
        File_history.write editor#file_history;
        self#project_write_history();
        (*  *)
        finalize();
        GMain.Main.quit();
        Pervasives.exit 0
      with Messages.Cancel_process_termination -> (GtkSignal.stop_emit())
    in
    let pages = List.filter (fun p -> p#buffer#modified) editor#pages in
    let pages = List.map (fun x -> true, x) pages in
    editor#dialog_save_modified ~close:false ~callback:finalize pages

  method private init () =
    let _ = Editor.set_menu_item_nav_history_sensitive := self#set_menu_item_nav_history_sensitive in
    (** Menubar items *)
    let menu_item_view_menubar = ref [] in
    let menu_item_view_toolbar = ref [] in
    let menu_item_view_tabbar = ref [] in
    let menu_item_view_outline = ref [] in
    let menu_item_view_messages = ref [] in
    let menu_item_view_hmessages = ref [] in
    let group = accel_group in
    let menu_items = Menu.create ~browser:self ~group
      ~get_menu_item_undo
      ~get_menu_item_redo
      ~get_menu_item_nav_history_backward
      ~get_menu_item_nav_history_forward
      ~get_menu_item_nav_history_last
      ~menu_item_view_menubar
      ~menu_item_view_toolbar
      ~menu_item_view_tabbar
      ~menu_item_view_outline
      ~menu_item_view_messages ~menu_item_view_hmessages () in
    menu <- Some menu_items;
    List.iter menubar#prepend menu_items.Menu.menu_items;
    (** Update Window menu with files added to the editor *)
    ignore (editor#connect#add_page ~callback:begin fun page ->
      begin
        match page#file with None -> () | Some file ->
          let offset = page#initial_offset in
          let scroll_offset = page#view#get_scroll_top () in
          self#with_current_project (fun project -> Project.add_file project ~scroll_offset ~offset file);
      end;
      Gaux.may menu ~f:begin fun menu ->
        menu.Menu.window_signal_locked <- true;
        let basename = Filename.basename page#get_filename in
        let label = sprintf "%s%s" basename (if page#buffer#modified then "*" else "") in
        let group = menu.Menu.window_radio_group in
        let item = GMenu.radio_menu_item ?group ~active:true
          ~label ~packing:(menu.Menu.window#insert ~pos:menu.Menu.window_n_childs) ()
        in
        menu.Menu.window_n_childs <- menu.Menu.window_n_childs + 1;
        let _ = item#connect#toggled ~callback:begin fun () ->
          if not menu.Menu.window_signal_locked then begin
            ignore (editor#open_file ~active:true ~scroll_offset:0 ~offset:0 page#get_filename)
          end
        end in
        menu.Menu.window_pages <- (page#misc#get_oid, item) :: menu.Menu.window_pages;
        menu.Menu.window_signal_locked <- false;
        menu.Menu.window_radio_group <- Some item#group;
      end;
    end);
    (** Update Window menu with files removed from the editor *)
    ignore (editor#connect#remove_page ~callback:begin fun page ->
      self#with_current_project begin fun project ->
        Project.remove_file project page#get_filename;
        window#set_title (Convert.to_utf8 project.name);
        Gaux.may menu ~f:begin fun menu ->
          try
            let item = List.assoc page#misc#get_oid menu.Menu.window_pages in
            item#destroy();
            menu.Menu.window_pages <- List.remove_assoc page#misc#get_oid menu.Menu.window_pages;
	    menu.Menu.window_n_childs <- menu.Menu.window_n_childs - 1;
          with Not_found -> ()
        end;
      end
    end);
    (**  *)
    ignore (editor#connect#switch_page ~callback:begin fun page ->
      Gaux.may menu ~f:begin fun menu ->
        let basename = Filename.basename page#get_filename in
        kprintf (Menu.set_label menu.Menu.file_rename) "Rename \xC2\xAB%s\xC2\xBB" basename;
        menu.Menu.file_switch#misc#set_sensitive
          ((basename ^^ ".ml") || (basename ^^ ".mli"));
        kprintf (Menu.set_label menu.Menu.file_close) "Close \xC2\xAB%s\xC2\xBB" basename;
        kprintf (Menu.set_label menu.Menu.file_close_all) "Close All Except \xC2\xAB%s\xC2\xBB" basename;
        kprintf (Menu.set_label menu.Menu.file_revert) "Revert to Saved \xC2\xAB%s\xC2\xBB" basename;
        kprintf (Menu.set_label menu.Menu.file_delete) "Delete \xC2\xAB%s\xC2\xBB" basename;
        match List_opt.assoc page#misc#get_oid menu.Menu.window_pages with
          | Some item ->kprintf (Menu.set_label item) "%s" basename
          | _ -> ()
      end;
    end);
    (** Update Project menu with project history *)
    ignore (self#connect#project_history_changed ~callback:begin fun history ->
      Gaux.may menu ~f:begin fun menu ->
        Gmisclib.Idle.add ~prio:600 begin fun () ->
          List.iter (fun (_, i) -> menu.Menu.project#remove (i :> GMenu.menu_item)) menu.Menu.project_history;
          menu.Menu.project_history <- []
        end;
        let project_names = List.map (fun x -> x, Filename.chop_extension (Filename.basename x)) history.File_history.content in
        let project_names = List.sort (fun (_, x1) (_, x2) ->
          compare (String.lowercase x1) (String.lowercase x2)) project_names in
        List.iter begin fun (filename, label) ->
          Gmisclib.Idle.add ~prio:600 begin fun () ->
            let item = GMenu.check_menu_item ~label ~packing:menu.Menu.project#add () in
            ignore (item#connect#after#toggled ~callback:(fun () ->
              if not menu.Menu.project_history_signal_locked && item#active then (ignore (self#project_open filename))));
            menu.Menu.project_history <- (filename, item) :: menu.Menu.project_history;
          end;
        end project_names;
      end
    end);

    Editor_menu.menu_item_view_menubar := (fun () -> menu_item_view_menubar);

    (** Load current project *)
    let rec load_current_proj history =
      match history with [] -> () | filename :: _ ->
        if Sys.file_exists filename then (ignore (self#project_open filename))
        else (load_current_proj (List.tl history))
    in
    File_history.read project_history;
    project_history.File_history.content <- List.filter (fun x -> (x ^^ Project.extension)) project_history.File_history.content;
    project_history_changed#call project_history;
    load_current_proj project_history.File_history.content;

    (** Toolbar signals *)
    toolbar#bind_signals self;

    (**  *)
    self#set_menu_item_nav_history_sensitive();
    self#connect#menubar_visibility_changed ~callback:begin fun visible ->
      List.iter begin fun (mi, sign) ->
        mi#misc#handler_block sign;
        mi#set_active visible;
        mi#misc#handler_unblock sign;
      end !menu_item_view_menubar
    end;
    self#connect#toolbar_visibility_changed ~callback:begin fun visible ->
      List.iter begin fun (mi, sign) ->
        mi#misc#handler_block sign;
        mi#set_active visible;
        mi#misc#handler_unblock sign;
      end !menu_item_view_toolbar
    end;
    self#connect#tabbar_visibility_changed ~callback:begin fun visible ->
      List.iter begin fun (mi, sign) ->
        mi#misc#handler_block sign;
        mi#set_active visible;
        mi#misc#handler_unblock sign;
      end !menu_item_view_tabbar
    end;
    self#connect#outline_visibility_changed ~callback:begin fun visible ->
      List.iter begin fun (mi, sign) ->
        mi#misc#handler_block sign;
        mi#set_active visible;
        mi#misc#handler_unblock sign;
      end !menu_item_view_outline
    end;
    let update_view_vmessages_items visible =
      List.iter begin fun (mi, sign) ->
        toolbar#tool_messages_handler_block ();
        mi#misc#handler_block sign;
        mi#set_active visible;
        toolbar#tool_messages_set_active visible;
        mi#misc#handler_unblock sign;
        toolbar#tool_messages_handler_unblock ();
      end !menu_item_view_messages
    in
    Messages.vmessages#connect#visible_changed ~callback:update_view_vmessages_items;
    self#connect#vmessages_visibility_changed ~callback:update_view_vmessages_items;
    let update_view_hmessages_items visible =
      List.iter begin fun (mi, sign) ->
        toolbar#tool_hmessages_handler_block ();
        mi#misc#handler_block sign;
        mi#set_active visible;
        toolbar#tool_hmessages_set_active visible;
        mi#misc#handler_unblock sign;
        toolbar#tool_hmessages_handler_unblock ();
      end !menu_item_view_hmessages
    in
    Messages.hmessages#connect#visible_changed ~callback:update_view_hmessages_items;
    self#connect#hvmessages_visibility_changed ~callback:update_view_hmessages_items;

    (** Editor *)
    paned#pack1 ~resize:true ~shrink:true editor#coerce;
    let update_toolbar_save () =
      toolbar#tool_save_all#misc#set_sensitive (List.exists (fun p -> p#view#buffer#modified) editor#pages);
      Gaux.may (editor#get_page `ACTIVE) ~f:begin fun page ->
        toolbar#tool_save#misc#set_sensitive page#buffer#modified;
      end;
    in
    let update_toolbar_undo () =
      Gaux.may (editor#get_page `ACTIVE) ~f:begin fun page ->
        let can_undo = (*page#buffer#modified &&*) page#buffer#undo#can_undo in
        let can_redo = (*page#buffer#modified &&*) page#buffer#undo#can_redo in
        toolbar#tool_undo#misc#set_sensitive can_undo;
        toolbar#tool_redo#misc#set_sensitive can_redo;
        (!get_menu_item_undo())#misc#set_sensitive can_undo;
        (!get_menu_item_redo())#misc#set_sensitive can_redo;
      end;
    in
    let callback _ =
      toolbar#update current_project#get;
      update_toolbar_save();
      update_toolbar_undo();
      Gaux.may (editor#get_page `ACTIVE) ~f:begin fun page ->
        self#set_title editor;
      end;
      self#set_menu_item_nav_history_sensitive();
    in
    ignore (editor#connect#switch_page ~callback);
    ignore (editor#connect#remove_page ~callback);
    ignore (editor#connect#add_page ~callback);
    ignore (editor#connect#after#changed ~callback:update_toolbar_undo);
    ignore (editor#connect#after#modified_changed ~callback:update_toolbar_save);
    ignore (editor#connect#file_history_changed ~callback:begin fun fh ->
      Gaux.may menu ~f:begin fun menu ->
        let f =
          if List.length fh.File_history.content > 0
          then (fun x -> x#misc#show()) else (fun x -> x#misc#hide())
        in
        List.iter f [
          menu.Menu.file_recent_select;
          menu.Menu.file_recent_clear;
          menu.Menu.file_recent_sep;
        ]
      end;
    end);
    ignore (self#connect#switch_project ~callback:begin fun () ->
      toolbar#update current_project#get;
      self#set_menu_item_nav_history_sensitive()
    end);
    List.iter (fun c -> c#misc#set_sensitive (current_project#get <> None)) toolbar#children;
    callback();
    (* Initialize Quick_file_chooser *)
    let roots = List.map Filename.dirname project_history.File_history.content in
    Quick_file_chooser.init ~roots ~filter:Dialog_find_file.filter;
    (** Geometry settings *)
    let screen = window#screen in
    let height = ref 650 in
    let width = ref 1052 in
    let pos_x = ref None in
    let pos_y = ref None in
    let is_menubar_visible = ref true in
    let is_toolbar_visible = ref true in
    let is_tabbar_visible = ref true in
    let is_outline_visible = ref true in
    begin
      try
        let chan = open_in (Filename.concat Oe_config.ocamleditor_user_home "geometry") in
        width := int_of_string (input_line chan);
        height := int_of_string (input_line chan);
        pos_x := Some (int_of_string (input_line chan));
        pos_y := Some (int_of_string (input_line chan));
        (try is_menubar_visible := (bool_of_string (input_line chan)) with End_of_file -> ());
        (try is_tabbar_visible := (bool_of_string (input_line chan)) with End_of_file -> ());
        (try is_toolbar_visible := (bool_of_string (input_line chan)) with End_of_file -> ());
        (try is_outline_visible := (bool_of_string (input_line chan)) with End_of_file -> ());
        close_in chan;
      with _ -> ()
    end;
    self#set_menubar_visible !is_menubar_visible;
    self#set_toolbar_visible !is_toolbar_visible;
    self#set_tabbar_visible !is_tabbar_visible;
    self#set_outline_visible !is_outline_visible;
    window#resize ~width:!width ~height:!height;
    window#show();
    Messages.vmessages#set_position (!height * 7 / 10);
    Messages.hmessages#set_position (!width * 7 / 10);
    ignore (window#event#connect#after#delete ~callback:(fun _ -> self#exit editor (); true));
    (*  *)
    Ocaml_text.create_shell := self#shell;
    (* Check for updates at startup *)
    if Preferences.preferences#get.Preferences.pref_check_updates then begin
      Gmisclib.Idle.add (fun () -> self#check_for_updates ~verbose:false ())
    end;
    (* Load custom templates *)
    begin
      try Templ.load_custom `user;
      with Templ.Error (msg, details) ->
        (Dialog.message ~title:"Warning" ~message:(sprintf "%s\n\n\"%s\"" msg details) `WARNING);
    end;
    (* Focus on active text view *)
    Gaux.may (editor#get_page `ACTIVE) ~f:(fun page -> page#view#misc#grab_focus());
    (*  *)
    self#set_geometry();
    window#add_accel_group accel_group;

  method connect = new signals ~switch_project ~menubar_visibility_changed ~toolbar_visibility_changed
    ~tabbar_visibility_changed ~outline_visibility_changed
    ~vmessages_visibility_changed ~hvmessages_visibility_changed
    ~project_history_changed

  initializer self#init()

end

and switch_project () = object (self) inherit [unit] signal () as super end
and menubar_visibility_changed () = object (self) inherit [bool] signal () as super end
and toolbar_visibility_changed () = object (self) inherit [bool] signal () as super end
and tabbar_visibility_changed () = object (self) inherit [bool] signal () as super end
and outline_visibility_changed () = object (self) inherit [bool] signal () as super end
and vmessages_visibility_changed () = object (self) inherit [bool] signal () as super end
and hvmessages_visibility_changed () = object (self) inherit [bool] signal () as super end
and project_history_changed () = object (self) inherit [File_history.t] signal () as super end
and signals ~switch_project ~menubar_visibility_changed ~toolbar_visibility_changed
  ~tabbar_visibility_changed ~outline_visibility_changed
  ~vmessages_visibility_changed ~hvmessages_visibility_changed
  ~project_history_changed =
object (self)
  inherit ml_signals [switch_project#disconnect; menubar_visibility_changed#disconnect;
    toolbar_visibility_changed#disconnect; tabbar_visibility_changed#disconnect;
    vmessages_visibility_changed#disconnect; project_history_changed#disconnect ]
  method switch_project = switch_project#connect ~after
  method menubar_visibility_changed = menubar_visibility_changed#connect ~after
  method toolbar_visibility_changed = toolbar_visibility_changed#connect ~after
  method tabbar_visibility_changed = tabbar_visibility_changed#connect ~after
  method outline_visibility_changed = outline_visibility_changed#connect ~after
  method vmessages_visibility_changed = vmessages_visibility_changed#connect ~after
  method hvmessages_visibility_changed = hvmessages_visibility_changed#connect ~after
  method project_history_changed = project_history_changed#connect ~after
end

let browser = begin
  Project_xml.init();
  Sys.chdir (Filename.dirname Sys.executable_name);
  (*GtkMain.Main.disable_setlocale();*)
  (*Unix.putenv "LANGUAGE" "C";*)
  (*Unix.putenv "GTK_SETLOCALE" "0";*)
  (*let locale = Glib.Main.setlocale `ALL (Some "C") in*)
  let locale = GtkMain.Main.init ~setlocale:false () in
  let window = GWindow.window
    ~title:Oe_config.title
    ~icon:Icons.oe
    ~type_hint:`NORMAL
    ~kind:`TOPLEVEL
    ~allow_shrink:true
    ~show:false
    ()
  in
  new browser window;
end
















