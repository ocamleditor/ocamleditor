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

open Project
open GdkKeysyms
open Printf
open Miscellanea
open GUtil
open Browser_types

class browser window =
  let switch_project = new switch_project () in
  let menubar_visibility_changed = new menubar_visibility_changed () in
  let toolbar_visibility_changed = new toolbar_visibility_changed () in
  let tabbar_visibility_changed = new tabbar_visibility_changed () in
  let messages_visibility_changed = new messages_visibility_changed () in
  let vbox = GPack.vbox ~packing:window#add () in
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let messages = Messages.messages in
  let editor = new Editor.editor () in
  let toolbar = new Toolbar.toolbar ~messages ~editor () in
  let _ = vbox#pack toolbar#coerce in
  let paned = Messages.paned in
  let _ = vbox#add paned#coerce in
  (*  *)
  let accel_group = GtkData.AccelGroup.create () in
  let _ = window#add_accel_group accel_group in
  let get_menu_item_nav_history_backward = ref (fun () -> failwith "get_menu_item_nav_history_backward") in
  let get_menu_item_nav_history_forward = ref (fun () -> failwith "get_menu_item_nav_history_forward") in
  let get_menu_item_nav_history_last = ref (fun () -> failwith "get_menu_item_nav_history_last") in
  let get_menu_item_undo = ref (fun () -> failwith "get_menu_item_undo") in
  let get_menu_item_redo = ref (fun () -> failwith "get_menu_item_redo") in

object (self)
  val mutable finalize = fun _ -> ()
  val mutable projects = []
  val mutable current_project = None
  val mutable menubar_visible = true;
  val mutable toolbar_visible = true;
  val mutable tabbar_visible = true;
  val mutable project_history =
    File_history.create
      ~filename:Oe_config.project_history_filename
      ~max_length:Oe_config.project_history_max_length
  method connect = new signals ~switch_project ~menubar_visibility_changed ~toolbar_visibility_changed
    ~tabbar_visibility_changed ~messages_visibility_changed
  method editor = editor
  method messages = messages
  method window : GWindow.window = window
  method project_history = project_history

  method set_title ed =
    let filename = match ed#get_page Editor_types.Current with None -> "" | Some page -> page#get_filename in
    let text =
      (Printf.sprintf "%s • %s"
        (try self#current_project.name with No_current_project -> "")
        filename
      ) in
    window#set_title text

  method exit (editor : Editor.editor) () =
    self#set_maximized_view `NONE ();
    let finalize () =
      try
        ignore(messages#remove_all_tabs());
        (* Save the project *)
        begin
          try
            let proj = self#current_project in
            Project.save ~editor proj;
          with No_current_project | Gpointer.Null -> ()
        end;
        (* Save project and file history *)
        File_history.write editor#file_history;
        File_history.write project_history;
        (* Save geometry *)
        let alloc = window#misc#allocation in
        let chan = open_out (Filename.concat Oe_config.ocamleditor_user_home "geometry") in
        fprintf chan "%d\n%d\n%d\n%d\n%b\n%b\n%b\n%!" (alloc.Gtk.width) (alloc.Gtk.height) (alloc.Gtk.x) (alloc.Gtk.y)
          menubar_visible editor#show_tabs toolbar_visible;
        close_out chan;
        (*  *)
        finalize();
        GMain.Main.quit();
        Pervasives.exit 0
      with Messages.Cancel_process_termination -> (GtkSignal.stop_emit())
    in
    let pages = List.filter (fun p -> p#buffer#modified) editor#pages in
    let pages = List.map (fun x -> true, x) pages in
    editor#dialog_save_modified ~close:false ~callback:finalize pages

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

  method shell () =
    messages#add_ocaml_shell ~project:self#current_project ();
    messages#set_visible true;

  method save_all () =
    editor#save_all();
    (try Project.save ~editor self#current_project with No_current_project -> ())

  method private set_current_project proj =
    (* Previous project *)
    begin try
      let previous = self#current_project in
      Project.save ~editor previous;
      File_history.write project_history;
      Project.unload_path previous Config.load_path;
      List.iter (fun p -> Autosave.delete ~filename:p#get_filename ()) editor#pages;
    with No_current_project -> () end;
    (* Load current project path *)
    Project.load_path proj Config.load_path;
    (* *)
    current_project <- Some proj;
    editor#set_project self#current_project;
    Sys.chdir (proj.root // Project.src);
    switch_project#call();
    window#set_title (Convert.to_utf8 proj.name);

  method refresh () = Project.refresh self#current_project

  method current_project =
    match current_project with None -> raise No_current_project | Some p -> p

  method dialog_project_open () =
    let project_home = try self#current_project.root
      with No_current_project -> Oe_config.user_home
    in
    let pat1 = "*"^Project.extension in
    let pat2 = "*" ^ Project_old_1.project_name_extension in
    let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~width:600 ~height:600
      ~title:"Open project..." ~icon:Icons.oe ~position:`CENTER ~show:false () in
    dialog#add_filter (GFile.filter
      ~name:(sprintf "%s projects (%s)" Oe_config.title pat1) ~patterns:[pat1] ());
    dialog#add_filter (GFile.filter
      ~name:(sprintf "%s projects ver. 1.1 (%s)" Oe_config.title pat2) ~patterns:[pat2] ());
    dialog#add_select_button_stock `OK `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#set_select_multiple false;
    dialog#set_current_folder (Filename.dirname project_home);
    match dialog#run () with
      | `OK ->
        List.iter (fun filename -> self#project_open filename) dialog#get_filenames;
        dialog#destroy()
      | _ -> dialog#destroy()

  method project_open filename =
    let proj = Project.load filename in
    File_history.add project_history filename;
    self#set_current_project proj;
    editor#close_all ();
    Autosave.recover();
    (* Load files *)
    let active_exists = ref false in
    let i = ref 0 in
    let _ =
      List.map begin fun (filename, offset, active) ->
        incr i;
        active_exists := !active_exists || active;
        let active = active || (List.length proj.open_files = !i && not !active_exists) in
        editor#open_file ~active ~offset filename;
      end proj.open_files
    in
    proj.open_files <- [];
    proj.modified <- false;

  val mutable dialog_project_properties = None
  method dialog_project_properties ?page () =
    try
      begin
        match dialog_project_properties with
          | None ->
            let window = Project_properties.create ~editor ?page ~project:self#current_project () in
            window#connect#destroy ~callback:(fun _ -> dialog_project_properties <- None);
            window#misc#connect#hide ~callback:(fun () -> toolbar#update current_project);
            dialog_project_properties <- Some window
          | Some window -> window#present()
      end
    with No_current_project -> ()

  method dialog_project_new () =
    ignore (Project_properties.create ~editor ~callback:begin fun proj ->
      Project.save ~editor proj;
      self#project_open (Project.filename proj);
    end ());

  method dialog_file_new () =
    try
      let rec mkname n =
        if n = 1000 then (failwith "Browser#dialog_file_new (mkname)");
        let name = Filename.concat (self#current_project.root // Project.src)
          (sprintf "untitled%s.ml" (if n = 0 then "" else string_of_int n)) in
        if not (Sys.file_exists name) then name else (mkname (n + 1))
      in
      let filename = mkname 0 in
      let chan = open_out_gen [Open_creat; Open_excl; Open_text] 0o664 filename in
      close_out chan;
      editor#open_file ~active:true ~offset:0 filename;
      begin
        match editor#get_page (Editor_types.File (File.create filename ())) with
          | None -> assert false
          | Some page ->
            editor#dialog_rename page;
            self#set_title editor
      end
    with No_current_project -> ()

  method menubar_visible = menubar_visible
  method set_menubar_visible x =
    if not x then menubar#misc#hide() else (menubar#misc#show_all());
    menubar_visible <- x;
    menubar_visibility_changed#call x;

  method toolbar_visible = toolbar_visible
  method set_toolbar_visible x =
    if not x then toolbar#misc#hide() else (toolbar#misc#show_all());
    toolbar_visible <- x;
    toolbar_visibility_changed#call toolbar_visible;

  method set_tabbar_visible x =
    editor#set_show_tabs x;
    tabbar_visible <- x;
    tabbar_visibility_changed#call x;

  method set_messages_visible x =
    messages#set_visible x;
    messages_visibility_changed#call x;

  val mutable pref_max_view_2 = false
  val mutable pref_max_view_fullscreen = false
  val mutable max_height_prev_h = -1
  val mutable max_height_prev_y = -1
  val mutable is_max_height = false
  val mutable is_fullscreen = false
  val mutable maximized_view_action = `NONE
  val mutable maximized_view_actions = [
    `NONE, (fun () -> {
      mva_menubar = true; (* dummies *)
      mva_toolbar = true;
      mva_tabbar = true;
      mva_messages = false;
      mva_fullscreen = false;
    });
    `FIRST, (fun () -> {
      mva_menubar = !Preferences.preferences.Preferences.pref_max_view_1_menubar;
      mva_toolbar = !Preferences.preferences.Preferences.pref_max_view_1_toolbar;
      mva_tabbar = !Preferences.preferences.Preferences.pref_max_view_1_tabbar;
      mva_messages = !Preferences.preferences.Preferences.pref_max_view_1_messages;
      mva_fullscreen = !Preferences.preferences.Preferences.pref_max_view_1_fullscreen;
    });
    `SECOND, (fun () -> {
      mva_menubar = !Preferences.preferences.Preferences.pref_max_view_2_menubar;
      mva_toolbar = !Preferences.preferences.Preferences.pref_max_view_2_toolbar;
      mva_tabbar = !Preferences.preferences.Preferences.pref_max_view_2_tabbar;
      mva_messages = !Preferences.preferences.Preferences.pref_max_view_2_messages;
      mva_fullscreen = !Preferences.preferences.Preferences.pref_max_view_2_fullscreen;
    })
  ];

  method set_maximized_view view () =
    let mb = menubar_visible in
    let tb = toolbar_visible in
    let tab = tabbar_visible in
    let ms = messages#visible in
    let fs = is_fullscreen in
    let save_default () =
      maximized_view_actions <- (`NONE, (fun () -> {
        mva_menubar = mb;
        mva_toolbar = tb;
        mva_tabbar = tab;
        mva_messages = ms;
        mva_fullscreen = fs;
      })) :: (List.remove_assoc `NONE maximized_view_actions);
    in
    let reset_default () =
      let original = (List.assoc `NONE maximized_view_actions) () in
      self#set_menubar_visible original.mva_menubar;
      self#set_toolbar_visible original.mva_toolbar;
      self#set_tabbar_visible original.mva_tabbar;
      self#set_messages_visible original.mva_messages;
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
          if messages#visible then (self#set_messages_visible first.mva_messages);
          self#set_fullscreen first.mva_fullscreen;
          maximized_view_action <- `FIRST;
        | `SECOND when maximized_view_action = `NONE ->
          save_default();
          let second = (List.assoc `SECOND maximized_view_actions) () in
          self#set_menubar_visible second.mva_menubar;
          self#set_toolbar_visible second.mva_toolbar;
          self#set_tabbar_visible second.mva_tabbar;
          if messages#visible then (self#set_messages_visible second.mva_messages);
          self#set_fullscreen second.mva_fullscreen;
          maximized_view_action <- `SECOND;
        | `FIRST when maximized_view_action = `SECOND ->
          reset_default();
          self#set_maximized_view view ()
        | `SECOND when maximized_view_action = `FIRST ->
          reset_default();
          self#set_maximized_view view ()
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
        pref_max_view_fullscreen <- !Preferences.preferences.Preferences.pref_max_view_fullscreen;
        if !Preferences.preferences.Preferences.pref_max_view_fullscreen then begin
          if Oe_config.is_win32 then (window#set_decorated false);
          window#fullscreen();
        end else (window#maximize());
      end else if (not x) && is_fullscreen then begin
        if pref_max_view_fullscreen then begin
          window#unfullscreen();
          if Oe_config.is_win32 then (window#set_decorated true);
        end else (window#unmaximize());
      end;
      is_fullscreen <- x;
    end

  method set_max_height x =
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
    is_max_height <- x;

  method dialog_external_tools ~menu = External_tools.create
    ~menu
    ~get_editor:(fun () -> self#editor)
    ~get_current_project:(fun () -> self#current_project)()

  method dialog_find_file ?all () =
    try
      let proj = self#current_project in
      let path = Project.path_src proj in
      Dialog_find_file.create ?all ~path ~editor ()
    with No_current_project -> ()

  method check_for_updates ?(verbose=true) () = Check_for_updates.dialog ~verbose ()

  method generate_build_script = Build_script.create

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
            match editor#get_page Editor_types.Current with
              | None -> move dir
              | Some page ->
                let iter = page#buffer#get_iter `INSERT in
                let current_editor_location = iter#offset in
                begin
                  match Location_history.current_location editor#location_history with
                    | Some chl when (chl.Location_history.filename = page#get_filename) ->
                      let already_there =
                        match chl.Location_history.mark with
                          | None -> current_editor_location = chl.Location_history.offset
                          | Some hm ->
                            let hmn = match GtkText.Mark.get_name hm with None -> assert false | Some x -> x in
                            List.exists begin fun m ->
                              match GtkText.Mark.get_name m with
                                | None -> false
                                | Some name -> hmn = name
                            end iter#marks
                      in
                      if already_there then (move dir) else (move (`LOCATION chl));
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
    ignore (Bconf_console.exec_sync ~project:self#current_project ~editor tasks)

  method annot_type () =
    editor#with_current_page begin fun page ->
      let iter = `ITER (page#buffer#get_iter `INSERT) in
      page#annot_type#popup (*~position:`TOP_RIGHT*) iter ();
      if !Preferences.preferences.Preferences.pref_err_tooltip then (page#error_indication#tooltip ~sticky:true iter)
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
    !Preferences.preferences.Preferences.pref_annot_type_tooltips_enabled <- x;
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

  method get_default_build_config () = try Project.default_build_config self#current_project with No_current_project -> None

  method get_default_runtime_config () =
    try
      Some (List.find (fun x -> x.Rconf.default) self#current_project.Project.runtime)
    with Not_found | No_current_project -> None

  method find_and_replace
      ?(find_all=false)
      ?(search_word_at_cursor=(!Preferences.preferences.Preferences.pref_search_word_at_cursor))
      () =
    editor#with_current_page begin fun current ->
      let buffer = current#view#buffer in
      let finish = ref (fun _ -> ()) in
      let dialog, res = Find_text_dialog.create ~buffer ~editor:editor
        ~project:self#current_project ~find_all ~search_word_at_cursor ()
      in
      let hbox = GPack.hbox ~spacing:3 () in
      let icon = GMisc.image ~pixbuf:Icons.search_results_16 ~packing:hbox#pack () in
      let label = GMisc.label ~packing:hbox#pack () in
      res#connect#search_started ~callback:begin fun () ->
        if res#misc#parent = None then
          (finish := fst (Messages.messages#append_page "" ~label_widget:hbox#coerce res#coerce));
        label#set_text res#text_to_find;
        Messages.messages#present res#coerce;
      end;
      ignore (res#connect#search_finished ~callback:begin fun () ->
        !finish true;
        Messages.messages#present res#coerce;
      end);
      if not find_all then (dialog#show())
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

  initializer
    let _ = Editor.set_menu_item_nav_history_sensitive := self#set_menu_item_nav_history_sensitive in
    editor#connect#add_page ~callback:begin fun page ->
      match page#file with None -> () | Some file ->
        let offset = page#initial_offset in
        begin
          try Project.add_file self#current_project ~offset file;
          with No_current_project -> ()
        end
    end;
    editor#connect#remove_page ~callback:begin fun page ->
      try
        Project.remove_file self#current_project page#get_filename;
        window#set_title (Convert.to_utf8 self#current_project.name)
      with No_current_project -> ()
    end;

    (** Load current project *)
    let rec load_current_proj history =
      match history with [] -> () | filename :: _ ->
        if Sys.file_exists filename then (self#project_open filename)
        else (load_current_proj (List.tl history))
    in
    File_history.read project_history;
    load_current_proj project_history.File_history.content;

    (** Menubar items *)
    let menu_item_view_menubar = ref [] in
    let menu_item_view_toolbar = ref [] in
    let menu_item_view_tabbar = ref [] in
    let menu_item_view_messages = ref [] in
    let group = accel_group in
    let menu = Menu.create
      group
      get_menu_item_undo
      get_menu_item_redo
      get_menu_item_nav_history_backward
      get_menu_item_nav_history_forward
      get_menu_item_nav_history_last
      menu_item_view_menubar
      menu_item_view_toolbar
      menu_item_view_tabbar
      menu_item_view_messages
      self
    in
    List.iter (fun mi -> menubar#prepend (mi ())) (List.rev menu);
    editor#set_menu (List.rev menu);
    List.iter editor#populate_menu editor#pages;

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
    let update_view_messages_items visible =
      List.iter begin fun (mi, sign) ->
        toolbar#tool_messages_handler_block ();
        mi#misc#handler_block sign;
        mi#set_active visible;
        toolbar#tool_messages_set_active visible;
        mi#misc#handler_unblock sign;
        toolbar#tool_messages_handler_unblock ();
      end !menu_item_view_messages
    in
    messages#connect#visible_changed ~callback:update_view_messages_items;
    self#connect#messages_visibility_changed ~callback:update_view_messages_items;

    (** Editor *)
    paned#pack1 ~resize:true editor#coerce;
    let update_toolbar_save () =
      toolbar#tool_save_all#misc#set_sensitive (List.exists (fun p -> p#view#buffer#modified) editor#pages);
      Gaux.may (editor#get_page Editor_types.Current) ~f:begin fun page ->
        toolbar#tool_save#misc#set_sensitive page#buffer#modified;
      end;
    in
    let update_toolbar_undo () =
      Gaux.may (editor#get_page Editor_types.Current) ~f:begin fun page ->
        let can_undo = (*page#buffer#modified &&*) page#buffer#undo#can_undo in
        let can_redo = (*page#buffer#modified &&*) page#buffer#undo#can_redo in
        toolbar#tool_undo#misc#set_sensitive can_undo;
        toolbar#tool_redo#misc#set_sensitive can_redo;
        (!get_menu_item_undo())#misc#set_sensitive can_undo;
        (!get_menu_item_redo())#misc#set_sensitive can_redo;
      end;
    in
    let callback _ =
      toolbar#update current_project;
      update_toolbar_save();
      update_toolbar_undo();
      Gaux.may (editor#get_page Editor_types.Current) ~f:begin fun current ->
        self#set_title editor;
      end;
      self#set_menu_item_nav_history_sensitive();
    in
    editor#connect#switch_page ~callback;
    editor#connect#remove_page ~callback;
    editor#connect#add_page ~callback;
    editor#connect#after#changed ~callback:update_toolbar_undo;
    editor#connect#after#modified_changed ~callback:update_toolbar_save;
    self#connect#switch_project ~callback:(fun () -> toolbar#update current_project; self#set_menu_item_nav_history_sensitive());
    List.iter (fun c -> c#misc#set_sensitive (current_project <> None)) toolbar#children;
    callback();
    (* Geometry settings *)
    let screen = window#screen in
    let height = ref 600 in
    let width = ref 850 in
    let pos_x = ref None in
    let pos_y = ref None in
    let is_menubar_visible = ref true in
    let is_toolbar_visible = ref true in
    let is_tabbar_visible = ref true in
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
        close_in chan;
      with _ -> ()
    end;
    self#set_menubar_visible !is_menubar_visible;
    self#set_toolbar_visible !is_toolbar_visible;
    self#set_tabbar_visible !is_tabbar_visible;
    window#resize ~width:!width ~height:!height;
    let screen_width = Gdk.Screen.width ~screen () in
    let screen_height = Gdk.Screen.height ~screen () in
    let y = if (float_of_int !height) > (float_of_int screen_height) /. 10. *. 9.2 then 0
      else ((screen_height - !height) / 2) in
    window#move ~x:((screen_width - !width) / 2) ~y;
    window#show();
    messages#set_position (!height * 7 / 10);
    window#event#connect#delete ~callback:(fun _ -> self#exit editor (); true);
    (*  *)
    Ocaml_text.create_shell := self#shell;
    (* Check for updates at startup *)
    if !Preferences.preferences.Preferences.pref_check_updates then begin
      Gtk_util.idle_add (fun () -> self#check_for_updates ~verbose:false ())
    end;
    (* Focus on active text view *)
    Gaux.may (editor#get_page Editor_types.Current) ~f:begin fun page ->
      page#view#misc#grab_focus()
    end;
end

and switch_project () = object (self) inherit [unit] signal () as super end
and menubar_visibility_changed () = object (self) inherit [bool] signal () as super end
and toolbar_visibility_changed () = object (self) inherit [bool] signal () as super end
and tabbar_visibility_changed () = object (self) inherit [bool] signal () as super end
and messages_visibility_changed () = object (self) inherit [bool] signal () as super end
and signals ~switch_project ~menubar_visibility_changed ~toolbar_visibility_changed
  ~tabbar_visibility_changed ~messages_visibility_changed =
object (self)
  inherit ml_signals [switch_project#disconnect; menubar_visibility_changed#disconnect;
    toolbar_visibility_changed#disconnect; tabbar_visibility_changed#disconnect;
    messages_visibility_changed#disconnect]
  method switch_project = switch_project#connect ~after
  method menubar_visibility_changed = menubar_visibility_changed#connect ~after
  method toolbar_visibility_changed = toolbar_visibility_changed#connect ~after
  method tabbar_visibility_changed = tabbar_visibility_changed#connect ~after
  method messages_visibility_changed = messages_visibility_changed#connect ~after
end

let browser = begin
  Sys.chdir (Filename.dirname Sys.executable_name);
  Unix.putenv "GTK_SETLOCALE" "0";
  let locale = GtkMain.Main.init () in
  let window = GWindow.window
    ~title:Oe_config.title
    ~icon:Icons.oe
    ~type_hint:`NORMAL
    ~kind:`TOPLEVEL
    ~show:false
    ()
  in
  Gtk_util.init ();
  new browser window;
end
















