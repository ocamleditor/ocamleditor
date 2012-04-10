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


open Module_browser
open Printf
open Code_insight
open GdkKeysyms
open Miscellanea

(** browser *)
class browser ~project ?packing () =
  let ebox           = GBin.event_box ?packing () in
  let vbox           = GPack.vbox ~packing:ebox#add () in
  let toolbar        = GButton.toolbar ~border_width:0 ~style:`ICONS ~orientation:`HORIZONTAL ~packing:vbox#pack () in
  let _              = toolbar#set_icon_size `SMALL_TOOLBAR in
  let button_back    = GButton.tool_button ~stock:`GO_BACK ~packing:toolbar#insert () in
  let button_forward = GButton.tool_button ~stock:`GO_FORWARD ~packing:toolbar#insert () in
  let button_home    = GButton.tool_button ~stock:`HOME ~packing:toolbar#insert () in
  let _              = GButton.separator_tool_item ~draw:false ~packing:toolbar#insert () in
  let item_title     = GButton.tool_item ~expand:true ~packing:toolbar#insert () in
  let label_title    = GMisc.label ~markup:"" ~xalign:0.0 ~packing:item_title#add () in
  let _              = GButton.separator_tool_item ~draw:false ~packing:toolbar#insert () in
  let button_add     = GButton.tool_button ~stock:`ADD ~packing:toolbar#insert () in
  let button_remove  = GButton.tool_button ~stock:`REMOVE ~packing:toolbar#insert () in
  let vpaned         = GPack.paned `VERTICAL ~packing:vbox#add () in
  let stackbox       = GPack.hbox ~packing:vpaned#add1 () in
  let dbox           = GPack.vbox ~packing:vpaned#add2 ~spacing:3 () in (* details box *)

  let tsw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:dbox#add () in
  let tbuf = new Ocaml_text.buffer ~lexical_enabled:true () in
  let tview = new Ocaml_text.view ~buffer:tbuf () in
  let _ =
    tsw#add tview#coerce;
    let pref = {!Preferences.preferences with Preferences.pref_code_folding_enabled=false} in
    pref.Preferences.pref_indent_lines <- false;
    pref.Preferences.pref_highlight_current_line <- false;
    pref.Preferences.pref_show_line_numbers <- false;
    pref.Preferences.pref_right_margin_visible <- false;
    let tags = pref.Preferences.pref_tags in
    let tag_names, colors = List.split tags in
    Lexical.init_tags ~tags:tag_names ~colors (tbuf :> GText.buffer);
    Preferences_apply.apply (tview :> Text.view) pref;
    tview#set_pixels_above_lines 1;
    tview#set_pixels_below_lines 2;
    tview#set_border_window_size ~typ:`LEFT ~size:0;
    tview#set_editable false;
    tview#set_left_margin 5;
    tview#set_right_margin 5;
    tview#misc#set_has_tooltip true;
  in
  let hyperlink = new Gmisclib.Text.Hyperlink.hyperlink ~view:(tview :> GText.view) ~use_ctrl_key:false () in
  let _              = hyperlink#enable() in

  let dview          = GMisc.label ~markup:"" ~packing:dbox#pack () in (* doc view *)
  let width          = 500 in
  let height         = 300 in
object (self)
  inherit GObj.widget ebox#as_widget

  val mutable stack_back = Stack.create()
  val mutable stack_forward = Stack.create()
  val mutable widget_paths : Module_browser.widget option = None
  val mutable libpath = []
  val mutable current_libdir = None
  val mutable tooltip_popup = None
  val tooltip_liim = Liim.create ~delay:0.3 ()

  initializer self#init ()

  (** init *)
  method private init () =
    ignore (Liim.start tooltip_liim);
    self#open_libraries ();
    (** Buttons *)
    ignore (button_back#connect#clicked ~callback:(fun () -> ignore (self#go_back())));
    ignore (button_forward#connect#clicked ~callback:self#go_forward);
    ignore (button_home#connect#clicked ~callback:self#go_home);
    ignore (button_remove#connect#clicked ~callback:begin fun () ->
      let wlibs = self#get_libraries_widget() in
      let paths = wlibs#view#selection#get_selected_rows in
      let rr = List.map wlibs#model#get_row_reference paths in
      List.iter (fun reference -> ignore (wlibs#model#remove reference#iter)) rr;
      (* set next cursor position *)
      let count = ref 0 in
      wlibs#model#foreach (fun _ _ -> incr count; false);
      let last = GTree.Path.create [!count - 1] in
      let path = min last (try List.hd paths with Failure "hd" -> last) in
      wlibs#view#selection#select_path path;
      wlibs#view#misc#grab_focus()
    end);
    (** Tooltips *)
    ignore (tview#misc#connect#query_tooltip ~callback:self#tooltip);
    ignore (tview#event#connect#after#leave_notify ~callback:begin fun _ ->
      Gmisclib.Idle.add ~prio:600 (fun () -> ignore (self#tooltip_destroy()));
      false
    end);
    (** Hyperlinks *)
    ignore (hyperlink#connect#hover ~callback:begin fun (bounds, iter) ->
      if not iter#ends_line then begin
        let start, stop = self#get_word_bounds iter in
        let id = tbuf#get_text ~start ~stop () in
        match self#get_longid id with
          | Some absolute_id ->
            let item = Code_insight.find_symbol absolute_id libpath in
            bounds := (if item = None then None else Some (start, stop))
          | _ -> ()
      end
    end);
    ignore (hyperlink#connect#activate ~callback:begin fun iter ->
      let start, stop = self#get_word_bounds iter in
      let id = tbuf#get_text ~start ~stop () in
      match self#get_longid id with
        | Some absolute_id ->
          let item = Code_insight.find_symbol absolute_id libpath in
          begin
            match item with
              | Some item ->
                Printf.printf "%s\n%!" item.ci_descr;
              | _ -> ()
          end;
        | _ -> ()
    end);

  (** tooltip_destroy *)
  method private tooltip_destroy () =
    Liim.set tooltip_liim ignore;
    (match tooltip_popup with Some (w, _) -> w#destroy(); tooltip_popup <- None | _ -> ());
    false

  (** tooltip *)
  method private tooltip ~x ~y ~kbd tooltip =
    match current_libdir with
      | Some current_libdir ->
        let x, y = tview#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
        let iter = tview#get_iter_at_location ~x ~y in
        if not iter#ends_line then begin
          let start, stop = self#get_word_bounds iter in
          let symbol = tbuf#get_text ~start ~stop () in
          match self#get_longid symbol with
            | Some absolute_id ->
              (*Printf.printf "absolute_id = %S\n%!" absolute_id;*)
              begin
                match Code_insight.find_symbol absolute_id libpath with
                  | Some item ->
                    let markup = sprintf "<big>%s</big>\n\n<tt>%s</tt>"
                      (Print_type.markup2 absolute_id) (Print_type.markup2 item.ci_descr) in
                    let label = GMisc.label ~xpad:5 ~ypad:5 ~markup () in
                    Liim.set tooltip_liim begin fun () ->
                      match tooltip_popup with
                        | Some (w, old) when old = markup -> ()
                        | _ ->
                          Gaux.may (tview#get_window `WIDGET) ~f:begin fun window ->
                            let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
                            ignore (self#tooltip_destroy());
                            let popup = Gtk_util.window_tooltip label#coerce ~fade:true ~x:(pX + 10) ~y:(pY + 10) () in
                            tooltip_popup <- Some (popup, markup);
                          end;
                    end;
                    false
                  | _ -> self#tooltip_destroy()
              end;
            | _ -> self#tooltip_destroy()
        end else (self#tooltip_destroy())
      | _ -> self#tooltip_destroy()

  (** open_libraries *)
  method private open_libraries () =
    let wlibs = self#create_widget ~packing:stackbox#add () in
    Stack.push wlibs stack_back;
    (** Standard Library *)
    let name = "Standard Library" in
    let ocamllib = project.Project.ocamllib in
    let ocamllib = if ocamllib = "" then Ocaml_config.ocamllib () else ocamllib in
    libpath <- ocamllib :: libpath;
    let row = wlibs#model#append () in
    wlibs#model#set ~row ~column:wlibs#col_name name;
    wlibs#model#set ~row ~column:wlibs#col_descr (sprintf "%s" name);
    wlibs#model#set ~row ~column:wlibs#col_path ocamllib;
    (** Other libraries required from the current project *)
    let project_load_path = List.sort Pervasives.compare (Project.get_load_path project) in
    List.iter begin fun path ->
      let basename = Filename.basename path in
      let markup =
        match Miscellanea.filename_relative (Project.path_src project) path with
          | Some "" -> Project.src
          | Some x -> x
          | _ ->
            begin
              match Miscellanea.filename_relative ocamllib path with
                | Some x -> sprintf "+%s" x
                | _ -> if Filename.is_implicit path then (Project.src ^ "/" ^ path) else path
            end
      in
      let row = wlibs#model#append () in
      wlibs#model#set ~row ~column:wlibs#col_name basename;
      wlibs#model#set ~row ~column:wlibs#col_descr markup;
      wlibs#model#set ~row ~column:wlibs#col_path path;
      libpath <- path :: libpath;
    end project_load_path;
    (** row_activated *)
    ignore (wlibs#view#connect#row_activated ~callback:begin fun path vc ->
      self#open_library ~parent:wlibs ~path
    end);
    ignore (wlibs#view#selection#connect#changed ~callback:begin fun () ->
      current_libdir <- None;
    end);
    wlibs#view#selection#set_mode `MULTIPLE;
    wlibs#view#set_cursor (GTree.Path.create [0]) wlibs#vc_descr;
    wlibs#view#misc#grab_focus();
    self#show_current();

  (** open_library *)
  method private open_library ~parent ~path =
    let wlib = self#create_widget () in
    let row = parent#model#get_iter path in
    let libpath = parent#model#get ~row ~column:parent#col_path in
    current_libdir <- Some libpath;
    let module_names = Code_insight.read_module_names ~sort:true ~path:[libpath] () in
    let result = List.map begin fun (module_name, _) -> {
      ci_modlid = "";
      ci_name   = module_name;
      ci_longid = [module_name];
      ci_descr  = "";
      ci_kind   = Pmodule;
      ci_cdecl  = None;
      ci_mtype  = None;
      ci_local  = false
    } end module_names in
    let insight = {ci_mode=Ci_module ""; ci_text=""; ci_length=0; ci_title=""; ci_result=result} in
    ignore (wlib#view#connect#row_activated ~callback:(fun path _ -> self#open_module ~parent:wlib ~path));
    wlib#fill insight;
    (** Patch model with module paths *)
    let module_names = ref module_names in
    wlib#model#foreach begin fun path row ->
      begin
        try
          let _, module_path = List.hd !module_names in
          wlib#model#set ~row ~column:wlib#col_path module_path;
          module_names := List.tl !module_names;
        with Failure _ -> assert false
      end;
      false
    end;
    wlib#vc_icon#set_visible false;
    self#push wlib;

  (** open_module *)
  method private open_module ~parent ~path =
    let row         = parent#model#get_iter path in
    let widget      = self#create_widget () in
    let modlid      = parent#model#get ~row ~column:parent#col_name in
    let _, p_item   = parent#model#get ~row ~column:parent#col_data in
    let parent_path = parent#model#get ~row ~column:parent#col_path in
    let sign        =
      match p_item.ci_mtype with
        | None ->
          Code_insight.signature ~path:(Misc.find_in_path_uncap [parent_path] (modlid^".cmi")) modlid
        | Some (modlid, mtype) ->
          let items, _ = Code_insight.parse_module_type p_item.ci_longid mtype in
          items
    in
    let sign = List.rev sign in
    let insight = {ci_mode = Ci_module ""; ci_text=""; ci_length=0; ci_title=""; ci_result=sign} in
    widget#vc_modlid#set_visible false;
    widget#fill insight;
    widget#model#foreach begin fun path row ->
      widget#model#set ~row ~column:widget#col_path parent_path;
      false
    end;
    self#push widget;
    let show_details () =
      try
        let path = List.hd widget#view#selection#get_selected_rows in
        let row = widget#model#get_iter path in
        let _, data = widget#model#get ~row ~column:widget#col_data in
        match data.ci_kind with
          | Pmodule | Pmodtype | Pcltype -> tview#buffer#set_text ""
          | Pclass ->
            tview#buffer#set_text data.ci_descr;
            Lexical.tag tview#buffer ~start:tbuf#start_iter ~stop:tbuf#end_iter;
            (*Greek.replace tview#buffer;*)
          | _ ->
            tview#buffer#set_text data.ci_descr;
            Lexical.tag tview#buffer ~start:tbuf#start_iter ~stop:tbuf#end_iter;
            (*Greek.replace tview#buffer;*)
      with Failure "hd" -> ()
    in
    ignore (widget#view#selection#connect#changed ~callback:show_details);
    ignore (widget#view#connect#row_activated ~callback:begin fun path _ ->
      let row = widget#model#get_iter path in
      let _, data = widget#model#get ~row ~column:widget#col_data in
      match data.ci_kind with
        | Pmodule -> self#open_module ~parent:widget ~path
        | Pmodtype -> ()
        | Pclass -> (match data.ci_cdecl with Some x -> self#open_class x | _ -> ())
        | Pcltype -> ()
        | _ -> tview#buffer#set_text data.ci_descr
    end);
    show_details()

  (** open_class *)
  method private open_class (modlid, class_name, typ) =
    let widget = self#create_widget () in
    let sign = Code_insight.parse_class_declaration modlid class_name typ in
    let insight = {ci_mode = Ci_class None; ci_text=""; ci_length=0; ci_title=""; ci_result=sign} in
    widget#vc_modlid#set_visible false;
    widget#fill insight;
    self#push widget;
    let show_details () =
      try
        let path = List.hd widget#view#selection#get_selected_rows in
        let row = widget#model#get_iter path in
        let _, data = widget#model#get ~row ~column:widget#col_data in
        match data.ci_kind with
          | Pmethod | Pmethod_private | Pmethod_virtual | Pmethod_private_virtual ->
            tview#buffer#set_text data.ci_descr;
            Lexical.tag tview#buffer ~start:tbuf#start_iter ~stop:tbuf#end_iter;
            (*Greek.replace tview#buffer*)
          | _ -> tview#buffer#set_text ""
      with Failure "hd" -> ()
    in
    ignore (widget#view#selection#connect#changed ~callback:show_details);
    show_details()

  (** show_current *)
  method private show_current () =
    tview#buffer#set_text "";
    Stack.iter (fun w -> w#misc#hide()) stack_forward;
    Stack.iter (fun w -> w#misc#hide()) stack_back;
    button_back#misc#set_sensitive (Stack.length stack_back > 1);
    button_home#misc#set_sensitive (Stack.length stack_back > 1);
    button_forward#misc#set_sensitive (Stack.length stack_forward > 0);
    button_add#misc#set_sensitive (Stack.length stack_back = 1);
    button_remove#misc#set_sensitive (Stack.length stack_back = 1);
    try
      let current = Stack.top stack_back in
      current#misc#show ();
      current#view#misc#grab_focus ();
      let title, info =
        match self#get_current_longid () with
          | None -> "Libraries", ""
          | Some (libpath, _ :: []) -> (Filename.basename libpath), (Filename.dirname libpath)
          | Some (libpath, longid) -> (String.concat "." (List.rev (List.tl longid))), libpath
      in
      let info = match info with "" -> info | x -> x ^ "\n" in
      let markup = sprintf "<small><i>%s</i></small><span font-size='large'><b>%s</b></span>" info title in
      label_title#set_label markup;
    with Stack.Empty -> ()

  (** push *)
  method private push widget =
    stackbox#add widget#coerce;
    Stack.push widget stack_back;
    Stack.clear stack_forward;
    self#show_current();

  (** go_back *)
  method go_back () =
    if Stack.length stack_back > 1 then begin
      (try Stack.push (Stack.pop stack_back) stack_forward with Stack.Empty -> ());
      self#show_current();
      false
    end else true (* Home? *)

  (** go_forward *)
  method go_forward () =
    (try Stack.push (Stack.pop stack_forward) stack_back with Stack.Empty -> ());
    self#show_current()

  (** go_home *)
  method go_home () = while not (self#go_back()) do () done

  (** is_relative *)
  method private is_relative id =
    let is_relative = function
      | first_name :: _ when String.length first_name > 0 ->
        let first_char = (String.get first_name 0) in
        if first_char = Char.lowercase first_char then true else begin
          let widget = self#get_current_widget () in
          let model = widget#model in
          let found = ref false in
          model#foreach begin fun path row ->
            let _, data = widget#model#get ~row ~column:widget#col_data in
            found := data.ci_name = first_name;
            !found
          end;
          !found
        end
      | _ -> assert false
    in
    let id = Longident.flatten (Longident.parse id) in
    is_relative id

  (** get_longid *)
  method private get_longid id =
    match self#get_current_longid () with
      | Some (_, longid) ->
        Some begin
          match self#is_relative id with
            | true ->
              let longpath = try List.tl longid with Failure _ -> [] in
              (String.concat "." (List.rev longpath)) ^ "." ^ id
            | false -> id
        end
      | None -> None

  (** get_current_longid *)
  method private get_current_longid () =
    let widget = self#get_current_widget () in
    let model = widget#model in
    try
      let selected = List.hd widget#view#selection#get_selected_rows in
      let row = model#get_iter selected in
      let path = model#get ~row ~column:widget#col_path in
      let _, data = model#get ~row ~column:widget#col_data in
      Some (path, data.ci_longid)
    with Failure _ -> None

  (** get_current_widget *)
  method get_current_widget : unit -> Module_browser.widget = function () ->
    try Stack.top stack_back with Stack.Empty -> assert false

  (** get_libraries_widget *)
  method private get_libraries_widget () =
    match widget_paths with Some w -> w | _ -> assert false

  (** get_current_model_path *)
  method get_current_model_path () =
    let widget = self#get_current_widget () in
    let model = widget#model in
    let view = widget#view in
    match view#selection#get_selected_rows with
      | [] -> assert false
      | path :: _ -> path, model#get_iter path

  (** get_word_bounds *)
  method private get_word_bounds iter =
    tbuf#select_word ~iter ~pat:Ocaml_word_bound.longid ~select:false ()

  (** create_widget *)
  method private create_widget : ?packing:(GObj.widget -> unit) -> unit -> widget = fun ?packing () ->
    let widget = new widget ~width ~height ?packing () in
    widget#view#set_enable_search true;
    widget#view#set_search_column widget#col_name.GTree.index;
    widget#sw#set_shadow_type `IN;
    ignore (widget#connect#backspace ~callback:(fun () -> ignore (self#go_back ())));
    ignore (widget#view#event#connect#key_press ~callback:begin fun ev ->
      let state = GdkEvent.Key.state ev in
      let key = GdkEvent.Key.keyval ev in
      if state = [`MOD1] && key = _Left then begin
        ignore (self#go_back ());
        true
      end else if state = [`MOD1] && key = _Right then begin
        self#go_forward();
        true
      end else false
    end);
    widget

end

(** create *)
let create ~project =
  let title = "Module Browser" in
  let window = GWindow.window ~title ~position:`CENTER ~icon:Icons.oe ~height:600 ~show:false () in
  let widget = new browser ~project ~packing:window#add () in

(*  ignore (widget#title#connect#changed ~callback:(fun (text, _, _) ->




    window#set_title (match text with "" -> title | x -> sprintf "%s - %s" title x)));*)
  window#show()
;;


