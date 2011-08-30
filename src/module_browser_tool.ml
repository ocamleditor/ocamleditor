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
open GdkKeysyms
open Miscellanea
open Oe

(** pixbuf_of_kind *)
let pixbuf_of_kind = function
  | Pvalue       -> Icons.func
  | Pfunc        -> Icons.func
  | Pmethod      -> Icons.met
  | Pmethod_private_virtual -> Icons.met_private_virtual
  | Pmethod_private -> Icons.met_private
  | Pmethod_virtual -> Icons.met_virtual
  | Ptype        -> Icons.typ
  | Plabel       -> Icons.none_14
  | Pconstructor -> Icons.constructor
  | Pexception   -> Icons.exc
  | Pmodule      -> Icons.module_impl
  | Pmodtype     -> Icons.module_impl
  | Pclass       -> Icons.classe
  | Pcltype      -> Icons.class_type
;;

(** symbol_list *)
class symbol_list ?packing ?width ?height () =
  let renderer               = GTree.cell_renderer_text [] in
  let renderer_pixbuf        = GTree.cell_renderer_pixbuf [] in
  let cols                   = new GTree.column_list in
  let col_icon               = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf") in
  let col_search             = cols#add Gobject.Data.string in
  let col_type_descr         = cols#add Gobject.Data.string in
  let col_add_descr          = cols#add Gobject.Data.string in
  let col_symbol_data        = cols#add Gobject.Data.caml in
  let model                  = GTree.list_store cols in
  let vc_icon                = GTree.view_column ~renderer:(renderer_pixbuf, ["pixbuf", col_icon]) ~title:"" () in
  let vc_type_descr          = GTree.view_column ~renderer:(renderer, ["markup", col_type_descr]) ~title:"Type Description" () in
  let vc_add_descr           = GTree.view_column ~renderer:(renderer, ["markup", col_add_descr]) ~title:"" () in
  let sw                     = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ?packing () in
  let view                   = GTree.view
    ~model:model
    ~enable_search:false
    ~headers_visible:true
    ~reorderable:false
    ~packing:sw#add ()
  in
  let _                      = view#append_column vc_icon in
  let _                      = view#append_column vc_type_descr in
  let _                      = view#append_column vc_add_descr in
  let _                      = view#set_headers_visible false in
  let _                      = vc_add_descr#set_visible false in
  let _                      = view#set_search_column col_search.GTree.index in

object (self)
  inherit GObj.widget sw#as_widget
  val backspace = new backspace ()
  val mutable title = "", ""
  val mutable is_search_output = false

  method connect = new symbol_list_signals ~backspace
  method model = model
  method col_icon : [`pixbuf] Gobject.obj GTree.column = col_icon
  method col_search : string GTree.column = col_search
  method col_type_descr : string GTree.column = col_type_descr
  method col_add_descr = col_add_descr
  method col_symbol_data : symbol_cache_entry GTree.column = col_symbol_data
  method vc_icon = vc_icon
  method vc_type_descr = vc_type_descr
  method vc_add_descr = vc_add_descr
  method view = view
  method sw = sw
  method renderer = renderer
  method renderer_pixbuf = renderer_pixbuf
  method title = title
  method set_title x y = title <- x, y
  method is_search_output = is_search_output
  method set_is_search_output x = is_search_output <- x

  initializer
    ignore (view#event#connect#key_press ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = _BackSpace then begin
        backspace#call();
        true
      end else false
    end);

  method fill symbols =
    model#clear();
    (** Fill model *)
    List.iter begin fun symbol ->
      let name = Symbol.get_name symbol in
      let symbol_path = Symbol.concat_value_path symbol in
      let row = model#append () in
      model#set ~row ~column:col_symbol_data symbol;
      model#set ~row ~column:col_icon (pixbuf_of_kind symbol.sy_kind);
      model#set ~row ~column:col_search name;
      let local = false in
      let descr = if local then sprintf "%s : %s" name symbol.sy_type else symbol.sy_type in
      let descr = Print_type.markup2 descr in
      let descr = replace_first ["\\([ \t]*\\)\\("^(Str.quote (Glib.Markup.escape_text name))^"\\)\\([ \t]+\\|$\\)", "\\1<b>\\2</b>\\3"] descr in
      let descr =
        let lines = Str.split (!~ "\n") descr in
        if List.length lines > 4 then (sprintf "<b>%s</b> (...)" name)
        else descr
      in
      let descr = if is_search_output
        then sprintf "<span size='small' color='#877033'>%s</span>\n%s" symbol_path descr (* #404040 *)
        else descr
      in
      model#set ~row ~column:col_type_descr descr;
    end symbols;

end

and symbol_list_signals ~backspace = object
  inherit GUtil.ml_signals [backspace#disconnect]
  method backspace = backspace#connect ~after
end

and backspace () = object (self) inherit [unit] GUtil.signal () as super end

(** widget *)
class widget ~project ?packing () =
  let ebox           = GBin.event_box ?packing () in
  let vbox           = GPack.vbox ~packing:ebox#add () in
  let toolbar        = GButton.toolbar ~border_width:0 ~style:`ICONS ~orientation:`HORIZONTAL ~packing:vbox#pack () in
  let _              = toolbar#set_icon_size `SMALL_TOOLBAR in
  let button_back    = GButton.tool_button ~stock:`GO_BACK ~packing:toolbar#insert () in
  let button_forward = GButton.tool_button ~stock:`GO_FORWARD ~packing:toolbar#insert () in
  let button_up      = GButton.tool_button ~stock:`GO_UP ~packing:toolbar#insert () in
  let button_home    = GButton.tool_button ~stock:`HOME ~packing:toolbar#insert () in
  let _              = GButton.separator_tool_item ~draw:false ~packing:toolbar#insert () in
  let item_find      = GButton.tool_item ~expand:false ~packing:toolbar#insert () in
  let entry_find     = GEdit.entry ~packing:item_find#add () in
  let button_find    = GButton.tool_button ~stock:`FIND ~packing:toolbar#insert () in
  let _              = GButton.separator_tool_item ~draw:true ~packing:toolbar#insert () in
  let item_title     = GButton.tool_item ~expand:true ~packing:toolbar#insert () in
  let label_title    = GMisc.label ~markup:"" ~xalign:0.0 ~packing:item_title#add () in
  let _              = GButton.separator_tool_item ~draw:true ~packing:toolbar#insert () in
  let button_add     = GButton.tool_button ~stock:`ADD ~packing:toolbar#insert () in
  let button_remove  = GButton.tool_button ~stock:`REMOVE ~packing:toolbar#insert () in
  let vpaned         = GPack.paned `HORIZONTAL ~packing:vbox#add () in
  let stackbox       = GPack.hbox ~packing:vpaned#add1 () in
  let dbox           = GPack.vbox ~packing:vpaned#add2 ~spacing:3 () in (* details box *)
  let tsw            = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:dbox#add () in
  let tbuf           = new Ocaml_text.buffer ~lexical_enabled:true () in
  let tview          = new Ocaml_text.view ~buffer:tbuf () in
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
    tview#set_cursor_visible false;
    tview#set_left_margin 5;
    tview#set_right_margin 5;
    tview#misc#set_has_tooltip false;
    vpaned#set_position 450;
  in
  (* Hyperlink created after Ocaml_text.view so that hyperlink tag takes precedence. *)
  let hyperlink      = new Gmisclib.Text.Hyperlink.hyperlink ~view:(tview :> GText.view) ~use_ctrl_key:false () in
  let _              = hyperlink#enable() in
object (self)
  inherit GObj.widget vbox#as_widget

  val mutable search_results_length = 0
  val mutable search_results_count = 0
  val mutable widget_libraries = None
  val mutable current_libdir = None
  val mutable stack_back = Stack.create()
  val mutable stack_forward = Stack.create()
  val mutable tooltip_popup = None
  (*val tooltip_liim = Liim.create ~delay:0.5 () (* 0.3 *)*)
  val liim_entry_find = Liim.create ~delay:0.85 ()
  val show_current = new show_current ()

  initializer self#init ()

  (** init *)
  method private init () =
    (*ignore (Liim.start tooltip_liim);*)
    ignore (Liim.start liim_entry_find);
    self#open_libraries ();
    (** Toolbar buttons *)
    let find_and_fill () = self#find ~fill:true () in
    ignore (button_back#connect#clicked ~callback:(fun () -> ignore (self#go_back())));
    ignore (button_forward#connect#clicked ~callback:self#go_forward);
    ignore (button_up#connect#clicked ~callback:self#go_up);
    ignore (button_home#connect#clicked ~callback:self#go_home);
    ignore (button_find#connect#clicked ~callback:find_and_fill);
    ignore (entry_find#connect#changed ~callback:self#find);
    ignore (button_remove#connect#clicked ~callback:begin fun () ->
      match widget_libraries with
        | None -> assert false
        | Some wlibs ->
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
    (** Tooltips (Right-click) *)
    (*ignore (tview#misc#connect#query_tooltip ~callback:begin fun ~x ~y ~kbd _ ->
      self#tooltip ~x ~y
    end);*)
    ignore (tview#event#connect#focus_in ~callback:begin fun _ ->
      ignore (self#tooltip_destroy()); false
    end);
    ignore (tview#event#connect#focus_out ~callback:begin fun _ ->
      ignore (self#tooltip_destroy()); false
    end);
    ignore (tview#event#connect#button_press ~callback:begin fun ev ->
      if GdkEvent.Button.button ev = 3 then begin
        let x = int_of_float (GdkEvent.Button.x ev) in
        let y = int_of_float (GdkEvent.Button.y ev) in
        tview#misc#grab_focus();
        ignore (self#tooltip ~x ~y);
        true
      end else begin
        ignore (self#tooltip_destroy());
        false
      end
    end);
    (** Hyperlinks *)
    let with_symbol_at_iter iter f =
      Activity.wrap Activity.Symbol begin fun () ->
        match self#get_current_symbol () with
          | Some current ->
            let start, stop = self#get_word_bounds iter in
            let text = tbuf#get_text ~start ~stop () in
            let root = Symbol.get_module_path current in
            let symbol = self#find_symbol ~root text in
            f start stop symbol
          | _ -> ()
      end ()
    in
    ignore (hyperlink#connect#hover ~callback:begin fun (bounds, iter) ->
      if not iter#ends_line && not (Glib.Unichar.isspace iter#char) then begin
        with_symbol_at_iter iter begin fun start stop symbol ->
          bounds := (if symbol = None then (ignore (self#tooltip_destroy()); None) else Some (start, stop))
        end
      end else (ignore (self#tooltip_destroy()))
    end);
    ignore (hyperlink#connect#activate ~callback:begin fun iter ->
      with_symbol_at_iter iter begin fun start stop -> function
        | Some symbol -> self#go_to_symbol symbol
        | _ -> ()
      end
    end);
    (** On show_current event *)
    ignore (self#connect#show_current ~callback:begin fun widget ->
      let small_title, title = widget#title in
      let small_title = if small_title = "" then "" else sprintf "<i><small>%s</small></i>\n" small_title in
      kprintf label_title#set_label "%s<b><big>%s</big></b>" small_title title;
      ignore (self#tooltip_destroy());
    end);
    (** Entry find *)
    (*Gobject.Property.set entry_find#as_widget
      {Gobject.name="primary-icon-stock"; conv=Gobject.Data.string} "gtk-find";*)
    Gobject.Property.set entry_find#as_widget
      {Gobject.name="secondary-icon-stock"; conv=Gobject.Data.string} "gtk-clear";
    ignore begin GtkSignal.connect
      ~sgn:{
        GtkSignal.name="icon-press";
        classe=`widget;
        marshaller=fun f ->
          GtkSignal.marshal1 Gobject.Data.int "GtkEntry::icon-press" (fun icon_position -> f icon_position)
      }
      ~callback:self#entry_find_icon_press_cb
      entry_find#as_widget
    end;
    ignore (entry_find#event#connect#key_press ~callback:begin fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._Return then begin
        find_and_fill();
        true
      end else false
    end);
    (**  *)
    ignore (self#misc#connect#destroy ~callback:(fun () -> Liim.destroy liim_entry_find));
    ignore (ebox#event#connect#key_press ~callback:begin fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._F3 then begin
        entry_find#misc#grab_focus();
        true
      end else false
    end);

    (**  *)
    self#find();
    entry_find#misc#grab_focus();

  (** entry_find_icon_press_cb *)
  method private entry_find_icon_press_cb = function
    | 0 -> entry_find#misc#grab_focus()
    | 1 -> entry_find#set_text ""; entry_find#misc#grab_focus()
    | _ -> assert false

  (** find *)
  method private find ?(fill=false) () =
    Liim.set liim_entry_find begin fun () ->
      let text = entry_find#text in
      Gobject.Property.set entry_find#as_widget
        {Gobject.name="secondary-icon-sensitive"; conv=Gobject.Data.boolean} (String.length text > 0);
      if String.length text > 0 then begin
        let re = Str.regexp_case_fold (".*" ^ text ^ ".*") in
        let symbols = Symbol.filter_by_name project.Project.symbols text in
        let path = List.sort Pervasives.compare (Project.get_load_path project) in
        let ocamllib = project.Project.ocamllib in
        let path = ocamllib :: path in
        let modules = Symbol.read_modules_name ~sort:false ~path () in
        let symbols = symbols @ (Miscellanea.Xlist.filter_map begin fun (m, filename) ->
          if Str.string_match re m 0 then Some {sy_id=[m]; sy_kind=Pmodule; sy_type=m; sy_filename=filename} else None
        end modules) in
        let symbols = List.sort (fun a b -> compare a.sy_id b.sy_id) symbols in
        search_results_length <- List.length symbols;
        (*Printf.printf "text = %s --> %d\n%!" text search_results_length;*)
        let top = Stack.top stack_back in
        let widget =
          if not top#is_search_output then begin
            let widget = self#create_widget () in
            widget#set_is_search_output true;
            widget#vc_type_descr#set_sizing `AUTOSIZE;
            (*widget#view#set_headers_visible true;*)
            ignore (widget#view#connect#row_activated ~callback:begin fun path _ ->
              let row = widget#model#get_iter path in
              let symbol = widget#model#get ~row ~column:widget#col_symbol_data in
              let libdir = symbol.sy_filename in
              match symbol.sy_kind with
                | Pmodule -> self#open_module ~libdir ~module_path:symbol.sy_id ()
                | Pmodtype -> ()
                | Pclass -> self#open_class ~libdir ~class_path:symbol.sy_id ()
                | Pcltype -> ()
                | _ -> ()
            end);
            ignore (widget#view#selection#connect#after#changed
              ~callback:(fun () -> self#open_module_details widget ()));
            ignore (self#connect#show_current ~callback:begin fun cur ->
              if widget#misc#get_oid = cur#misc#get_oid then (self#open_module_details widget ())
            end);
            self#open_module_details widget ();
            self#push widget;
            widget
          end else top
        in
        label_title#set_label (sprintf "%d results" search_results_length);
        widget#model#clear();
        search_results_count <- 0;
        entry_find#misc#grab_focus();
        entry_find#set_position (Glib.Utf8.length entry_find#text);
        if fill || search_results_length <= 150 then begin
          ignore ((*Thread.create*) ((*Miscellanea.crono*) widget#fill) symbols);
        end;
      end
    end

  (** tooltip_destroy *)
  method private tooltip_destroy () =
    (*Liim.set tooltip_liim ignore;*)
    (match tooltip_popup with Some (w, _) -> w#destroy(); tooltip_popup <- None | _ -> ());
    false

  (** tooltip *)
  method private tooltip ~x ~y =
    let x, y = tview#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
    let iter = tview#get_iter_at_location ~x ~y in
    if not iter#ends_line && not (Glib.Unichar.isspace iter#char) then begin
      match self#get_current_symbol () with
        | Some symbol ->
          let start, stop = self#get_word_bounds iter in
          let text = tbuf#get_text ~start ~stop () in
          begin
            match self#find_symbol ~root:(Symbol.get_module_path symbol) text with
              | Some symbol ->
                begin
                  let markup = sprintf "<big>%s</big>\n\n<tt>%s</tt>"
                    (Print_type.markup2 (Symbol.string_of_id symbol.sy_id))
                    (Print_type.markup2 symbol.sy_type)
                  in
                  let label = GMisc.label ~xpad:5 ~ypad:5 ~markup () in
                  (*Liim.set tooltip_liim begin fun () ->
                    match tooltip_popup with
                      | Some (w, old) when old = markup -> ()
                      | _ ->*)
                        Gaux.may (tview#get_window `WIDGET) ~f:begin fun window ->
                          let pX, pY = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
                          ignore (self#tooltip_destroy());
                          let popup = Gtk_util.window_tooltip label#coerce ~parent:tview ~fade:true ~x:(pX + 0) ~y:(pY + 20) () in
                          tooltip_popup <- Some (popup, markup);
                        end;
                  (*end;*)
                end;
                false
              | _ -> self#tooltip_destroy()
          end
        | _ -> self#tooltip_destroy()
    end else (self#tooltip_destroy())

  (** open_libraries *)
  method private open_libraries () =
    let wlibs = self#create_widget ~packing:stackbox#add () in
    widget_libraries <- Some wlibs;
    Stack.push wlibs stack_back;
    (** Standard Library *)
    let name = "Standard Library" in
    let ocamllib = project.Project.ocamllib in
    let ocamllib = if ocamllib = "" then Ocaml_config.ocamllib () else ocamllib in
    let row = wlibs#model#append () in
    wlibs#model#set ~row ~column:wlibs#col_search name;
    wlibs#model#set ~row ~column:wlibs#col_type_descr name;
    wlibs#model#set ~row ~column:wlibs#col_add_descr (sprintf "<i><small>%s</small></i>" ocamllib);
    wlibs#model#set ~row ~column:wlibs#col_symbol_data
      {sy_id=[name]; sy_kind=Pmodule; sy_type=""; sy_filename=ocamllib};
    (** Other libraries required from the current project *)
    let project_load_path = List.sort Pervasives.compare (Project.get_load_path project) in
    List.iter begin fun path ->
      let basename = Filename.basename path in
      let add_descr =
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
      wlibs#model#set ~row ~column:wlibs#col_search basename;
      wlibs#model#set ~row ~column:wlibs#col_type_descr basename;
      wlibs#model#set ~row ~column:wlibs#col_add_descr (sprintf "<i><small>%s</small></i>" add_descr);
      wlibs#model#set ~row ~column:wlibs#col_symbol_data
        {sy_id=[basename]; sy_kind=Pmodule; sy_type=""; sy_filename=path};
    end project_load_path;
    (** row_activated *)
    ignore (wlibs#view#connect#row_activated ~callback:begin fun path vc ->
      let row = wlibs#model#get_iter path in
      let parent = wlibs#model#get ~row ~column:wlibs#col_symbol_data in
      let libdir = parent.sy_filename in
      self#open_library ~libdir ()
    end);
    wlibs#vc_icon#set_visible false;
    wlibs#vc_add_descr#set_visible true;
    wlibs#view#selection#set_mode `MULTIPLE;
    wlibs#view#set_cursor (GTree.Path.create [0]) wlibs#vc_type_descr;
    wlibs#view#misc#grab_focus();
    wlibs#set_title "" "Libraries";
    self#show_current();

  (** open_library *)
  method private open_library ~libdir () =
    let wlib = self#create_widget () in
    current_libdir <- Some libdir;
    let entries = Symbol.read_modules_name ~sort:true ~path:[libdir] () in
    ignore (wlib#view#connect#row_activated ~callback:begin fun path _ ->
      let row = wlib#model#get_iter path in
      let symbol = wlib#model#get ~row ~column:wlib#col_symbol_data in
      let module_name = Symbol.get_module_name symbol in
      self#open_module ~libdir ~module_path:[module_name] ()
    end);
    List.iter begin fun (module_name, filename) ->
      let row = wlib#model#append () in
      wlib#model#set ~row ~column:wlib#col_icon (pixbuf_of_kind Pmodule);
      wlib#model#set ~row ~column:wlib#col_search module_name;
      wlib#model#set ~row ~column:wlib#col_type_descr (sprintf "<b>%s</b>" module_name);
      wlib#model#set ~row ~column:wlib#col_add_descr (sprintf "<i><small>%s</small></i>" filename);
      wlib#model#set ~row ~column:wlib#col_symbol_data
        {sy_id=[module_name]; sy_kind=Pmodule; sy_type=""; sy_filename=filename};
    end entries;
    wlib#vc_icon#set_visible false;
    wlib#vc_add_descr#set_visible true;
    wlib#set_title (Filename.dirname libdir) (Filename.basename libdir);
    self#push wlib;

  (** open_module *)
  method private open_module ~module_path ?(libdir="") ?(f=fun _ -> ()) () =
    Activity.wrap Activity.Symbol begin fun () ->
      let flat_view = false in
      let len = List.length module_path + 1 in
      let widget = self#create_widget () in
      let entries = Symbol.filter_by_modulepath project.Project.symbols module_path in
      let entries = List.filter begin fun s ->
        s.sy_id <> module_path && (flat_view || List.length s.sy_id = len)
      end entries in
      (*let entries = List.sort (fun a b -> compare a.sy_id b.sy_id) entries in*)
      ignore (widget#view#connect#row_activated ~callback:begin fun path _ ->
        let row = widget#model#get_iter path in
        let symbol = widget#model#get ~row ~column:widget#col_symbol_data in
        match symbol.sy_kind with
          | Pmodule -> self#open_module ~libdir ~module_path:symbol.sy_id ()
          | Pmodtype -> ()
          | Pclass -> self#open_class ~libdir ~class_path:symbol.sy_id ()
          | Pcltype -> ()
          | _ -> ()
      end);
      widget#fill entries;
      widget#set_title libdir (String.concat "." module_path);
      self#push widget;
      ignore (widget#view#selection#connect#after#changed
        ~callback:(fun () -> self#open_module_details widget ()));
      ignore (self#connect#show_current ~callback:begin fun cur ->
        if widget#misc#get_oid = cur#misc#get_oid then (self#open_module_details widget ())
      end);
      self#open_module_details widget ();
      f widget
    end ()

  (** open_module_details *)
  method private open_module_details widget () =
    try
      let path = List.hd widget#view#selection#get_selected_rows in
      let row = widget#model#get_iter path in
      let data = widget#model#get ~row ~column:widget#col_symbol_data in
      match data.sy_kind with
        | Pmodule | Pmodtype | Pcltype -> tview#buffer#set_text ""
        | Pclass ->
          tview#buffer#set_text data.sy_type;
          Lexical.tag tview#buffer ~start:tbuf#start_iter ~stop:tbuf#end_iter;
          (*Greek.replace tview#buffer;*)
        | _ ->
          tview#buffer#set_text data.sy_type;
          Lexical.tag tview#buffer ~start:tbuf#start_iter ~stop:tbuf#end_iter;
          (*Greek.replace tview#buffer;*)
    with Failure "hd" -> ()

  (** open_class *)
  method private open_class ~class_path ?(libdir="") () =
    Activity.wrap Activity.Symbol begin fun () ->
      let widget = self#create_widget () in
      let entries = Symbol.filter_methods project.Project.symbols class_path in
      let entries = List.filter (fun s -> s.sy_id <> class_path) entries in
      (*let entries = List.sort (fun a b -> compare a.sy_id b.sy_id) entries in*)
      widget#fill entries;
      widget#set_title libdir (String.concat "." class_path);
      self#push widget;
      ignore (self#connect#show_current ~callback:begin fun cur ->
        if widget#misc#get_oid = cur#misc#get_oid then (self#open_class_details widget ())
      end);
      ignore (widget#view#selection#connect#changed ~callback:(fun () -> self#open_class_details widget()));
      self#open_class_details widget ()
    end ()

  (** open_class_details *)
  method private open_class_details widget () =
    try
      let path = List.hd widget#view#selection#get_selected_rows in
      let row = widget#model#get_iter path in
      let data = widget#model#get ~row ~column:widget#col_symbol_data in
      match data.sy_kind with
        | Pmethod | Pmethod_private | Pmethod_virtual | Pmethod_private_virtual ->
          tview#buffer#set_text data.sy_type;
          Lexical.tag tview#buffer ~start:tbuf#start_iter ~stop:tbuf#end_iter;
          (*Greek.replace tview#buffer*)
        | _ -> tview#buffer#set_text ""
    with Failure "hd" -> ()

  (** push *)
  method private push widget =
    stackbox#add widget#coerce;
    Stack.push widget stack_back;
    Stack.iter (fun w -> w#destroy()) stack_forward;
    Stack.clear stack_forward;
    self#show_current ();

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

  (** go_up *)
  method go_up () =
    match current_libdir with
      | Some libdir ->
        begin
          match self#get_current_symbol () with
            | Some symbol ->
              begin
                try
                  let module_path = Symbol.get_module_path symbol in
                  let module_path = Xlist.rev_tl module_path in
                  if module_path = [] then (raise Exit);
                  self#open_module ~libdir ~module_path ()
                with Invalid_argument "Empty List" | Exit -> (self#open_library ~libdir ())
              end;
            | _ -> ()
        end;
      | _ -> ()

  (** go_to_symbol *)
  method go_to_symbol ?(kind=[Ptype; Pclass]) symbol =
    match current_libdir with
      | Some libdir ->
        let module_path = Symbol.get_module_path symbol in
        begin
          let f (widget : symbol_list) =
            widget#model#foreach begin fun path row ->
              let sym = widget#model#get ~row ~column:widget#col_symbol_data in
              if sym.sy_id = symbol.sy_id && List.mem sym.sy_kind kind then begin
                widget#view#selection#select_iter row;
                Gmisclib.Idle.add (fun () ->
                  widget#view#scroll_to_cell ~align:(0.0, 0.38) path widget#vc_type_descr);
                true
              end else false
            end
          in
          self#open_module ~libdir ~module_path ~f ();
        end;
      | _ -> ()

  (** show_current *)
  method private show_current () =
    tview#buffer#set_text "";
    Stack.iter (fun w -> w#misc#hide()) stack_forward;
    Stack.iter (fun w -> w#misc#hide()) stack_back;
    button_back#misc#set_sensitive (Stack.length stack_back > 1);
    button_home#misc#set_sensitive (Stack.length stack_back > 1);
    button_forward#misc#set_sensitive (Stack.length stack_forward > 0);
    button_up#misc#set_sensitive (Stack.length stack_back > 1 && current_libdir <> None);
    button_add#misc#set_sensitive (Stack.length stack_back = 1);
    button_remove#misc#set_sensitive (Stack.length stack_back = 1);
    try
      let current = Stack.top stack_back in
      current#misc#show ();
      current#view#misc#grab_focus ();
      show_current#call current;
    with Stack.Empty -> ()

  (** find_symbol *)
  method private find_symbol ?(root=[]) text =
    let ident = Longident.flatten (Longident.parse text) in
    let kind = [Ptype; Pclass] in
    match Symbol.find_by_modulepath ~kind project.Project.symbols ident with
      | None ->
        (* If the symbol is relative, search recursively in the parent module *)
        let rec find ?(root=[]) text =
          let absolute_ident = root @ ident in
          match Symbol.find_by_modulepath ~kind project.Project.symbols absolute_ident with
            | None ->
              if root = [] then None else begin
                let root = try Xlist.rev_tl root with Invalid_argument "Empty List" -> [] in
                self#find_symbol ~root text
              end
            | symbol -> symbol
        in
        begin
          match find ~root text with
            | None ->
              let pervasives_ident = "Pervasives" :: ident in
              Symbol.find_by_modulepath project.Project.symbols pervasives_ident
            | symbol -> symbol
        end;
      | symbol -> symbol

  (** get_current_symbol *)
  method get_current_symbol () =
    let widget = Stack.top stack_back in
    try
      let path = List.hd widget#view#selection#get_selected_rows in
      let row = widget#model#get_iter path in
      let symbol = widget#model#get ~row ~column:widget#col_symbol_data in
      Some symbol
    with Failure "hd" -> None

  (** get_word_bounds *)
  method private get_word_bounds iter =
    tbuf#select_word ~iter ~pat:Ocaml_word_bound.longid ~select:false ()

  (** create_widget *)
  method private create_widget : ?packing:(GObj.widget -> unit) -> unit -> symbol_list = fun ?packing () ->
    let widget = new symbol_list (*~width ~height*) ?packing () in
    widget#view#set_enable_search true;
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

  (** connect *)
  method connect = new widget_signals ~show_current

end

(** widget_signals *)
and widget_signals ~show_current = object
  inherit GUtil.ml_signals [show_current#disconnect]
  method show_current = show_current#connect ~after
end

and show_current () = object (self) inherit [symbol_list] GUtil.signal () as super end

(** create *)
let create ~project =
  let title = "Module Browser" in
  let window = GWindow.window ~title ~position:`CENTER ~icon:Icons.oe ~width:900 ~height:600 ~border_width:5 ~allow_shrink:true ~show:false () in
  let widget = new widget ~project ~packing:window#add () in
  window#show()
;;
