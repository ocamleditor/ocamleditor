(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

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
open Oe
open Miscellanea
open Mbrowser_slist

module Log = Common.Log.Make(struct let prefix = "Mbrowser_compl" end)

type compl =
  | Compl_module of string list * string (* module_path x module_name *)
  | Compl_class of string
  | Compl_id of string
  | Compl_none

(** completion *)
class completion ~project ?packing () =
  let window_decorated = Preferences.preferences#get.Preferences.pref_compl_decorated in
  let vbox            = GPack.vbox ~spacing:0 ~border_width:2 ?packing () in
  let tbox            = GPack.hbox ~spacing:5 ~border_width:0 () in
  let ebox_resize     = GBin.event_box ~packing:(tbox#pack ~fill:false ~expand:false) ~show:(not window_decorated) () in
  let _               = GMisc.image ~pixbuf:Icons.grip ~packing:ebox_resize#add () in
  let ebox_title      = GBin.event_box ~packing:tbox#add () in
  let ebox_doc        = GBin.event_box ~packing:tbox#pack () in
  let ebox_font_incr  = GBin.event_box ~packing:tbox#pack () in
  let ebox_font_decr  = GBin.event_box ~packing:tbox#pack () in
  let ebox_pin        = GBin.event_box ~packing:tbox#pack () in
  let ebox_close      = GBin.event_box ~packing:tbox#pack ~show:(not window_decorated) () in
  let _               = List.iter (fun x -> x#misc#set_property "visible-window" (`BOOL false))
                        [ ebox_pin; ebox_resize; ebox_doc; ebox_font_incr; ebox_font_decr; ebox_close] in
  let _               = ebox_pin#misc#set_tooltip_text "Toggle pin status" in
  let _               = ebox_doc#misc#set_tooltip_markup "Show documentation pane (<tt><small>F1</small></tt>)" in
  let label_title     = GMisc.label ~markup:"" ~ypad:0 ~packing:ebox_title#add ~show:false () in
  let _               = GMisc.image ~pixbuf:Icons.pin_off ~packing:ebox_pin#add () in
  let _               = GMisc.image ~pixbuf:Icons.doc ~packing:ebox_doc#add () in
  let _               = GMisc.image ~pixbuf:Icons.zoom_in_14 ~packing:ebox_font_incr#add () in
  let _               = GMisc.image ~pixbuf:Icons.zoom_out_14 ~packing:ebox_font_decr#add () in
  let _               = GMisc.image ~pixbuf:Icons.close_window ~icon_size:`MENU ~packing:ebox_close#add () in
  let statusbar_box   = GPack.hbox ~spacing:5 ~packing:(vbox#pack ~from:`END) () in
  let label_longid    = GMisc.label ~markup:"" ~ypad:0 ~xalign:0.0 ~packing:statusbar_box#add () in
  let _               = GMisc.separator `VERTICAL ~packing:statusbar_box#pack () in
  let label_search    = GMisc.label ~markup:"" ~ypad:0 ~xalign:0.0 ~width:50 ~packing:statusbar_box#pack () in
  let _               = GMisc.separator `VERTICAL ~packing:statusbar_box#pack () in
  let _               = GMisc.label ~markup:"<small>Press \"<tt>Ctrl+Return</tt>\" to insert the simple value name</small>" ~ypad:0 ~xalign:0.0 ~packing:statusbar_box#pack () in
  let _               = GMisc.separator `VERTICAL ~packing:statusbar_box#pack ~show:window_decorated () in
  let _               = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~from:`END) () in
  let _               = if window_decorated then statusbar_box#pack tbox#coerce
    else begin
      vbox#pack ~from:`START tbox#coerce;
      GMisc.separator `HORIZONTAL ~packing:vbox#pack ~show:(not window_decorated) () |> ignore;
    end
  in
object (self)
  inherit GObj.widget vbox#as_widget

  val widget =
    let width, height = Oe_config.completion_popup_default_dimensions in
    new Mbrowser_tool.widget ~is_completion:true ~project ~width ~height ~packing:vbox#add ()

  val mutable search_string = ""
  val mutable current_page : Editor_page.page option = None
  val mutable current_window : GWindow.window option = None
  val mutable pin_status = false

  initializer
    (*let color = `NAME Preferences.preferences#get.Preferences.pref_bg_color_popup in
    ebox_title#misc#modify_bg [`NORMAL, color];*)
    let set_title = self#set_title "" in
    ignore (widget#connect#switch_page ~callback:begin fun symbol_list ->
      set_title symbol_list#title.title;
      self#update_search_string (`set "");
    end);
    ignore (widget#connect#add_page ~callback:begin fun symbol_list ->
      symbol_list#view#set_enable_search false;
      symbol_list#view#set_search_column col_search.GTree.index;
      self#connect_signals (Some symbol_list);
      set_title symbol_list#title.title;
      symbol_list#sw#set_shadow_type `NONE;
      (*symbol_list#view#misc#modify_base [`NORMAL, `NAME Preferences.preferences#get.Preferences.pref_bg_color_popup];
      symbol_list#renderer#set_properties [`CELL_BACKGROUND Preferences.preferences#get.Preferences.pref_bg_color_popup];*)
      symbol_list#renderer_pixbuf#set_properties [`XPAD 3];
      (*let bg = Preferences.preferences#get.Preferences.pref_bg_color_popup in
      symbol_list#renderer_pixbuf#set_properties [`CELL_BACKGROUND bg];*)
    end);
    widget#connect#layout_toggled ~callback:self#update_button_states |> ignore;
    widget#button_layout_odoc#set_active true;


  method set_pin_status value =
    pin_status <- value;
    let pixbuf = if pin_status then Icons.pin_on else Icons.pin_off in
    let image = GMisc.image ~pixbuf () in
    ebox_pin#remove ebox_pin#child;
    ebox_pin#add image#coerce;

  method private find ~(page : Editor_page.page) () =
    match self#get_word ~page () with
      | Compl_class text ->
        self#compl_class ~text ~page ()
      | Compl_id prefix ->
        self#compl_id ~prefix ~page ();
      | Compl_module (module_path, name) ->
        let module_path = List.rev module_path in
        self#compl_module ~module_path ~prefix:name;
      | Compl_none -> self#compl_root()

  method private with_current_page f =
    match current_page with Some page -> f page | _ -> ()

  method private init_search_string ~(page : Editor_page.page) =
    let start, _ = page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.regexp ~select:false ~search:false () in
    let stop = page#buffer#get_iter `INSERT in
    let text =
      if start#compare stop >= 0 then ""
      else page#buffer#get_text ~start ~stop ()
    in
    self#update_search_string (`set text);

  method private update_search_string mode =
    let text = search_string in
    search_string <- begin
      match mode with
        | `backspace when search_string = "" -> ""
        | `backspace -> String.sub text 0 (String.length text - 1)
        | `append x -> text ^ x
        | `set x -> x
    end;
    label_search#set_label search_string

  method private get_compl text =
    let re = Miscellanea.regexp "[a-zA-Z_0-9'.#]+$" in
    if Str.string_match re text 0 then begin
      let value_path = Symbols.split_value_path text in
      begin
        match List.rev value_path with
          | hd :: [] when String.contains hd '#' ->
            Compl_class text;
          | prefix :: [] ->
            Compl_id prefix;
          | name :: module_path ->
            Compl_module (module_path, name);
          | [] -> assert false;
      end;
    end else Compl_none

  method private get_word ~(page : Editor_page.page) () =
    let start, _ = page#buffer#as_text_buffer#select_word
      ~pat:Ocaml_word_bound.longid ~select:false ~search:false () in
    let stop = page#buffer#get_iter `INSERT in
    if start#compare stop >= 0 then Compl_none
    else begin
      let text = page#buffer#get_text ~start ~stop () in
      self#get_compl text;
    end

  method private set_title t1 t2 =
    if window_decorated then self#set_title_text (t1 ^ " " ^ t2)
    else kprintf self#set_title_markup "%s <b><tt>%s</tt></b>" t1 t2

  method private set_title_text text =
    Option.iter (fun w -> w#set_title text) current_window;

  method private set_title_markup markup =
    kprintf label_title#set_label
      "<span weight=\"normal\" underline=\"none\" font_size=\"x-large\">%s</span>"
      (*Preferences.preferences#get.Preferences.pref_fg_color_popup*)
      markup;
    label_title#misc#show()

  method private compl_root () =
    widget#create_widget_modules ();
    kprintf self#set_title "" "";

  method private compl_id ~prefix ~page () =
    let f = widget#select_symbol_by_prefix ~prefix ~kind:[] in
    widget#find_compl ~include_methods:false ~prefix ~page ~f ();
    kprintf self#set_title "" "";

  method private compl_module ~module_path ~prefix =
    kprintf self#set_title "Module" (Symbols.string_of_id module_path);
    let f = widget#select_symbol_by_prefix ~module_path ~prefix ~kind:[] in
    widget#create_widget_module ~module_path ~f ~sort:true ();

  method private compl_class ~text ~page () =
    (*let re1 = Miscellanea.regexp "[a-zA-Z_0-9']$" in*)
    let ins = page#buffer#get_iter `INSERT in
    let iter = ins#backward_find_char (fun x -> Glib.Utf8.from_unichar x = "#") in
    let prefix = page#buffer#get_text ~start:iter#forward_char ~stop:ins () in
    let typ = Binannot_type.find ~page ~iter:iter#backward_char () in
    let class_type = Option.map (fun x -> x.Binannot_type.ba_type) typ in
    match class_type with
      | Some class_type ->
        Log.println `TRACE "class_type = %s\n%!" class_type;
        let class_path = Longident.flatten (Longident.parse class_type) in
        let f = widget#select_symbol_by_prefix ~module_path:class_path ~prefix ~kind:[] in
        widget#create_widget_class ~class_path ~f ();
        kprintf self#set_title "Class" class_type
      | _ -> kprintf self#set_title "" "";

  method private apply_completion
      ~(page : Editor_page.page)
      ~(symbol_list : symbol_list)
      ~longid
      ~path =
    (*let paths_opened = Lex.paths_opened (page#buffer#get_text ()) in*)
    let row = symbol_list#model#get_iter path in
    let symbol = symbol_list#model#get ~row ~column:col_symbol_data in
    let id = if longid then Symbols.concat_value_path symbol else symbol_list#model#get ~row ~column:col_search in
    let re_id = !~ "[a-zA-Z_0-9']$" in
    let re_longid = !~ "[a-zA-Z_0-9'.]$" in
    let start_iter = page#buffer#start_iter in
    let backward_to_id_start re =
      let iter = ref (page#buffer#get_iter `INSERT) in
      iter := !iter#backward_char;
      while
        let char = !iter#get_text ~stop:!iter#forward_char in
         Str.string_match re char 0 && (!iter#compare start_iter > 0)
      do iter := !iter#backward_char done;
      if !iter#equal start_iter then !iter else !iter#forward_char;
    in
    let forward_to_id_stop re =
      let iter = ref (page#buffer#get_iter `INSERT) in
      while
        let char = !iter#get_text ~stop:!iter#forward_char in
        Str.string_match re char 0
      do iter := !iter#forward_char done;
      !iter
    in
    let start = backward_to_id_start (if longid then re_longid else re_id) in
    let stop = forward_to_id_stop (if longid then re_longid else re_id) in
    page#buffer#select_range start stop;
    ignore (page#buffer#delete_selection ());
    let start = page#buffer#get_iter `INSERT in
    page#buffer#insert ~iter:start id;
    (*page#buffer#select_range start (start#backward_chars (Glib.Utf8.length id));*)

  method private select_symbol ~symbol_list ~page =
    match self#get_compl search_string with
      | Compl_module (module_path, name) ->
        let module_path = List.rev module_path in
        (*Printf.printf "===> Comple_module (%s, %s)\n%!" (String.concat "." module_path) name;*)
        widget#select_symbol_by_prefix ~module_path ~prefix:name ~kind:[] symbol_list;
        true
      | Compl_class text ->
        (*Printf.printf "===> Compl_class (%s)\n%!" text;*)
        true
      | Compl_id text ->
        (*Printf.printf "===> Compl_id (%s)\n%!" text;*)
        widget#select_symbol_by_prefix ~prefix:text ~kind:[] symbol_list;
        true;
      | Compl_none -> false

  method private activate_row ?(longid=true) ?(is_class=false) ~(symbol_list : symbol_list) () =
    self#with_current_page begin fun page ->
      if page#ocaml_view#editable then begin
        match symbol_list#view#selection#get_selected_rows with
          | [] -> ()
          | path :: _ ->
            let row = symbol_list#model#get_iter path in
            let symbol = symbol_list#model#get ~row ~column:col_symbol_data in
            begin
              let longid = if symbol.Oe.sy_local then false else longid && (Symbols.get_module_name symbol <> "Pervasives") in
              match symbol.Oe.sy_kind with
                | Oe.Pmodule | Oe.Pmodtype | Oe.Pclass | Oe.Pcltype when not longid ->
                  self#apply_completion ~page ~symbol_list ~longid ~path;
                  self#hide()
                | Oe.Pclass when is_class ->
                  Log.println `TRACE " = %b\n%!" longid;
                  self#apply_completion ~page ~symbol_list ~longid ~path;
                  self#hide()
                | Oe.Pmodule | Oe.Pmodtype | Oe.Pclass | Oe.Pcltype | Oe.Std_lib | Oe.Lib -> ()
                | Pmethod | Pmethod_private | Pmethod_virtual | Pmethod_private_virtual | Pattribute ->
                  self#apply_completion ~page ~symbol_list ~longid:false ~path;
                  self#hide()
                | _ ->
                  self#apply_completion ~page ~symbol_list ~longid ~path;
                  self#hide()
            end;
      end
    end

  method private connect_signals = function
    | Some symbol_list ->
      (** Key press events *)
      ignore (symbol_list#connect#backspace ~callback:begin fun () ->
        self#with_current_page begin fun page ->
          self#update_search_string `backspace;
          let found = self#select_symbol ~symbol_list ~page in
          if not found then begin
            let path = GTree.Path.create [0] in
            symbol_list#view#selection#select_path path;
            symbol_list#view#set_cursor path symbol_list#vc_type_descr;
            symbol_list#view#scroll_to_point 0 0;
          end
        end
      end);
      ignore (symbol_list#view#event#connect#key_press ~callback:begin fun ev ->
        let key = GdkEvent.Key.keyval ev in
        let state = GdkEvent.Key.state ev in
        if key = _Return && List.for_all (fun s -> List.mem s state) [`CONTROL; `SHIFT] then begin
          self#activate_row ~longid:true ~is_class:true ~symbol_list ();
          true
        end else if key = _Return && state = [`CONTROL] then begin
          self#activate_row ~longid:false ~symbol_list ();
          true
        end else if List.mem key [_Up; _Down; _Left; _Right; _Page_Up; _Page_Down; _Home; _End; _Return] then begin
          false
        end else if key = _Tab then begin
          Gmisclib.Idle.add widget#odoc_view#misc#grab_focus;
          true
        end else if key = _BackSpace then begin
          assert false;
        end else if key = _Escape then begin
          false;
        end else begin
          let char = GdkEvent.Key.string ev in
          if char <> "" && Glib.Unichar.isprint (Glib.Utf8.first_char char) then begin
            self#with_current_page begin fun page ->
              let txt = Glib.Utf8.from_unichar (Glib.Utf8.first_char char) in
              self#update_search_string (`append txt);
              let found = self#select_symbol ~symbol_list ~page in
              if not found then self#hide()
            end
          end;
          true
        end;
      end);
      (** row_activated *)
      ignore (symbol_list#view#connect#row_activated ~callback:begin fun _ _ ->
        self#activate_row ~symbol_list ();
      end);
      (** selection changed *)
      ignore (symbol_list#view#selection#connect#changed ~callback:begin fun () ->
        match symbol_list#view#selection#get_selected_rows with
          | [] -> ()
          | path :: _ ->
            let row = symbol_list#model#get_iter path in
            let symbol = symbol_list#model#get ~row ~column:col_symbol_data in
            kprintf label_longid#set_label "<tt>%s</tt>" (Glib.Markup.escape_text (Symbols.concat_value_path symbol))
      end);
    | _ -> ()

  method hide () =
    if pin_status then begin
      Option.iter begin fun p ->
        Option.iter 
          (fun w -> w#set_opacity 
              (match Preferences.preferences#get.Preferences.pref_compl_opacity with 
               | Some opa -> opa 
               | _  -> 1.0)) 
          current_window;
        (*Opt.may pref.Preferences.pref_compl_opacity (fun opa -> Opt.may current_window (fun w -> w#set_opacity opa));*)
        Option.iter (fun w -> w#present()) (GWindow.toplevel p#coerce);
        widget#is_onscreen#set true;
      end current_page
    end else begin
      ignore (widget#tooltip_destroy());
      widget#clear();
      self#with_current_page begin fun page ->
        page#buffer#undo#end_block ();
        (*if not widget#is_slist_visible then begin
          widget#button_layout_slist#set_active true;
          (*widget#toggle_details();*)
          end;*)
        current_page <- None;
        widget#is_onscreen#set false;
        match current_window with
          | Some w -> w#misc#hide();
          | _ -> ()
      end;
    end

  method present ?page () =
    let xy =
      match page with
        | Some page ->
          current_page <- Some page;
          self#find ~page ();
          self#init_search_string ~page;
          page#buffer#undo#begin_block ~name:"compl0";
          Some (page#ocaml_view#get_location_at_cursor ())
        | _ -> None
    in
    match current_window with
      | Some window ->
        begin
          match xy with
            | Some (x, y) ->
              if not pin_status then begin
                if Sys.win32 then (window#show());
                window#move ~x ~y;
                let alloc = window#misc#allocation in
                let x, y =
                  (if x + alloc.Gtk.width > (Gdk.Screen.width()) then (Gdk.Screen.width() - alloc.Gtk.width) else x),
                  (if y + alloc.Gtk.height > (Gdk.Screen.height()) then (Gdk.Screen.height() - alloc.Gtk.height) else y);
                in
                window#move ~x ~y;
              end;
              widget#is_onscreen#set true;
              window#present();
            | _ -> ()
        end;
        window
      | None ->
        let x, y = match xy with Some (x, y) -> x, y | _ -> 0,0 in
        let window = Gtk_util.window self#coerce
            ~decorated:window_decorated
            ~type_hint:(if Sys.win32 then `UTILITY else `DIALOG)
            ~x ~y ~focus:true ~escape:false ~show:(xy <> None) ()
        in
        window#set_icon (Some Icons.oe);
        window#set_title "";
        window#set_deletable true;
        current_window <- Some window;
        ignore (window#event#connect#focus_out ~callback:begin fun _ ->
          self#hide();
          true (* Prevent the event to be propagated to the Gtk_util.window
                  focus_out handler, which destroys the window. *)
        end);
        if not Ocaml_config.is_mingw then begin
          ignore (window#event#connect#focus_in ~callback:begin fun _ ->
              Gmisclib.Idle.add (fun () -> window#set_opacity 1.0);
              false;
          end);
        end;
        ignore (window#event#connect#delete ~callback:begin fun _ ->
            self#set_pin_status false;
            self#hide();
            true;
        end);
        ignore (window#event#connect#after#key_release ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = GdkKeysyms._Escape then begin
            self#hide();
            true
          end else false
        end);
        (** Window buttons *)
        ebox_pin#event#connect#button_release ~callback:begin fun _ ->
          self#set_pin_status (not pin_status);
          true
        end |> ignore;
        ebox_font_incr#event#connect#button_release ~callback:begin fun _ ->
          widget#change_font_size 1;
          true
        end |> ignore;
        ebox_font_decr#event#connect#button_release ~callback:begin fun _ ->
          widget#change_font_size (-1);
          true
        end |> ignore;
        ebox_doc#event#connect#button_release ~callback:begin fun _ ->
          begin
            match widget#layout with
              | `odoc -> widget#button_layout_slist#set_active true;
              | `slist -> widget#button_layout_odoc#set_active true;
              | `both -> widget#button_layout_odoc#set_active false;
          end;
          true
        end |> ignore;
        ebox_close#event#connect#button_release ~callback:begin fun _ ->
          self#set_pin_status false;
          self#hide();
          true;
        end |> ignore;
        (** Window Move *)
        let motion = ref false in
        let xm0 = ref 0 in
        let ym0 = ref 0 in
        ignore (ebox_title#event#connect#button_press ~callback:begin fun ev ->
          xm0 := int_of_float (GdkEvent.Button.x ev);
          ym0 := int_of_float (GdkEvent.Button.y ev);
          Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `FLEUR);
          motion := true;
          false
        end);
        ignore (ebox_title#event#connect#button_release ~callback:begin fun ev ->
          Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `ARROW);
          motion := false;
          false
        end);
        ignore (ebox_title#event#connect#motion_notify ~callback:begin fun ev ->
          let x' = int_of_float (GdkEvent.Motion.x_root ev) in
          let y' = int_of_float (GdkEvent.Motion.y_root ev) in
          let x = x' - !xm0 - vbox#border_width - 21 in
          let y = y' - !ym0 - vbox#border_width in
          window#move ~x ~y;
          true
        end);
        (** Window Resize *)
        let resize = ref false in
        let xr0 = ref 0 in
        let yr0 = ref 0 in
        let width = ref 0 in
        let height = ref 0 in
        ignore (ebox_resize#event#connect#button_press ~callback:begin fun ev ->
          xr0 := int_of_float (GdkEvent.Button.x_root ev);
          yr0 := int_of_float (GdkEvent.Button.y_root ev);
          xm0 := int_of_float (GdkEvent.Button.x ev);
          ym0 := int_of_float (GdkEvent.Button.y ev);
          width := window#misc#allocation.Gtk.width;
          height := window#misc#allocation.Gtk.height;
          resize := true;
          false
        end);
        ignore (ebox_resize#event#connect#button_release ~callback:begin fun ev ->
          Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `ARROW);
          resize := false;
          false
        end);
        ignore (ebox_resize#event#connect#enter_notify ~callback:begin fun ev ->
          Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `TOP_LEFT_CORNER);
          false
        end);
        ignore (ebox_resize#event#connect#leave_notify ~callback:begin fun ev ->
          Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `ARROW);
          false
        end);
        ignore (ebox_resize#event#connect#motion_notify ~callback:begin fun ev ->
          let x' = int_of_float (GdkEvent.Motion.x_root ev) in
          let y' = int_of_float (GdkEvent.Motion.y_root ev) in
          let dx = !xr0 - x' - vbox#border_width in
          let dy = !yr0 - y' - vbox#border_width in
          let w = (*max width0*) (!width + dx) in
          let h = (*max height0*) (!height + dy) in
          (*let px, py = Gdk.Window.get_position window#misc#window in
          let x = if dx > 0 then x' - !x0 - vbox#border_width else px in
          let y = if dy > 0 then y' - !y0 - vbox#border_width else py in*)
          let x = x' - !xm0 - vbox#border_width in
          let y = y' - !ym0 - vbox#border_width in
          window#resize ~width:w ~height:h;
          window#move ~x ~y;
          true
        end);
        self#update_button_states();
        widget#is_onscreen#set (xy <> None);
        window

  method private update_button_states () =
    ebox_font_incr#misc#set_sensitive widget#button_layout_odoc#get_active;
    ebox_font_decr#misc#set_sensitive widget#button_layout_odoc#get_active;

end


(** create *)
let cache : (string * (GWindow.window * completion)) list ref = ref [];;

let create ~project ?(page : Editor_page.page option) () =
  let _ =
    try
      let _, compl = List.assoc (Project.filename project) !cache in
      ignore (compl#present ?page ());
      compl
    with Not_found -> begin
      let compl = new completion ~project () in
      let window = compl#present ?page () in
      cache := (Project.filename project, (window, compl)) :: !cache;
      compl
    end
  in ()
;;


