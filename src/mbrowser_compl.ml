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


open Printf
open GdkKeysyms
open Oe
open Miscellanea
open Mbrowser_tool
open Mbrowser_slist

type compl =
  | Compl_module of string list * string (* module_path x module_name *)
  | Compl_class of string
  | Compl_id of string
  | Compl_none

(** completion *)
class completion ~project ?packing () =
  let vbox            = GPack.vbox ~spacing:0 ~border_width:2 ?packing () in
  let tbox            = GPack.hbox ~spacing:5 ~border_width:0 ~packing:vbox#pack () in
  let ebox_resize     = GBin.event_box ~packing:(tbox#pack ~fill:false ~expand:false) () in
  let _               = GMisc.image ~pixbuf:Icons.grip ~packing:ebox_resize#add () in
  let ebox_title      = GBin.event_box ~packing:tbox#add () in
  let ebox_close      = GBin.event_box ~packing:tbox#pack () in
  let _               = List.iter (fun x -> x#misc#set_property "visible-window" (`BOOL false))
                        [ ebox_close; ebox_resize] in
  let label_title     = GMisc.label ~markup:"" ~ypad:0 ~packing:ebox_title#add ~show:false () in
  let _               = GMisc.separator `HORIZONTAL ~packing:vbox#pack () in
  let _               = GMisc.image ~pixbuf:Icons.close ~packing:ebox_close#add () in
  let statusbar_box   = GPack.hbox ~spacing:5 ~packing:(vbox#pack ~from:`END) () in
  let label_longid    = GMisc.label ~markup:"" ~ypad:0 ~xalign:0.0 ~packing:statusbar_box#add () in
  let _               = GMisc.separator `VERTICAL ~packing:statusbar_box#pack () in
  let label_search    = GMisc.label ~markup:"" ~ypad:0 ~xalign:0.0 ~width:50 ~packing:statusbar_box#pack () in
  let _               = GMisc.separator `VERTICAL ~packing:statusbar_box#pack () in
  let _               = GMisc.label ~markup:"<small>Press \"<tt>Ctrl+Return</tt>\" to insert the simple value name</small>" ~ypad:0 ~xalign:0.0 ~packing:statusbar_box#pack () in
  let _               = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~from:`END) () in
object (self)
  inherit GObj.widget vbox#as_widget

  val widget =
    let width, height = Oe_config.completion_popup_default_dimensions in
    new Mbrowser_tool.widget ~is_completion:true ~project ~width ~height ~packing:vbox#add ()

  val mutable search_string = ""
  val mutable current_page : Editor_page.page option = None
  val mutable current_window : GWindow.window option = None

  initializer self#init ()

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
      let value_path = Symbol.split_value_path text in
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

  method private set_title_markup markup =
    kprintf label_title#set_label
      "<span color=\"%s\" weight=\"normal\" underline=\"none\" font_size=\"x-large\">%s</span>"
      Preferences.preferences#get.Preferences.pref_fg_color_popup
      markup;
      label_title#misc#show()

  method private compl_root () = widget#create_widget_modules ()

  method private compl_id ~prefix ~page () =
    let f = widget#select_symbol_by_prefix ~prefix ~kind:[] in
    widget#find_compl ~include_methods:false ~prefix ~page ~f ();

  method private compl_module ~module_path ~prefix =
    let f = widget#select_symbol_by_prefix ~module_path ~prefix ~kind:[] in
    widget#create_widget_module ~module_path ~f ~sort:true ();
    kprintf self#set_title_markup "Module <b><tt>%s</tt></b>" (Symbol.string_of_id module_path);

  method private compl_class ~text ~page () =
    let re1 = Miscellanea.regexp "[a-zA-Z_0-9']$" in
    let ins = page#buffer#get_iter `INSERT in
    let filename = page#get_filename in
    let iter = ins#backward_find_char (fun x -> Glib.Utf8.from_unichar x = "#") in
    let prefix = page#buffer#get_text ~start:iter#forward_char ~stop:ins () in
    let offset = iter#backward_char#offset in
    let annot = Annotation.find_block_at_offset ~project ~filename ~offset in
    let class_type =
      match annot with
        | None -> (* try to lookup obj_name in the ident. table *)
          let stop = iter in
          let start = (stop#backward_find_char (fun x -> not (Str.string_match re1 (Glib.Utf8.from_unichar x) 0)))#forward_char in
          let obj_name = page#buffer#get_text ~start ~stop () in
          let obj_types =
            Miscellanea.Xlist.filter_map begin fun ((f, n), (dstop, dtype)) ->
              if f = filename && n = obj_name then begin
                match dstop with
                  | None -> Some dtype
                  | Some stop when offset >= stop.Oe.annot_cnum ->
                    Some dtype
                  | _ -> None
              end else None
            end !Annotation.itable
          in
          (* TODO: exculde non-class types from the list *)
          (* The head of the list is the last defined name *)
          (match obj_types with [] -> None | x :: _ -> x);
        | Some block -> Annotation.get_type block.Oe.annot_annotations
    in
    match class_type with
      | Some class_type ->
        Printf.printf "class_type = %s\n%!" class_type;
        let class_path = Longident.flatten (Longident.parse class_type) in
        let f = widget#select_symbol_by_prefix ~module_path:class_path ~prefix ~kind:[] in
        widget#create_widget_class ~class_path ~f ();
        kprintf self#set_title_markup "Class <b><tt>%s</tt></b>" class_type
      | _ -> ()

  method private apply_completion
      ~(page : Editor_page.page)
      ~(symbol_list : symbol_list)
      ~longid
      ~path =
    let paths_opened = Lex.paths_opened (page#buffer#get_text ()) in
    let row = symbol_list#model#get_iter path in
    let symbol = symbol_list#model#get ~row ~column:col_symbol_data in
    let id = if longid then Symbol.concat_value_path symbol else symbol_list#model#get ~row ~column:col_search in
    let re_id = !~ "[a-zA-Z_0-9']$" in
    let re_longid = !~ "[a-zA-Z_0-9'.]$" in
    let backward_to_id_start re =
      let iter = ref (page#buffer#get_iter `INSERT) in
      iter := !iter#backward_char;
      while
        let char = !iter#get_text ~stop:!iter#forward_char in
        Str.string_match re char 0
      do iter := !iter#backward_char done;
      !iter#forward_char;
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
    page#buffer#delete_selection ();
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

  method private init () =
    let color = `NAME Preferences.preferences#get.Preferences.pref_bg_color_popup in
    ebox_title#misc#modify_bg [`NORMAL, color];
    let set_title = kprintf self#set_title_markup "<b><tt>%s</tt></b>" in
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
      symbol_list#view#misc#modify_base [`NORMAL, `NAME Preferences.preferences#get.Preferences.pref_bg_color_popup];
      symbol_list#renderer#set_properties [`CELL_BACKGROUND Preferences.preferences#get.Preferences.pref_bg_color_popup];
      symbol_list#renderer_pixbuf#set_properties [`XPAD 3];
      let bg = Preferences.preferences#get.Preferences.pref_bg_color_popup in
      symbol_list#renderer_pixbuf#set_properties [`CELL_BACKGROUND bg];
    end);

  method private activate_row ?(longid=true) ?(is_class=false) ~(symbol_list : symbol_list) () =
    self#with_current_page begin fun page ->
      if page#ocaml_view#editable then begin
        match symbol_list#view#selection#get_selected_rows with
          | [] -> ()
          | path :: _ ->
            let row = symbol_list#model#get_iter path in
            let symbol = symbol_list#model#get ~row ~column:col_symbol_data in
            begin
              let longid = if symbol.Oe.sy_local then false else longid && (Symbol.get_module_name symbol <> "Pervasives") in
              match symbol.Oe.sy_kind with
                | Oe.Pmodule | Oe.Pmodtype | Oe.Pclass | Oe.Pcltype when not longid ->
                  self#apply_completion ~page ~symbol_list ~longid ~path;
                  self#hide()
                | Oe.Pclass when is_class ->
                  Printf.printf " = %b\n%!" longid;
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
      ignore (symbol_list#view#connect#row_activated ~callback:begin fun path _ ->
        self#activate_row ~symbol_list ();
      end);
      (** selection changed *)
      ignore (symbol_list#view#selection#connect#changed ~callback:begin fun () ->
        match symbol_list#view#selection#get_selected_rows with
          | [] -> ()
          | path :: _ ->
            let row = symbol_list#model#get_iter path in
            let symbol = symbol_list#model#get ~row ~column:col_symbol_data in
            kprintf label_longid#set_label "<tt>%s</tt>" (Glib.Markup.escape_text (Symbol.concat_value_path symbol))
      end);
    | _ -> ()

  method private hide () =
    ignore (widget#tooltip_destroy());
    widget#clear();
    self#with_current_page begin fun page ->
      page#buffer#undo#end_block ();
      if not widget#is_slist_visible then begin
        widget#button_layout_slist#set_active true;
        (*widget#toggle_details();*)
      end;
      current_page <- None;
      match current_window with
        | Some w -> w#misc#hide();
        | _ -> ()
    end;

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
              if Sys.os_type = "Win32" then (window#present());
              window#move ~x ~y;
              let alloc = window#misc#allocation in
              let x, y =
                (if x + alloc.Gtk.width > (Gdk.Screen.width()) then (Gdk.Screen.width() - alloc.Gtk.width) else x),
                (if y + alloc.Gtk.height > (Gdk.Screen.height()) then (Gdk.Screen.height() - alloc.Gtk.height) else y);
              in
              window#move ~x ~y;
              window#present();
            | _ -> ()
        end;
        window
      | None ->
        let x, y = match xy with Some (x, y) -> x, y | _ -> 0,0 in
        let window = Gtk_util.window self#coerce ~x ~y ~focus:true ~escape:false ~show:(xy <> None) () in
        current_window <- Some window;
        ignore (window#event#connect#focus_out ~callback:begin fun ev ->
          self#hide();
          true (* Prevent the event to be propagated to the Gtk_util.window
                  focus_out handler, which destroys the window. *)
        end);
        ignore (window#event#connect#key_release ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = GdkKeysyms._Escape then (self#hide(); true) else false
        end);
        (** Window buttons *)
        ignore (ebox_close#event#connect#button_press ~callback:(fun _ -> self#hide(); true));
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
        window
end


(** create *)
let cache : (string * (GWindow.window * completion)) list ref = ref [];;

let create ~project ?(page : Editor_page.page option) () =
  let compl =
    try
      let window, compl = List.assoc (Project.filename project) !cache in
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


