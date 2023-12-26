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
open Miscellanea
open Oe
open Mbrowser_slist

let title = "Module Browser"

let type_kinds = [Ptype; Pclass; Pcltype; Ptype_abstract; Ptype_variant; Ptype_record; Pexception]

(** widget *)
let messages  =
  match Oe_config.layout_find_module_browser with
    | `HORIZONTAL -> Messages.hmessages | _ -> Messages.vmessages;;

let cache_widget_modules = ref []

class widget ~project ?(is_completion=false) ?(enable_history=true) ?width ?height ?packing () =
  let ebox                = GBin.event_box ?packing () in
  let vbox                = GPack.vbox ~packing:ebox#add () in
  let sep_visible         = true in
  let paned               = GPack.paned `HORIZONTAL ~packing:vbox#add () in
  let toolbar             = GButton.toolbar ~border_width:0 ~orientation:`HORIZONTAL ~style:`ICONS ~packing:vbox#pack () in
  let _                   = toolbar#set_icon_size `MENU in
  let button_back         = GButton.tool_button ~stock:`GO_BACK ~homogeneous:false ~packing:toolbar#insert () in
  let button_forward      = GButton.tool_button ~stock:`GO_FORWARD ~homogeneous:false ~packing:toolbar#insert () in
  let button_up           = GButton.tool_button ~stock:`GO_UP ~homogeneous:false ~packing:toolbar#insert () in
  let button_libraries    = GButton.tool_button ~label:"Libraries" ~homogeneous:false ~packing:toolbar#insert () in
  let button_modules      = GButton.tool_button ~label:"Modules" ~homogeneous:false ~packing:toolbar#insert () in
  let _                   = GButton.separator_tool_item ~draw:true ~expand:false ~packing:toolbar#insert () in
  let item_find           = GButton.tool_item ~expand:false ~homogeneous:false ~packing:toolbar#insert () in
  let radio_find_path     = GButton.radio_tool_button ~packing:toolbar#insert ~active:true () in
  let radio_find_name     = GButton.radio_tool_button ~packing:toolbar#insert ~group:radio_find_path () in
  let radio_find_type     = GButton.radio_tool_button ~packing:toolbar#insert ~group:radio_find_path () in
  let _                   = radio_find_path#set_icon_widget (GMisc.image ~pixbuf:Icons.path_name ())#coerce in
  let _                   = radio_find_name#set_icon_widget (GMisc.image ~pixbuf:Icons.simple_name ())#coerce in
  let _                   = radio_find_type#set_icon_widget (GMisc.image ~pixbuf:Icons.typ ())#coerce in
  let _                   = radio_find_path#misc#set_tooltip_text "Search by value path" in
  let _                   = radio_find_name#misc#set_tooltip_text "Search by simple value name" in
  let _                   = radio_find_type#misc#set_tooltip_text "Search by type" in
  let entry_find          = GEdit.entry ~width_chars:15 ~packing:item_find#add () in
  let button_find         = GButton.tool_button (*~stock:`FIND*) ~homogeneous:false ~packing:toolbar#insert () in
  let _                   = button_find#set_icon_widget (GMisc.image ~pixbuf:Icons.find_16 ())#coerce in
  let _                   = GButton.separator_tool_item ~draw:sep_visible ~packing:toolbar#insert () in
  let button_add          = GButton.tool_button ~stock:`ADD ~homogeneous:false ~packing:toolbar#insert ~show:false () in
  let button_remove       = GButton.tool_button ~stock:`REMOVE ~homogeneous:false ~packing:toolbar#insert () in
  let _                   = GButton.separator_tool_item ~draw:sep_visible ~packing:toolbar#insert () in
  let button_incr_font    = GButton.tool_button ~label:"Increase Font Size" ~homogeneous:false ~packing:toolbar#insert () in
  let _                   = button_incr_font#set_icon_widget (GMisc.image ~pixbuf:Icons.zoom_in_14 ~icon_size:`MENU ())#coerce in
  let button_decr_font    = GButton.tool_button ~label:"Decrease Font Size" ~homogeneous:false ~packing:toolbar#insert () in
  let _                   = button_decr_font#set_icon_widget (GMisc.image ~pixbuf:Icons.zoom_out_14 ~icon_size:`MENU ())#coerce in
  let _                   = GButton.separator_tool_item ~draw:true ~expand:false ~packing:toolbar#insert () in
  let button_layout_slist = GButton.toggle_tool_button ~label:"Symbol list" ~homogeneous:false ~active:false ~packing:toolbar#insert () in
  let _                   = button_layout_slist#set_icon_widget (GMisc.image ~pixbuf:Icons.item_list ~icon_size:`MENU ())#coerce in
  let button_layout_odoc  = GButton.toggle_tool_button ~label:"Documentation" ~homogeneous:false ~active:false ~packing:toolbar#insert () in
  let _                   = button_layout_odoc#set_icon_widget (GMisc.image ~pixbuf:Icons.doc ~icon_size:`MENU ())#coerce in
  let _                   = GButton.separator_tool_item ~draw:true ~expand:false ~packing:toolbar#insert () in
  let button_detach       = GButton.tool_button ~label:"Detach" ~homogeneous:false ~packing:toolbar#insert () in
  let _                   = GMisc.image ~pixbuf:Icons.detach ~icon_size:`MENU ~packing:button_detach#set_icon_widget () in
  let box_slist           = GPack.vbox ~packing:paned#add1 ~show:false () in
  let label_title         = GMisc.label ~markup:"" ~xalign:0.5 ~xpad:3 ~ypad:5 ~packing:box_slist#pack () in
  let stack_box           = GPack.hbox ~packing:box_slist#add () in
  let box_odoc            = GPack.vbox ~packing:paned#add2 ~spacing:3 ~show:false () in (* details box *)
  let odoc_sw             = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:box_odoc#add () in
  let odoc_buffer         = new Ocaml_text.buffer ~lexical_enabled:true () in
  let odoc_view           = new Ocaml_text.view ~buffer:odoc_buffer () in
  let odoc_tag            = odoc_buffer#create_tag
                            ((`FONT Preferences.preferences#get.Preferences.pref_odoc_font) :: Oe_config.odoc_tag_properties) in
  let _                   = odoc_buffer#create_tag ~name:"large" [`SCALE `X_LARGE] in
  let tag_type2           = odoc_buffer#create_tag ~name:"type2" Oe_doc.Printer.Properties.type2 in
  let _                   = odoc_buffer#create_tag ~name:"line_spacing_small" [`SIZE_POINTS 4.] in
  let incremental_search  = new Incremental_search.incremental () in
  let pref                = {Preferences.preferences#get with Preferences.pref_code_folding_enabled=false} in
  let _ =
    odoc_buffer#undo#disable();
    odoc_sw#add odoc_view#coerce;
    pref.Preferences.pref_editor_indent_lines <- false;
    pref.Preferences.pref_highlight_current_line <- false;
    pref.Preferences.pref_show_line_numbers <- false;
    pref.Preferences.pref_right_margin_visible <- false;
    let tags = pref.Preferences.pref_tags in
    let tag_names, colors = List.split tags in
    odoc_buffer#init_tags ~tags:tag_names ~colors ();
    Preferences_apply.apply (odoc_view :> Text.view) pref;
    odoc_view#code_folding#set_enabled false;
    odoc_view#set_pixels_above_lines 0;
    odoc_view#set_pixels_below_lines 0;
    odoc_view#set_pixels_inside_wrap 0;
    odoc_view#set_editable false;
    odoc_view#set_cursor_visible false;
    odoc_view#set_left_margin (if is_completion then Oe_config.odoc_margin / 2 else Oe_config.odoc_margin);
    odoc_view#set_right_margin (if is_completion then Oe_config.odoc_margin / 2 else Oe_config.odoc_margin);
    odoc_view#set_wrap_mode `WORD;
    odoc_view#misc#set_has_tooltip false;
    odoc_view#options#set_show_markers false;
    odoc_view#options#set_show_dot_leaders false;
    odoc_view#set_accepts_tab false;
    odoc_view#set_border_window_size ~typ:`LEFT ~size:0;
    odoc_view#buffer#insert "\n";
    paned#set_position (900 / 5 * 2);
  in
  (* Hyperlink created after Ocaml_text.view to give precedence to the hyperlink tag. *)
  let hyperlink      = Gmisclib.Text.hyperlink ~view:(odoc_view :> GText.view) ~use_ctrl_key:false () in
  let _              = hyperlink#enable() in
object (self)
  inherit GObj.widget ebox#as_widget
  inherit Messages.page ~role:"module-browser"

  val mutable search_results_length = 0
  val mutable widget_libraries = None
  val mutable stack_back = Stack.create()
  val mutable stack_forward = Stack.create()
  val mutable cache_widget_module = []
  val mutable tooltip_popup = None
  val mutable current_symbol = None
  val mutable radio_signals = []
  val tout_entry_find = Timeout.create ~delay:0.85 ()
  val switch_page = new switch_page ()
  val add_page = new add_page ()
  val layout_toggled = new layout_toggled ()
  val tag_table = new GText.tag_table odoc_view#as_gtext_view#buffer#tag_table
  val mutable tags = []
  val mutable reuse = false

  method button_layout_slist = button_layout_slist
  method button_layout_odoc = button_layout_odoc
  method entry_find = entry_find
  method reuse = reuse
  method set_reuse x = reuse <- x

  initializer
    self#set_is_completion is_completion;
    (*  *)
    ignore (Preferences.preferences#connect#changed ~callback:begin fun _ ->
      odoc_view#misc#modify_font_by_name Preferences.preferences#get.Preferences.pref_base_font;
      odoc_tag#set_property (`FONT Preferences.preferences#get.Preferences.pref_odoc_font);
    end);
    tag_table#connect#tag_added ~callback:(fun tag -> tags <- new GText.tag tag :: tags) |> ignore;
    tag_table#connect#tag_removed ~callback:begin fun tag ->
      let oid = (new GText.tag tag)#get_oid in
      tags <- List.filter (fun t -> t#get_oid <> oid) tags;
    end |> ignore;
    button_incr_font#connect#clicked ~callback:(fun () -> self#change_font_size 1) |> ignore;
    button_decr_font#connect#clicked ~callback:( fun () -> self#change_font_size (-1)) |> ignore;
    (*  *)
    self#init_toolbar();
    self#init_tooltips();
    self#init_hyperlinks();
    (* On switch_page event *)
    ignore (self#connect#switch_page ~callback:begin fun widget ->
      let subtitle = String.trim widget#title.subtitle in
      let subtitle = if subtitle = "" then "" else sprintf "\n<i><small>%s</small></i>" subtitle in
      kprintf label_title#set_label "<b><big>%s</big></b>" widget#title.title (*small_title*);
      let tooltip = subtitle ^ "\n" ^ widget#title.tooltip in
      label_title#misc#set_tooltip_markup (String.trim tooltip);
      ignore (self#tooltip_destroy());
      (*  *)
      self#update_symbol_details widget ();
    end);
    (* Put toolbar on top of the window when detached. *)
    ignore (self#connect_detach#detached ~callback:begin function
      | true -> vbox#reorder_child ~pos:0 toolbar#coerce
      | false -> vbox#reorder_child ~pos:10 toolbar#coerce;
    end);
    (*  *)
    self#init_key_bindings();
    self#init_timeout_odoc();
    self#create_widget_modules ~push:false ();
    button_layout_slist#set_active true;
    button_layout_odoc#set_active (not is_completion);

  method change_font_size increment =
    let get_tag_fd (tag : GText.tag) =
      GPango.font_description_from_string (tag#get_property {
          Gobject.name = "font";
          conv         =
            {Gobject.kind = `STRING;
              proj         = (function `STRING (Some x) -> x | _ -> "");
              inj          = (fun x -> `STRING (Some x))}})
    in
    let change_size (fd : GPango.font_description) =
      let size = fd#size + increment * Pango.scale in
      if size >= 0 then fd#modify ~size ();
      size >= 0
    in
    let fd = odoc_view#misc#pango_context#font_description in
    if change_size fd then odoc_view#misc#modify_font fd;
    List.iter begin fun tag ->
      let fd = get_tag_fd tag in
      if change_size fd then tag#set_property (`FONT_DESC fd#fd);
    end (tag_type2 :: odoc_tag :: tags);
    (*Stack.iter begin fun widget ->
      let fd = widget#view#misc#pango_context#font_description in
      if change_size fd then widget#view#misc#modify_font fd
    end stack_back;
    Stack.iter begin fun widget ->
      let fd = widget#view#misc#pango_context#font_description in
      if change_size fd then widget#view#misc#modify_font fd
    end stack_forward;*)

  method init_hyperlinks () =
    let with_symbol_at_iter iter f =
      if not (iter#has_tag odoc_tag) then begin
        Activity.wrap Activity.Symbol begin fun () ->
          match self#get_current_symbol () with
            | Some current ->
              let start, stop = self#get_word_bounds iter in
              let text = odoc_buffer#get_text ~start ~stop () in
              let root = Symbols.get_parent_path current in
              let symbol = self#find_symbol ~root text in
              f start stop symbol
            | _ -> ()
        end ()
      end
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
    end)

  method init_toolbar () =
    let find_and_fill () = self#find_async ~fill:true () in
    ignore (button_back#connect#clicked ~callback:(fun () -> ignore (self#go_back())));
    ignore (button_forward#connect#clicked ~callback:self#go_forward);
    ignore (button_up#connect#clicked ~callback:self#go_up);
    ignore (button_libraries#connect#clicked ~callback:self#go_home);
    ignore (button_modules#connect#clicked ~callback:self#create_widget_modules);
    ignore (button_layout_slist#connect#toggled ~callback:(fun () -> self#toggle_layout `slist));
    ignore (button_layout_odoc#connect#toggled ~callback:(fun () -> self#toggle_layout `odoc));
    ignore (button_detach#connect#clicked ~callback:(fun () -> self#detach button_detach));
    ignore (radio_find_path#connect#toggled ~callback:(fun () -> if radio_find_path#get_active then find_and_fill()));
    ignore (radio_find_name#connect#toggled ~callback:(fun () -> if radio_find_name#get_active then find_and_fill()));
    ignore (radio_find_type#connect#toggled ~callback:(fun () -> if radio_find_type#get_active then find_and_fill()));
    ignore (button_find#connect#clicked ~callback:find_and_fill);
    ignore (entry_find#connect#changed ~callback:self#find_async);
    ignore (button_remove#connect#clicked ~callback:begin fun () ->
      match widget_libraries with
        | None -> ()
        | Some wlibs ->
          let paths = wlibs#view#selection#get_selected_rows in
          let rr = List.map wlibs#model#get_row_reference paths in
          List.iter (fun reference -> ignore (wlibs#model#remove reference#iter)) rr;
          (* set next cursor position *)
          let count = ref 0 in
          wlibs#model#foreach (fun _ _ -> incr count; false);
          let last = GTree.Path.create [!count - 1] in
          let path = min last (match paths with hd :: _ -> hd | _ -> last) in
          wlibs#view#selection#select_path path;
          wlibs#view#misc#grab_focus()
    end);
    (* entry_find *)
    entry_find#misc#set_property "secondary-icon-pixbuf" (`OBJECT (Some Icons.button_close_8));
    entry_find#misc#set_property "secondary-icon-sensitive" (`BOOL false);
    ignore begin GtkSignal.connect
      ~sgn:{
        GtkSignal.name = "icon-press";
        classe         = `widget;
        marshaller     = fun f ->
          GtkSignal.marshal1 Gobject.Data.int "GtkEntry::icon-press" (fun icon_position -> f icon_position)
      }
      ~callback:self#entry_find_icon_press_cb
      entry_find#as_widget
    end;
    ignore (entry_find#event#connect#key_press ~callback:begin fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._Return then begin
        find_and_fill();
        true
      end else if GdkEvent.Key.keyval ev = GdkKeysyms._Down || GdkEvent.Key.keyval ev = GdkKeysyms._Up then begin
        if box_slist#visible then (self#show_current_page());
        true;
      end else false
    end);
    (* Timeout for entry_find *)
    if not is_completion then ignore (Timeout.start tout_entry_find);
    ignore (self#misc#connect#destroy ~callback:begin fun () ->
      Timeout.destroy tout_entry_find;
      self#clear();
    end);
    ignore (self#misc#connect#destroy ~callback:self#clear);

  method private init_tooltips () =
    (*ignore (odoc_view#misc#connect#query_tooltip ~callback:begin fun ~x ~y ~kbd _ ->
      self#tooltip ~x ~y
    end);*)
    ignore (odoc_view#event#connect#focus_in ~callback:begin fun _ ->
      ignore (self#tooltip_destroy()); false
    end);
    ignore (odoc_view#event#connect#focus_out ~callback:begin fun _ ->
      ignore (self#tooltip_destroy()); false
    end);
    ignore (odoc_view#event#connect#button_press ~callback:begin fun ev ->
      if GdkEvent.Button.button ev = 3 then begin
        let x = int_of_float (GdkEvent.Button.x ev) in
        let y = int_of_float (GdkEvent.Button.y ev) in
        odoc_view#misc#grab_focus();
        ignore (self#tooltip_type ~x ~y);
        true
      end else begin
        ignore (self#tooltip_destroy());
        false
      end
    end);
    ignore (odoc_view#event#connect#key_press ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if List.mem key [GdkKeysyms._Left] then true else false
    end)

  val mutable is_onscreen = new GUtil.variable false
  method is_onscreen = is_onscreen

  method private init_timeout_odoc () =
    let need_timeout () = self#is_odoc_visible && self#is_slist_visible in
    let id = ref None in
    let create_timeout _ =
      id := Some (GMain.Timeout.add ~ms:100 ~callback:begin fun () ->
          let has_toplevel_focus = match GWindow.toplevel self#coerce with | Some w -> w#has_toplevel_focus | _ -> false in
          if has_toplevel_focus && (need_timeout()) then begin
            try self#update_module_details ()
            with ex -> Printf.eprintf "File \"module_browser_tool.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
          end;
          true
        end);
    in
    self#as_page#is_active#connect#changed ~callback:is_onscreen#set |> ignore;
    button_layout_odoc#connect#toggled ~callback:(fun () ->  is_onscreen#set (button_layout_slist#get_active && button_layout_odoc#get_active)) |> ignore;
    button_layout_slist#connect#toggled ~callback:(fun () -> is_onscreen#set (button_layout_slist#get_active && button_layout_odoc#get_active)) |> ignore;
    is_onscreen#connect#changed ~callback:begin fun x ->
      if x && need_timeout() then create_timeout() else (Gaux.may !id ~f:GMain.Timeout.remove)
    end |> ignore;
    self#misc#connect#destroy ~callback:(fun () -> Gaux.may !id ~f:GMain.Timeout.remove) |> ignore;

  method private init_key_bindings () =
    ignore (ebox#event#connect#key_release ~callback:begin fun ev ->
      let state = GdkEvent.Key.state ev in
      let key = GdkEvent.Key.keyval ev in
      let box_slist_visible = box_slist#visible in
      let box_odoc_visible = box_odoc#visible in
      if box_slist_visible && state = [`MOD1] && key = _Left then begin
        ignore (self#go_back ());
        true
      end else if box_slist_visible && state = [`MOD1] && key = GdkKeysyms._Right then begin
        self#go_forward();
        true
      end else if box_slist_visible && state = [`MOD1] && key = GdkKeysyms._Up then begin
        self#go_up();
        true
      (*end else if key = GdkKeysyms._Right then begin
        button_layout_odoc#set_active true;
        self#update_module_details ~force:true ();
	Gmisclib.Idle.add odoc_view#misc#grab_focus;
        true
      end else if key = GdkKeysyms._Left then begin
        button_layout_odoc#set_active false;
        Gaux.may (self#get_current_page()) ~f:begin fun (w : symbol_list) ->
          match w#view#selection#get_selected_rows with
            | path :: _ ->
              w#view#scroll_to_cell path w#vc_icon;
              w#view#misc#grab_focus();
            | _ -> ()
        end;
        true*)
      end else if key = GdkKeysyms._F3 then begin
        if entry_find#visible then (entry_find#misc#grab_focus());
        true
      end else if key = GdkKeysyms._F1 || (state = [`MOD1] && key = GdkKeysyms._Return) then begin
        button_layout_odoc#set_active (not button_layout_odoc#get_active);
        true
      end else if box_odoc_visible && (*not is_completion &&*) state = [`CONTROL] && key = GdkKeysyms._e then begin
        odoc_buffer#place_cursor ~where:odoc_buffer#start_iter;
        incremental_search#i_search ~view:(odoc_view :> Text.view) ~project;
        true
      end else false
    end);

  (** entry_find_icon_press_cb *)
  method private entry_find_icon_press_cb = function
    | 0 -> entry_find#misc#grab_focus()
    | 1 ->
      entry_find#set_text "";
      entry_find#misc#grab_focus();
      entry_find#misc#set_property "secondary-icon-sensitive" (`BOOL false);
    | _ -> assert false

  (** tooltip_destroy *)
  method tooltip_destroy () =
    (match tooltip_popup with Some (w, _) -> w#destroy(); tooltip_popup <- None | _ -> ());
    false

  (** tooltip_type *)
  method private tooltip_type ~x ~y =
    let x, y = odoc_view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
    let iter = odoc_view#get_iter_at_location ~x ~y in
    if not (iter#has_tag odoc_tag) && not iter#ends_line && not (Glib.Unichar.isspace iter#char) then begin
      match self#get_current_symbol () with
        | Some symbol ->
          let start, stop = self#get_word_bounds iter in
          let text = odoc_buffer#get_text ~start ~stop () in
          begin
            match self#find_symbol ~root:(Symbols.get_parent_path symbol) text with
              | Some symbol ->
                begin
                  let markup = sprintf "<big>%s</big>\n\n<tt>%s</tt>"
                    (Print_type.markup2 (Symbols.string_of_id symbol.sy_id))
                    (Print_type.markup2 symbol.sy_type)
                  in
                  let label = GMisc.label ~xpad:5 ~ypad:5 ~markup () in
                  Gaux.may (odoc_view#get_window `WIDGET) ~f:begin fun window ->
                    let pX, pY = Gdk.Window.get_pointer_location odoc_view#misc#window in
                    ignore (self#tooltip_destroy());
                    let popup = Gtk_util.window_tooltip label#coerce ~parent:odoc_view ~fade:true ~x:(pX + 0) ~y:(pY + 20) () in
                    tooltip_popup <- Some (popup, markup);
                  end;
                end;
                false
              | _ -> self#tooltip_destroy()
          end
        | _ -> self#tooltip_destroy()
    end else (self#tooltip_destroy())

  method get_current_page () =
    try Some (Stack.top stack_back) with Stack.Empty -> (None : symbol_list option)

  (** get_current_symbol *)
  method get_current_symbol () =
    let widget = Stack.top stack_back in
    match widget#view#selection#get_selected_rows with
      | path :: _ ->
        let row = widget#model#get_iter path in
        let symbol = widget#model#get ~row ~column:col_symbol_data in
        Some symbol
      | _ -> None

  (** get_word_bounds *)
  method private get_word_bounds iter =
    odoc_buffer#select_word ~iter ~pat:Ocaml_word_bound.longid ~select:false ()

  method is_odoc_visible = box_odoc#visible
  method is_slist_visible = box_slist#visible

  (** default_select_func *)
  method private default_select_func slist =
    if slist#length = 1 && slist#view#visible then
      Gaux.may slist#model#get_iter_first ~f:slist#view#selection#select_iter;

  (** select_best_match *)
  method private select_best_match text slist =
    let select_path path =
      Gmisclib.Idle.add begin fun () ->
        if slist#view#visible then begin
          (slist#view :> GTree.view)#scroll_to_cell ~align:(0.38, 0.0) path slist#vc_icon;
          slist#view#set_cursor path slist#vc_icon;
        end
      end
    in
    let regexp = Str.regexp_string_case_fold text in
    let found = ref None in
    (slist#model :> GTree.model)#foreach begin fun path row ->
      let symbol = slist#model#get ~row ~column:Mbrowser_slist.col_symbol_data in
      let value_path = Symbols.concat_value_path symbol in
      if Str.string_match regexp value_path 0 && Str.match_end () = String.length value_path then begin
        found := Some path;
        true
      end else false
    end;
    match !found with
      | Some path -> select_path path
      | None ->
        (slist#model :> GTree.model)#foreach begin fun path row ->
          let symbol = slist#model#get ~row ~column:Mbrowser_slist.col_symbol_data in
          if Str.string_match regexp (Symbols.get_name symbol) 0 then begin
            found := Some path;
            true
          end else false
        end;
        begin
          match !found with
            | Some path -> select_path path
            | None -> self#default_select_func slist
        end;

  (** select_symbol_by_prefix *)
  method select_symbol_by_prefix
      ?module_path
      ~prefix
      ~kind
      (widget : symbol_list) =
    let found = ref false in
    let paths = Index.find_all widget#index prefix in
    let full_prefix, by_value_path =
      match module_path with
        | Some mp -> (String.concat "" (mp @ [prefix])), true
        | _ -> prefix, false
    in
    let find re =
      ignore(List_opt.find begin fun (path, _) ->
        let row = widget#model#get_iter path in
        let sym = widget#model#get ~row ~column:col_symbol_data in
        let name = if by_value_path then String.concat "" sym.Oe.sy_id else Symbols.get_name sym in
        found :=
          if Str.string_match re name 0 && (kind = [] || List.mem sym.sy_kind kind) then begin
            widget#view#selection#select_iter row;
            Gmisclib.Idle.add begin fun () ->
              if widget#view#visible then begin
                widget#view#scroll_to_cell ~align:(0.38, 0.0) path widget#vc_icon;
                widget#view#set_cursor path widget#vc_icon;
              end;
            end;
            true;
          end else false;
          !found
      end paths)
    in
    find (Str.regexp_string full_prefix);
    if not !found then (find (Str.regexp_string_case_fold full_prefix));

  (** find *)
  method find ?(page : Editor_page.page option) ?text ?(fill=false) () =
    let text = match text with None -> entry_find#text | Some x -> x in
    entry_find#misc#set_property "secondary-icon-sensitive" (`BOOL (String.length text > 0));
    if String.length text > 0 then begin
      let pat = (if radio_find_type#get_active then sprintf ".*[\t\r\n .]%s.*" else sprintf ".*%s.*") (Str.quote text) in
      let regexp = Str.regexp_case_fold pat in
      let path = (Project.get_load_path project) in
      let include_locals =
        match page with
          | Some page ->
            Some (project, page#get_filename, (page#buffer#get_iter `INSERT)#offset)
          | _ -> None
      in
      let symbols =
        if radio_find_type#get_active
        then
          Symbols.filter_by_type ~regexp project.Prj.symbols.syt_table
        else
          Symbols.filter_by_name
            ~use_longidents:radio_find_path#get_active
            ~include_methods:true
            ~include_modules:path
            ?include_locals
            ~regexp
            project.Prj.symbols.syt_table
      in
      Gmisclib.Idle.add (fun () -> button_layout_slist#set_active (match symbols with [] | [_] -> false | _ -> true));
      odoc_view#buffer#set_text "";
      let f = self#select_best_match text in
      self#create_widget_search_results ~symbols ~fill ~f ();
    end

  (** find *)
  method private find_async ?text ?(fill=false) () =
    Timeout.set tout_entry_find 0 begin fun () ->
      GtkThread.async (fun () -> self#find ?text ~fill ()) ()
    end;

  (** find_compl *)
  method find_compl ~prefix ~(page : Editor_page.page) ~include_methods ?(f=self#default_select_func) () =
    let path = Project.get_load_path project in
    (*let path = project.Project.ocamllib :: path in*)
    if String.length prefix > 0 then begin
      let regexp = Str.regexp_string_case_fold prefix in
      let offset = (page#buffer#get_iter `INSERT)#offset in
      let symbols = Symbols.filter_by_name
        ~use_longidents:false
        ~include_methods
        ~include_modules:path
        ~include_locals:(project, page#get_filename, offset)
        ~regexp
        project.Prj.symbols.syt_table
      in
      self#create_widget_search_results ~symbols ~f ()
    end;

  (** find_symbol *)
  method private find_symbol ?(root=[]) text =
    let ident = Longident.flatten (Longident.parse text) in
    match Symbols.find_by_modulepath ~kind:type_kinds project.Prj.symbols ident with
      | None ->
        (* If the symbol is relative, search recursively in the parent module *)
        let rec find ?(root=[]) text =
          let absolute_ident = root @ ident in
          match Symbols.find_by_modulepath ~kind:type_kinds project.Prj.symbols absolute_ident with
            | None ->
              if root = [] then None else begin
                let root = (*try*) Xlist.rev_tl root (*with Invalid_argument "Empty List" -> []*) in
                self#find_symbol ~root text
              end
            | symbol -> symbol
        in
        begin
          match find ~root text with
            | None ->
              let pervasives_ident = "Pervasives" :: ident in
              Symbols.find_by_modulepath project.Prj.symbols pervasives_ident
            | symbol -> symbol
        end;
      | symbol -> symbol

  (** create_widget *)
  method private create_widget ~kind ?model ?index ?packing () =
    let widget = new symbol_list ~kind ~is_completion ?model ?index ?width ?height ?packing () in
    widget#view#set_enable_search true;
    widget#sw#set_shadow_type `IN;
    (* To avoid beep *)
    ignore (widget#view#event#connect#key_press ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if List.mem key [GdkKeysyms._Right] then true else false
    end);
    (*  *)
    if not is_completion then
      ignore (widget#connect#backspace ~callback:(fun () -> ignore (self#go_back ())));
    widget

  (** create_widget_libraries *)
  method create_widget_libraries () =
    let wlibs = self#create_widget ~kind:`Directory () in
    widget_libraries <- Some wlibs;
    let ocamllib = project.Prj.ocamllib in
    let project_load_path =
      List.sort begin fun a b ->
        if a = ocamllib then -1 else if b = ocamllib then 1 else compare a b
      end (Project.get_load_path project)
    in
    let is_source_path_relaitve = Miscellanea.filename_relative (Project.path_src project) in
    let is_ocamllib_relative = Miscellanea.filename_relative ocamllib in
    List.iter begin fun path ->
      (*let path = List.fold_left (//) "" (List.map (fun x -> Str.replace_first (!~ ":/") ":\\\\" x) (Miscellanea.filename_split path)) in*)
      let basename = Filename.basename path in
      let kind, basename, add_descr =
        match is_source_path_relaitve path with
          | Some "" -> Lib, basename, Prj.default_dir_src
          | Some x -> Lib, basename, x
          | _ ->
            begin
              match is_ocamllib_relative path with
                | Some "" -> Std_lib, "Standard Library", ocamllib
                | Some x -> Lib, basename, sprintf "+%s" x
                | _ ->
                  Lib, basename,
                  if Filename.is_implicit path then (Prj.default_dir_src ^ "/" ^ (*//*) path) else path
            end
      in
      let row = wlibs#model#append () in
      wlibs#model#set ~row ~column:col_search basename;
      wlibs#model#set ~row ~column:col_type_descr basename;
      wlibs#model#set ~row ~column:col_add_descr (sprintf "<i><small>%s</small></i>" add_descr);
      wlibs#model#set ~row ~column:col_symbol_data
        {sy_id=[basename]; sy_kind=kind; sy_type=""; sy_filename=path; sy_local=false};
    end project_load_path;
    (** row_activated *)
    ignore (wlibs#view#connect#row_activated ~callback:begin fun path vc ->
      let row = wlibs#model#get_iter path in
      let parent = wlibs#model#get ~row ~column:col_symbol_data in
      let lib_path = parent.sy_filename in
      self#create_widget_library ~lib_path ()
    end);
    wlibs#vc_icon#set_visible false;
    wlibs#vc_add_descr#set_visible true;
    wlibs#view#selection#set_mode `MULTIPLE;
    wlibs#view#set_cursor (GTree.Path.create [0]) wlibs#vc_type_descr;
    (*wlibs#view#misc#grab_focus();*)
    wlibs#set_title ~subtitle:"" "Index of Libraries";
    self#push wlibs;

  (** create_widget_library *)
  method private create_widget_library ~lib_path ?(f=fun _ -> ()) () =
    let wlib = self#create_widget ~kind:`Library () in
    let entries = Symbols.Modules.read ~path:[lib_path] () in
    let entries = List.sort Stdlib.compare entries in
    ignore (wlib#view#connect#row_activated ~callback:begin fun path _ ->
      let row = wlib#model#get_iter path in
      let symbol = wlib#model#get ~row ~column:col_symbol_data in
      let module_name = Symbols.get_module_name symbol in
      self#create_widget_module ~lib_path ~module_path:[module_name] ();
    end);
    Index.clear wlib#index;
    List.iter begin fun (module_name, filename) ->
      let row = wlib#model#append () in
      wlib#model#set ~row ~column:col_icon (pixbuf_of_kind Pmodule);
      wlib#model#set ~row ~column:col_search module_name;
      wlib#model#set ~row ~column:col_type_descr (sprintf "<b>%s</b>" module_name);
      wlib#model#set ~row ~column:col_add_descr (sprintf "<i><small>%s</small></i>" filename);
      wlib#model#set ~row ~column:col_symbol_data
        {sy_id=[module_name]; sy_kind=Pmodule; sy_type=""; sy_filename=filename; sy_local=false};
      Index.add wlib#index module_name (wlib#model#get_path row);
    end entries;
    Index.close wlib#index;
    wlib#vc_icon#set_visible false;
    wlib#vc_add_descr#set_visible true;
    if lib_path = project.Prj.ocamllib then
      wlib#set_title (Filename.dirname lib_path) "Standard Library"
    else (wlib#set_title (Filename.dirname lib_path) (Filename.basename lib_path));
    self#push wlib;
    f wlib

  (** create_widget_modules *)
  method create_widget_modules ?(push=true) () =
    let cache_id = Project.filename project in
    try
      let project_load_path = List.sort Stdlib.compare (Project.get_load_path project) in
      let path = (*project.Project.ocamllib ::*) project_load_path in
      let entries = Symbols.Modules.read ~path () in
      let entries = List.sort Stdlib.compare entries in
      let model =
        try
          let (model : GTree.list_store), prev_entries =
            List.assoc cache_id !cache_widget_modules in
          let new_entries = List.filter (fun e -> not (List.mem e prev_entries)) entries in
          if new_entries = [] then begin
            let old_entries = List.filter (fun e -> not (List.mem e entries)) prev_entries in
            if old_entries = [] then model else (raise Exit)
          end else (raise Exit)
        with Not_found ->
          let model = GTree.list_store cols in
          let entries = List.map begin fun ((module_name, filename) as entry) ->
            let row = model#append () in
            model#set ~row ~column:col_icon (pixbuf_of_kind Pmodule);
            model#set ~row ~column:col_search module_name;
            model#set ~row ~column:col_type_descr (sprintf "<b>%s</b>" module_name);
            model#set ~row ~column:col_add_descr (sprintf "<i><small>%s</small></i>" filename);
            model#set ~row ~column:col_symbol_data
              {sy_id=[module_name]; sy_kind=Pmodule; sy_type=""; sy_filename=filename; sy_local=false};
            entry
          end entries in
          cache_widget_modules := List.filter (fun (id, _) -> id <> cache_id) !cache_widget_modules;
          cache_widget_modules := (cache_id, (model, entries)) :: !cache_widget_modules;
          model
      in
      let wmods = self#create_widget ~kind:`Index ~model () in
      ignore (wmods#view#connect#row_activated ~callback:begin fun path _ ->
        let row = wmods#model#get_iter path in
        let symbol = wmods#model#get ~row ~column:col_symbol_data in
        let module_name = Symbols.get_module_name symbol in
        self#create_widget_module ~lib_path:symbol.sy_filename ~module_path:[module_name] ()
      end);
      wmods#vc_icon#set_visible false;
      wmods#vc_add_descr#set_visible true;
      wmods#set_title ~subtitle:"" "Index of Modules";
      (*if button_layout_odoc#get_active then button_layout_both#set_active true;*)
      if push then (self#push wmods);
    with Exit ->
      cache_widget_modules := List.filter (fun (id, _) -> id <> cache_id) !cache_widget_modules;
      self#create_widget_modules ~push ()

  (** create_widget_module *)
  method create_widget_module ~module_path ?(lib_path="") ?(sort=false) ?(f=fun _ -> ()) () =
    let module_path_str = String.concat "." module_path in
    Activity.wrap Activity.Symbol begin fun () ->
      let cache_changed = Symbols.Cache.update ~cache:project.Prj.symbols ~value_path:module_path () in
      let widget =
        try
          if cache_changed then (raise Not_found);
          let (model : GTree.list_store), index = List.assoc module_path_str cache_widget_module in
          self#create_widget ~kind:`Module ~model ~index ()
        with Not_found ->
          let len = List.length module_path + 1 in
          let entries = Symbols.filter_by_modulepath ~update_cache:false project.Prj.symbols module_path in
          let entries = List.filter begin fun s ->
            s.sy_id <> module_path && List.length s.sy_id = len
          end entries in
          let entries = if sort then List.sort (fun a b -> compare a.sy_id b.sy_id) entries else entries in
          let widget = self#create_widget ~kind:`Module () in
          widget#fill entries;
          widget
      in
      ignore (widget#misc#connect#destroy ~callback:begin fun () ->
        let model = widget#model in
        widget#view#set_model None;
        cache_widget_module <- (module_path_str, (model, widget#index)) :: (List.filter (fun (mp, _) -> mp <> module_path_str) cache_widget_module);
      end);
      ignore (widget#view#connect#row_activated ~callback:begin fun path _ ->
        let row = widget#model#get_iter path in
        let symbol = widget#model#get ~row ~column:col_symbol_data in
        match symbol.sy_kind with
          | Pmodule -> self#create_widget_module ~lib_path ~module_path:symbol.sy_id ()
          | Pmodtype -> ()
          | Pclass -> self#create_widget_class ~lib_path ~class_path:symbol.sy_id ()
          | Pcltype -> ()
          | _ -> ()
      end);
      ignore (widget#view#selection#connect#after#changed ~callback:begin fun () ->
        self#update_symbol_details widget ();
      end);
      widget#set_title (*~tooltip:odoc*) ~subtitle:lib_path module_path_str;
      self#push widget;
      self#update_symbol_details widget ();
      f widget;
    end ()

  (** create_widget_class *)
  method create_widget_class ~class_path ?(lib_path="") ?(f=fun _ -> ()) () =
    Activity.wrap Activity.Symbol begin fun () ->
      let widget = self#create_widget ~kind:`Class () in
      let entries = Symbols.filter_methods project.Prj.symbols class_path in
      let entries = List.filter (fun s -> s.sy_id <> class_path) entries in
      let entries = List.sort (fun a b -> compare a.sy_id b.sy_id) entries in
      widget#fill entries;
      widget#set_title lib_path (String.concat "." class_path);
      self#push widget;
      ignore (self#connect#switch_page ~callback:begin fun cur ->
        if widget#misc#get_oid = cur#misc#get_oid then (self#update_class_details widget ())
      end);
      ignore (widget#view#selection#connect#changed ~callback:(fun () -> self#update_class_details widget()));
      self#update_class_details widget ();
      f widget;
    end ()

  (** create_widget_search_results *)
  method private create_widget_search_results ~symbols ?(fill=false) ?(f=self#default_select_func) () =
    let symbols = List.sort (fun a b -> compare (Symbols.get_name a) (Symbols.get_name b)) symbols in
    search_results_length <- List.length symbols;
    let widget : symbol_list =
      match (self#get_current_page() : symbol_list option) with
        | Some current_page when current_page#is_search_output -> current_page
        | _ ->
          let widget = self#create_widget ~kind:`Search () in
          widget#set_is_search_output true;
          widget#vc_type_descr#set_sizing `AUTOSIZE;
          ignore (widget#view#connect#row_activated ~callback:begin fun path _ ->
            let row = widget#model#get_iter path in
            let symbol = widget#model#get ~row ~column:col_symbol_data in
            let lib_path = symbol.sy_filename in
            match symbol.sy_kind with
              | Pmodule -> self#create_widget_module ~lib_path ~module_path:symbol.sy_id ()
              | Pmodtype -> ()
              | Pclass -> self#create_widget_class ~lib_path ~class_path:symbol.sy_id ()
              | Pcltype -> ()
              | _ -> ()
          end);
          ignore (widget#view#selection#connect#after#changed
            ~callback:(fun () -> self#update_symbol_details widget ()));
          self#update_symbol_details widget ();
          self#push widget;
          widget
    in
    label_title#set_label (sprintf "%d results" search_results_length);
    widget#model#clear();
    Index.clear widget#index;
    if not is_completion then begin
      (*if entry_find#text <> "" then entry_find#misc#grab_focus();
      entry_find#set_position (Glib.Utf8.length entry_find#text);*)
    end else widget#view#misc#grab_focus();
    if fill || search_results_length <= Oe_config.module_browser_max_results then begin
      ignore (widget#fill symbols);
      Gmisclib.Idle.add (fun () -> if widget#visible then widget#view#scroll_to_point 0 0)
    end;
    f widget

  (** insert_odoc *)
  method private insert_odoc ~project ~symbol () =
    let odoc = Oe_doc.Database.find_name ~project ~symbol in
    let buffer = (odoc_buffer :> GText.buffer) in
    if odoc <> [] then begin
      let f () =
        List.iter (fun result ->
          Oe_doc.Printer.insert ~buffer ~kind:symbol.Oe.sy_kind result) odoc;
      in
      odoc_buffer#block_signal_handlers ();
      Gaux.may odoc_view#signal_expose ~f:odoc_view#misc#handler_block;
      Gtk_util.with_tag odoc_tag ~buffer f;
      Gaux.may odoc_view#signal_expose ~f:odoc_view#misc#handler_unblock;
      odoc_buffer#unblock_signal_handlers ();
      true
    end else false

  (** insert_odoc_full_module *)
  method private insert_odoc_full_module ~project ~symbol () =
    Gmisclib.Idle.add ~prio:100 begin fun () ->
      match Oe_doc.Database.find_module ~project ~symbol with
        | Some odoc ->
          Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `WATCH);
          Gaux.may (odoc_view#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `WATCH));
          let colorize ~start ~stop = Lexical.tag ~start ~stop in
          odoc_buffer#block_signal_handlers ();
          Gaux.may odoc_view#signal_expose ~f:odoc_view#misc#handler_block;
          Oe_doc.Printer.insert_full_module ~buffer:(odoc_buffer :> GText.buffer) ~colorize ~tag:odoc_tag odoc;
          odoc_buffer#place_cursor ~where:odoc_buffer#start_iter;
          Gmisclib.Idle.add ~prio:100 (fun () ->
            odoc_view#scroll_to_mark ~use_align:false ~within_margin:0.4 ~yalign:0.0 `INSERT);
          Gaux.may odoc_view#signal_expose ~f:odoc_view#misc#handler_unblock;
          odoc_buffer#unblock_signal_handlers ();
          Gdk.Window.set_cursor self#misc#window (Gdk.Cursor.create `ARROW);
          Gaux.may (odoc_view#get_window `TEXT) ~f:(fun w -> Gdk.Window.set_cursor w (Gdk.Cursor.create `ARROW));
        | _ -> kprintf (odoc_buffer :> GText.buffer)#insert "Documentation is not available (%s, %s, %s)"
                 (Symbols.concat_value_path symbol) (symbol.sy_filename) (Symbols.get_module_name symbol);
    end

  (** update_class_details *)
  method private update_class_details widget () =
    match widget#view#selection#get_selected_rows with
      | path :: _ ->
        let row = widget#model#get_iter path in
        let symbol = widget#model#get ~row ~column:col_symbol_data in
        begin
          match symbol.sy_kind with
            | Pmethod | Pmethod_private | Pmethod_virtual | Pmethod_private_virtual ->
              odoc_view#buffer#set_text symbol.sy_type;
              Lexical.tag odoc_view#buffer;
              (*Alignment.align ~view:(odoc_view :> GText.view) ~start:odoc_buffer#start_iter ~stop:odoc_buffer#end_iter;*)
            | _ -> odoc_view#buffer#set_text ""
        end;
        ignore (self#insert_odoc ~project ~symbol ());
      | _ -> ()

  (** update_symbol_details *)
  method private update_symbol_details widget () =
    match widget#view#selection#get_selected_rows with
      | path :: _ ->
        let row = widget#model#get_iter path in
        let symbol = widget#model#get ~row ~column:col_symbol_data in
        let insert_odoc () =
          let odoc_exists = self#insert_odoc ~project ~symbol () in
          if not odoc_exists then begin
            let m1 = odoc_buffer#create_mark (odoc_buffer#get_iter `INSERT) in
            odoc_buffer#set_text symbol.sy_type;
            let start = odoc_buffer#get_iter_at_mark (`MARK m1) in
            let stop = odoc_buffer#get_iter `INSERT in
            Alignment.align ~buffer:(odoc_buffer :> GText.buffer) ~start ~stop;
            odoc_buffer#delete_mark (`MARK m1);
            Lexical.tag odoc_view#buffer;
          end
        in
        begin
          match symbol.sy_kind with
            | Pmodule | Pmodtype | Pcltype ->
              odoc_buffer#set_text "";
              insert_odoc();
            | Ptype | Ptype_abstract | Ptype_variant | Ptype_record ->
              odoc_buffer#set_text "\n";
              odoc_buffer#insert ~tag_names:["large"] (Symbols.string_of_id (Symbols.get_parent_path symbol));
              odoc_buffer#insert "\n";
              (*Lexical.tag odoc_view#buffer;*)
              insert_odoc();
  (*          | Pclass ->
              odoc_buffer#set_text symbol.sy_type;
              Lexical.tag odoc_view#buffer;
              self#insert_odoc ~project ~symbol ();*)
            | _ ->
              odoc_buffer#set_text "\n";
              odoc_buffer#insert ~tag_names:["large"] (Symbols.string_of_id (Symbols.get_parent_path symbol));
              odoc_buffer#insert ~tag_names:["line_spacing_small"] "\n ";
              odoc_buffer#insert ~tag_names:["type2"] ("\n" ^ symbol.sy_type);
              Lexical.tag odoc_view#buffer;
              ignore (self#insert_odoc ~project ~symbol ());
        end;
      | _ -> ()

  (** update_module_details *)
  method private update_module_details ?(force=false) () =
    match self#get_current_page() with
      | Some (slist : symbol_list)
          when List.mem slist#kind [`Library; `Index]
            (*&& slist#view#misc#get_flag `HAS_FOCUS*) ->
        begin
          match slist#view#selection#get_selected_rows with
            | path :: _ ->
              let row = slist#model#get_iter path in
              let symbol = slist#model#get ~row ~column:col_symbol_data in
              begin
                let f () =
                  current_symbol <- Some symbol;
                  odoc_buffer#set_text "";
                  self#insert_odoc_full_module ~project ~symbol ();
                in
                match current_symbol with
                  | Some cs when cs = symbol && odoc_buffer#char_count > 0 -> ()
                  | Some cs when force || (Unix.gettimeofday()) -. slist#ts_changed > 0.5 -> f ()
                  | None -> f()
                  | _ -> ()
              end;
            | _ -> ()
        end;
      | _ -> ()

  (** push *)
  method private push widget =
    stack_box#add widget#coerce;
    if not enable_history then begin
      Stack.iter (fun w -> w#destroy()) stack_back;
      Stack.clear stack_back;
    end;
    Stack.push widget stack_back;
    Stack.iter (fun w -> w#destroy()) stack_forward;
    Stack.clear stack_forward;
    add_page#call widget;
    self#show_current_page ();

  (** clear *)
  method clear () =
    Stack.clear stack_back;
    Stack.clear stack_forward;
    List.iter (fun x -> x#destroy()) stack_box#children;

  (** go_back *)
  method go_back () =
    if Stack.length stack_back > 1 then begin
      (try Stack.push (Stack.pop stack_back) stack_forward with Stack.Empty -> ());
      self#show_current_page();
      false
    end else true (* Home? *)

  (** go_forward *)
  method go_forward () =
    (try Stack.push (Stack.pop stack_forward) stack_back with Stack.Empty -> ());
    self#show_current_page()

  (** go_home *)
  method go_home () =
    while not (self#go_back()) do () done;
    (*if button_layout_odoc#get_active then button_layout_both#set_active true;*)

  (** go_up *)
  method go_up () =
    match self#get_current_page() with
      | Some (current_page : symbol_list) ->
        begin
          match self#get_current_symbol () with
            | Some symbol' (* Symbol under the cursor *) ->
              let lib_path = Filename.dirname symbol'.sy_filename in
              let go child symbol =
                let kind = [Pmodule] in
                let module_path = Symbols.get_parent_path child in
                let prefix = Symbols.get_name child in
                let f = self#select_symbol_by_prefix ~module_path ~prefix ~kind in
                match symbol.sy_kind with
                  | Pmodule -> self#create_widget_module ~lib_path ~module_path:symbol.sy_id ~f ()
                  | Pmodtype -> ()
                  | Pclass -> self#create_widget_class ~lib_path ~class_path:symbol.sy_id ~f ()
                  | Pcltype -> ()
                  | _ -> ()
              in
              let go_parent_lib symbol =
                let lib_path = Filename.dirname symbol.sy_filename in
                let module_path = Symbols.get_parent_path symbol in
                let prefix = String.concat "." module_path in
                let f = self#select_symbol_by_prefix ~prefix ~kind:[] in
                if prefix = "" then self#create_widget_libraries ()
                else self#create_widget_library ~lib_path ~f ()
              in
              begin
                match Symbols.find_parent project.Prj.symbols symbol' with
                  | Some parent when current_page#is_search_output -> go symbol' parent;
                  | Some parent ->
                    begin
                      match Symbols.find_parent project.Prj.symbols parent with
                        | Some p_parent -> go parent p_parent;
                        | _ ->
                          let parent_path = Symbols.get_parent_path parent in
                          let prefix = Symbols.get_name parent in
                          let f = self#select_symbol_by_prefix ~module_path:parent_path ~prefix ~kind:[Pmodule] in
                          self#create_widget_module ~lib_path ~module_path:parent_path ~f ()
                    end;
                  | None -> go_parent_lib symbol'
              end;
            | None -> ()
        end;
      | None -> ()

  (** go_to_symbol *)
  method go_to_symbol ?(kind=type_kinds) symbol =
    let lib_path = Filename.dirname symbol.sy_filename in
    let module_path = Symbols.get_parent_path symbol in
    self#create_widget_module ~lib_path ~module_path
      ~f:(self#select_symbol_by_prefix ~module_path:(Symbols.get_parent_path symbol) ~prefix:(Symbols.get_name symbol) ~kind) ();

  (** show_current_page *)
  method private show_current_page () =
    (*odoc_view#buffer#set_text "";*)
    Stack.iter (fun w -> w#misc#hide()) stack_forward;
    Stack.iter (fun w -> w#misc#hide()) stack_back;
    button_back#misc#set_sensitive (Stack.length stack_back > 1);
    (*button_libraries#misc#set_sensitive (Stack.length stack_back > 1);*)
    button_forward#misc#set_sensitive (Stack.length stack_forward > 0);
    button_up#misc#set_sensitive (Stack.length stack_back > 1(* && current_lib_path <> None*));
    button_add#misc#set_sensitive (Stack.length stack_back = 1);
    button_remove#misc#set_sensitive (Stack.length stack_back = 1);
    try
      let current = Stack.top stack_back in
      current#misc#show ();
      current#view#misc#grab_focus ();
      switch_page#call current;
    with Stack.Empty -> ()

  method private toggle_layout : [ `slist | `odoc ] -> unit = function pane ->
    let (!!) x = x#visible in
    begin
      match pane with
        | `slist ->
          if !! box_slist then box_slist#misc#hide() else box_slist#misc#show();
        | `odoc ->
          if !! box_odoc then box_odoc#misc#hide() else box_odoc#misc#show();
    end;
    if not (!! box_slist) && not (!! box_odoc) then button_layout_slist#set_active true;
    layout_toggled#call()

  method layout : [ `slist | `odoc | `both ] =
    if box_slist#visible && box_odoc#visible then `both
    else if box_slist#visible then `slist
    else if box_odoc#visible then `odoc
    else assert false

  method private set_is_completion is_compl =
    if is_completion then begin
      toolbar#misc#hide();
      label_title#misc#hide();
    end else begin
      toolbar#misc#show();
      label_title#misc#show();
    end;

  method odoc_view = odoc_view

    (** connect *)
  method connect = new widget_signals ~switch_page ~add_page ~layout_toggled
end

(** widget_signals *)
and widget_signals ~switch_page ~add_page ~layout_toggled = object
  inherit GUtil.ml_signals [switch_page#disconnect; add_page#disconnect; layout_toggled#disconnect]
  method switch_page = switch_page#connect ~after
  method add_page = add_page#connect ~after
  method layout_toggled = layout_toggled#connect ~after
end
and switch_page () = object inherit [symbol_list] GUtil.signal () end
and add_page () = object inherit [symbol_list] GUtil.signal () end
and layout_toggled () = object inherit [unit] GUtil.signal () end

(*(** create *)
let create_window ~project =
  let window = GWindow.window ~title ~position:`CENTER ~icon:Icons.oe ~width:900 ~height:600 ~allow_shrink:true ~show:false () in
  let widget = new widget ~project ~packing:window#add () in
  widget#create_widget_libraries ();
  window#show()
;;*)

let reusable_widget : widget option ref = ref None

let append_to_messages ?page ?search_string ~project =
  let create ?search_string () =
    let widget = new widget ~project () in
    messages#set_visible true;
    let hbox = GPack.hbox ~spacing:1 () in
    let icon = GMisc.image ~pixbuf:Icons.module_browser ~packing:hbox#pack () in
    let label = GMisc.label ~text:title ~packing:hbox#add () in
    widget#create_widget_libraries ();
    begin
      match search_string with
        | Some text ->
          widget#find ?page ~text ();
          widget#entry_find#set_text "";
        | _ -> ();
    end;
    let button = messages#append_page ~label_widget:hbox#coerce ~with_spinner:false widget#as_page in
    widget#set_title title;
    widget#set_icon (Some icon#pixbuf);
    ignore (widget#connect#switch_page ~callback:begin fun w ->
        label#set_text w#title.title;
        widget#set_title w#title.title;
        if widget#icon = None then widget#set_icon (Some icon#pixbuf);
      end);
    messages#connect#remove_page ~callback:begin fun page ->
      match !reusable_widget with
        | Some widget when widget#misc#get_oid = page#misc#get_oid ->
          reusable_widget := None
        | _ -> ()
    end |> ignore;
    widget
  in
  match search_string with
    | Some text ->
      begin
        match !reusable_widget with
          | Some widget ->
            widget#find ?page ~text ();
            widget#entry_find#set_text "";
            widget#present();
          | _ ->
            let widget = create ~search_string:text () in
            widget#present();
            reusable_widget := Some widget;
      end;
    | _ ->
      let widget = create () in
      widget#present();
      Gmisclib.Idle.add widget#entry_find#misc#grab_focus

;;
