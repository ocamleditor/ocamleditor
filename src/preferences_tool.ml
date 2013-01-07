(*

  OCamlEditor
  Copyright (C) 2010-2013 Francesco Tovagliari

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

let ocaml_preview =
"(* Syntax Coloring Preview *)
open Printf

(** ocamldoc comment block.
  * @version 1.0
  *)
class sub_logic () =
  object (self)
    inherit core_logic
    (** ocamldoc comment block. *)
    method solve ~required ?optional () =
      let (/$/) = List.nth in
      let number = 1234 in
      printf \"The number is: %d\" number;
      if required /$/ number > 0 then begin
        match optional with
          | Some a ->
            ignore (Array.fold_left
              (fun acc x -> acc && x) a [| true; false |])
          | _ -> raise Not_found
      end else ()
  end
"


(** color_name *)
let color_name color =
  let r, g, b = (Gdk.Color.red color, Gdk.Color.green color, Gdk.Color.blue color) in
  let r, g, b =
    truncate ((float r) /. 65535. *. 255.),
    truncate ((float g) /. 65535. *. 255.),
    truncate ((float b) /. 65535. *. 255.) in
  sprintf "#%02X%02X%02X" r g b

let (//) = Filename.concat

(** create_align *)
let create_align ?title ?(indent=13) ~(vbox : GPack.box) () =
  let box = GPack.vbox ~spacing:8 ~packing:vbox#pack () in
  let indent = match title with
    | None -> 0
    | Some title ->
      let _ = GMisc.label ~markup:(sprintf "<b>%s</b>" title) ~xalign:0.0 ~packing:box#add () in
      indent
  in
  let align = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:box#add () in
  align


let spacing = 13
let xalign = 0.0
let col_spacings = 21
let row_spacings = 5

(** page *)
class virtual page title (box : GPack.box) =
  let tbox = GPack.vbox ~spacing:3 ~packing:box#pack () in
  let _    = GMisc.label ~markup:(sprintf "<big><b>%s</b></big>" title) ~xalign:0.0 ~packing:tbox#pack () in
  let _    = GMisc.separator `HORIZONTAL ~packing:tbox#pack () in
  let _    = box#reorder_child tbox#coerce ~pos:0 in
object
  inherit GObj.widget box#as_widget
  method virtual write : Preferences.t -> unit
  method virtual read : Preferences.t -> unit
  method title = title
end


(** preferences *)
and preferences ~(editor : Editor.editor) () =
  let initial_gtk_theme = Preferences.preferences#get.Preferences.pref_general_theme in
  let window            = GWindow.window ~allow_shrink:false ~allow_grow:false ~resizable:true ~width:750
    ~type_hint:`DIALOG ~modal:true ~title:"Preferences" ~position:`CENTER ~icon:Icons.oe ~show:false () in
  let _                 = Gaux.may (GWindow.toplevel editor) ~f:(fun w -> window#set_transient_for w#as_window) in
  let vbox              = GPack.vbox ~border_width:8 ~spacing:8 ~packing:window#add () in
  let hbox              = GPack.hbox ~spacing:8 ~packing:vbox#add () in
  let cols              = new GTree.column_list in
  let column            = cols#add Gobject.Data.string in
  let model             = GTree.tree_store cols in
  let renderer          = GTree.cell_renderer_text [] in
  let view_column       = GTree.view_column ~title:"File" ~renderer:(renderer, ["text", column]) () in
  let sw                = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:(hbox#pack ~expand:false) () in
  let view              = GTree.view ~model:model ~headers_visible:false ~reorderable:false ~width:140
    ~height:300 ~packing:sw#add () in
  let _                 = view#append_column view_column in
  let _                 = GMisc.separator `HORIZONTAL ~packing:vbox#pack () in
  let button_box        = GPack.button_box `HORIZONTAL ~layout:`END ~border_width:5
    ~spacing:8 ~packing:(vbox#pack ~expand:false) () in
  let ok_button         = GButton.button ~stock:`OK ~packing:button_box#add () in
  let cancel_button     = GButton.button ~stock:`CANCEL ~packing:button_box#add () in
  let reset_button      = GButton.button ~label:"Reset All" ~packing:button_box#add () in
  let _                 = button_box#set_child_secondary reset_button#coerce true in
  let reset_page_button = GButton.button ~label:"Reset Page" ~packing:button_box#add () in
  let _                 = button_box#set_child_secondary reset_page_button#coerce true in
object (self)
  inherit GObj.widget vbox#as_widget
  val mutable pages = []
  val mutable current = ""

  method show_page title =
    let page = List.assoc title pages in
    List.iter (fun (_, p) -> p#misc#hide()) pages;
    page#misc#show();
    current <- title;

  method write () =
    List.iter (fun (_, page) -> page#write Preferences.preferences#get) pages;
    editor#set_tab_pos Preferences.preferences#get.Preferences.pref_tab_pos;
    editor#code_folding_enabled#set Preferences.preferences#get.Preferences.pref_code_folding_enabled;
    editor#show_global_gutter#set Preferences.preferences#get.Preferences.pref_show_global_gutter;
    begin
      match Preferences.preferences#get.Preferences.pref_general_theme with
        | Some theme as new_theme when new_theme <> None && new_theme <> initial_gtk_theme ->
          Gtk_theme.set_theme ~theme ();
        | Some _ as new_theme when new_theme <> None && new_theme = initial_gtk_theme -> ()
        | _ -> self#reset_theme();
    end;

  method ok () =
    self#write();
    Preferences.save();
    window#destroy();

  method cancel () =
    self#reset_theme();
    window#destroy()

  method reset_defaults () =
    let defaults = Preferences.create_defaults() in
    List.iter (fun (_, page) -> page#read defaults) pages;

  method reset_page_defaults () =
    let defaults = Preferences.create_defaults() in
    match List_opt.find (fun (t, _) -> t = current) pages with
      | Some (_, p) -> p#read defaults
      | _ -> ()

  method private reset_theme () =
    Gtk_theme.set_theme ?theme:initial_gtk_theme ()

  method create ?(idle=false) title row  (page : string -> ?packing:(GObj.widget -> unit) -> unit -> page) =
    model#set ~row ~column title;
    let f () =
      let page = page title ~packing:hbox#add () in
      page#misc#hide();
      page#read Preferences.preferences#get;
      pages <- (title, page) :: pages
    in
    if idle then Gmisclib.Idle.add f else f()

  initializer
    (* Tree *)
    let row = model#append () in self#create "General" row (new pref_view);
    let row = model#append () in self#create ~idle:true "Fonts" row (new pref_fonts);
    let row as parent = model#append () in self#create "Editor" row (new pref_editor);
    let row = model#append ~parent () in self#create "Display" row (new pref_editor_display);
    let row = model#append ~parent () in self#create "Actions" row (new pref_editor_actions);
    let row = model#append ~parent () in self#create "Code Templates" row (new pref_templ);
    let row = model#append ~parent () in self#create "Color" row (new pref_color);
    view#expand_all();
    view#selection#set_mode `SINGLE;
    ignore (view#selection#connect#changed ~callback:begin fun () ->
      match view#selection#get_selected_rows with
        | path :: _ ->
          let title = model#get ~row:(model#get_iter path) ~column in
          self#show_page title
        | _ -> ()
    end);
    (* Buttons *)
    ignore (cancel_button#connect#clicked ~callback:self#cancel);
    ignore (ok_button#connect#clicked ~callback:self#ok);
    ignore (reset_button#connect#clicked ~callback:self#reset_defaults);
    ignore (reset_page_button#connect#clicked ~callback:self#reset_page_defaults);
    (*  *)
    Gmisclib.Idle.add (fun () -> view#selection#select_path (GTree.Path.create [0]));
    window#present();
end


(** pref_view *)
and pref_view title ?packing () =
  let vbox                  = GPack.vbox ~spacing ?packing () in
  let align                 = create_align ~vbox () in
  let table                 = GPack.table ~col_spacings ~row_spacings ~packing:align#add ~show:(Oe_config.themes_dir <> None) () in
  let combo_theme, _        = GEdit.combo_box_text ~strings:Gtk_theme.avail_themes ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let _                     = GMisc.label ~text:"Look and feel:" ~xalign ~packing:(table#attach ~top:0 ~left:0 ~expand:`NONE) () in
  let align                 = create_align ~title:"Tabs" ~vbox () in
  let table                 = GPack.table ~col_spacings ~row_spacings ~packing:align#add () in
  let _                     = GMisc.label ~text:"Orientation:" ~xalign ~packing:(table#attach ~top:0 ~left:0 ~expand:`NONE) () in
  let _                     = GMisc.label ~text:"Label type:" ~xalign ~packing:(table#attach ~top:1 ~left:0 ~expand:`NONE) () in
(*  let _ = GMisc.label ~text:"Insertions:" ~xalign ~packing:(table#attach ~top:2 ~left:0 ~expand:`NONE) () in*)
  let combo_orient, _       = GEdit.combo_box_text ~strings:[
      "Top"; "Right"; "Bottom"; "Left"; "Vertical on the left"; "Vertical on the right"
    ] ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let combo_labtype, _       = GEdit.combo_box_text ~strings:["Name"; "Shortname"]
    ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
(*  let combo_insert, _ = GEdit.combo_box_text ~strings:["Insert at end"; "Insert at beginning"; "Sort alphabetically"]
    ~packing:(table#attach ~top:2 ~left:1 ~expand:`X) () in*)
  (* Maximize View *)
  let align                 = create_align ~title:"Workspaces" ~vbox () in
  let box                   = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let table                 = GPack.table ~homogeneous:false ~col_spacings ~row_spacings ~packing:box#pack () in
  let top                   = ref 0 in
  let width                 = 65 in
  let none_action_label     = GMisc.label ~text:"" ~packing:(table#attach ~top:0 ~left:0) () in
  (*let label_menubar = GMisc.label ~width ~text:"Show\nMenubar" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:1) () in*)
  let label_toolbar         = GMisc.label ~width ~text:"Show\nToolbar" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:2) () in
  let label_tabbar          = GMisc.label ~width ~text:"Show\nTabs" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:3) () in
  let label_messages        = GMisc.label ~width ~text:"Keep\nMessages" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:4) () in
  let label_fullscreen      = GMisc.label ~width ~text:"Full-Screen" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:6) () in
  let _                     = incr top in
  let fst_action_label      = GMisc.label ~text:"Workspace 1:" ~xalign:0.0 ~packing:(table#attach ~top:!top ~left:0) () in
  (*let check_menubar_1 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:1) () in*)
  let check_toolbar_1       = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:2) () in
  let check_tabbar_1        = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:3) () in
  let check_messages_1      = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:4) () in
  let check_fullscreen_1    = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:6) () in
  let _                     = incr top in
(*  let snd_action_check = GButton.check_button ~label:"Second level:" ~packing:(table#attach ~top:!top ~left:0) () in*)
  let snd_action_label      = GMisc.label ~text:"Workspace 2:" ~xalign:0.0 ~packing:(table#attach ~top:!top ~left:0) () in
  (*let check_menubar_2 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:1) () in*)
  let check_toolbar_2       = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:2) () in
  let check_tabbar_2        = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:3) () in
  let check_messages_2      = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:4) () in
  let check_fullscreen_2    = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:6) () in
  let use_maximize          = GButton.check_button ~label:"Use maximized window instead of full-screen" ~packing:box#pack () in
(*  let _ = snd_action_check#connect#toggled ~callback:begin fun () ->
    check_menubar_2#misc#set_sensitive snd_action_check#active;
    check_toolbar_2#misc#set_sensitive snd_action_check#active;
    check_tabbar_2#misc#set_sensitive snd_action_check#active;
    check_messages_2#misc#set_sensitive snd_action_check#active;
    check_fullscreen_2#misc#set_sensitive snd_action_check#active;
  end in
  let _                     = snd_action_check#set_active true in
  let _                     = snd_action_check#set_active false in*)
object (self)
  inherit page title vbox

  initializer
    match Oe_config.themes_dir with
      | Some _ ->
        ignore (combo_theme#connect#changed ~callback:begin fun () ->
          let theme = self#get_theme_name() in
          Gtk_theme.set_theme ?theme ()
        end);
      | _ -> ()

  method private get_theme_name () =
    try Some (List.nth Gtk_theme.avail_themes combo_theme#active) with Invalid_argument _ -> None

  method write pref =
    pref.Preferences.pref_general_theme <- self#get_theme_name();
    pref.Preferences.pref_tab_pos <- (match combo_orient#active
      with 0 -> `TOP | 1 | 5 -> `RIGHT | 2 -> `BOTTOM | 3 | 4 -> `LEFT | _ -> assert false);
    pref.Preferences.pref_tab_vertical_text <- (match combo_orient#active
      with 0 | 1 | 2 | 3 -> false | 4 | 5 -> true | _ -> assert false);
    pref.Preferences.pref_tab_label_type <- combo_labtype#active;
    (*pref.Preferences.pref_max_view_1_menubar <- check_menubar_1#active;*)
    pref.Preferences.pref_max_view_1_toolbar <- check_toolbar_1#active;
    pref.Preferences.pref_max_view_1_tabbar <- check_tabbar_1#active;
    pref.Preferences.pref_max_view_1_messages <- check_messages_1#active;
    pref.Preferences.pref_max_view_1_fullscreen <- check_fullscreen_1#active;
    (*pref.Preferences.pref_max_view_2_menubar <- check_menubar_2#active;*)
    pref.Preferences.pref_max_view_2_toolbar <- check_toolbar_2#active;
    pref.Preferences.pref_max_view_2_tabbar <- check_tabbar_2#active;
    pref.Preferences.pref_max_view_2_messages <- check_messages_2#active;
    pref.Preferences.pref_max_view_2_fullscreen <- check_fullscreen_2#active;
    pref.Preferences.pref_max_view_2 <- true (*snd_action_check#active*);
    pref.Preferences.pref_max_view_fullscreen <- not use_maximize#active;

  method read pref =
    combo_theme#set_active (match pref.Preferences.pref_general_theme with
      | Some name -> Xlist.pos name Gtk_theme.avail_themes | _ -> -1);
    combo_orient#set_active (match pref.Preferences.pref_tab_pos, pref.Preferences.pref_tab_vertical_text with
      | `TOP, _ -> 0 | `RIGHT, false -> 1 | `BOTTOM, _ -> 2 | `LEFT, false -> 3
      | `LEFT, true -> 4 | `RIGHT, true -> 5);
    combo_labtype#set_active pref.Preferences.pref_tab_label_type;
    (*check_menubar_1#set_active pref.Preferences.pref_max_view_1_menubar;*)
    check_toolbar_1#set_active pref.Preferences.pref_max_view_1_toolbar;
    check_tabbar_1#set_active pref.Preferences.pref_max_view_1_tabbar;
    check_messages_1#set_active pref.Preferences.pref_max_view_1_messages;
    check_fullscreen_1#set_active pref.Preferences.pref_max_view_1_fullscreen;
    (*check_menubar_2#set_active pref.Preferences.pref_max_view_2_menubar;*)
    check_toolbar_2#set_active pref.Preferences.pref_max_view_2_toolbar;
    check_tabbar_2#set_active pref.Preferences.pref_max_view_2_tabbar;
    check_messages_2#set_active pref.Preferences.pref_max_view_2_messages;
    check_fullscreen_2#set_active pref.Preferences.pref_max_view_2_fullscreen;
    (*snd_action_check#set_active pref.Preferences.pref_max_view_2;*)
    use_maximize#set_active (not pref.Preferences.pref_max_view_fullscreen);
end

(** pref_editor_actions *)
and pref_editor_actions title ?packing () =
  let vbox                        = GPack.vbox ~spacing ?packing () in
  (* Smart Keys *)
  let align                       = create_align ~title:"Smart Keys" ~vbox () in
  let skbox                       = GPack.table ~row_spacings:row_spacings ~col_spacings:col_spacings ~packing:align#add () in
  let _                           = GMisc.label ~text:"Home: " ~xalign:0.0 ~packing:(skbox#attach ~top:0 ~left:0) () in
  let combo_home, _ = GEdit.combo_box_text ~active:0 ~strings:
    ["Move to beginning of first word"; "Move to first column position"]
    ~packing:(skbox#attach ~top:0 ~left:1 ~expand:`X) () in
  let _                           = GMisc.label ~text:"End: " ~xalign:0.0 ~packing:(skbox#attach ~top:1 ~left:0) () in
  let combo_end, _ = GEdit.combo_box_text ~active:0 ~strings:
    ["Move to last column position (includes whitespace)"; "Move to end of last character"]
    ~packing:(skbox#attach ~top:1 ~left:1 ~expand:`X) () in
(*  (* Type Annotation *)
  let align                       = create_align ~title:"Type Annotations" ~vbox () in
  let box                         = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let table_annot_type            = GPack.table ~row_spacings:row_spacings ~col_spacings:col_spacings ~packing:box#add () in
  let check_annot_type_enabled    = GButton.check_button ~label:"Enable tooltips"
    ~packing:(table_annot_type#attach ~top:0 ~left:0) () in
  let combo_annot_type_tooltips_delay, _ = GEdit.combo_box_text ~strings:
    ["Show immediately"; "Show after delay"]
    ~packing:(table_annot_type#attach ~top:0 ~left:1 ~expand:`X) () in
(*  let _ = GMisc.label ~show:false ~text:"Implementation: " ~xalign:0.0 ~packing:(table_annot_type#attach ~top:1 ~left:0) () in
  let combo_annot_type_tooltips_impl, _ = GEdit.combo_box_text ~show:false ~strings:
    ["Use popups"; "Use GTK tooltips"]
    ~packing:(table_annot_type#attach ~top:1 ~left:1 ~expand:`X) () in*)
  let _                           = check_annot_type_enabled#connect#toggled ~callback:begin fun () ->
    combo_annot_type_tooltips_delay#misc#set_sensitive check_annot_type_enabled#active;
  end in*)
  (* File Saving *)
  let align                       = create_align ~title:"File Saving" ~vbox () in
  let box                         = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let check_save_all_bef_comp     = GButton.check_button ~label:"Save all before compiling" ~packing:box#pack () in
  let check_bak                   = GButton.check_button ~label:"Create a backup copy of files before saving" ~packing:box#pack () in
  let check_trim                  = GButton.check_button ~label:"Strip trailing whitespace" ~packing:box#pack () in
  (* Searching *)
  let align                       = create_align ~title:"Searching" ~vbox () in
  let box                         = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let check_search_word_at_cursor = GButton.check_button ~label:"Search word at cursor" ~packing:box#pack () in
object
  inherit page title vbox

(*  initializer
    combo_annot_type_tooltips_delay#misc#set_sensitive check_annot_type_enabled#active;*)

  method write pref =
    pref.Preferences.pref_editor_bak <- check_bak#active;
    pref.Preferences.pref_editor_trim_lines <- check_trim#active;
    pref.Preferences.pref_smart_keys_home <- combo_home#active;
    pref.Preferences.pref_smart_keys_end <- combo_end#active;
(*    pref.Preferences.pref_annot_type_tooltips_enabled <- check_annot_type_enabled#active;
    pref.Preferences.pref_annot_type_tooltips_delay <- combo_annot_type_tooltips_delay#active;
    (*pref.Preferences.pref_annot_type_tooltips_impl <- combo_annot_type_tooltips_impl#active;*)*)
    pref.Preferences.pref_search_word_at_cursor <- check_search_word_at_cursor#active;
    pref.Preferences.pref_editor_save_all_bef_comp <- check_save_all_bef_comp#active;

  method read pref =
    check_bak#set_active pref.Preferences.pref_editor_bak;
    check_trim#set_active pref.Preferences.pref_editor_trim_lines;
    combo_home#set_active pref.Preferences.pref_smart_keys_home;
    combo_end#set_active pref.Preferences.pref_smart_keys_end;
(*    check_annot_type_enabled#set_active pref.Preferences.pref_annot_type_tooltips_enabled;
    combo_annot_type_tooltips_delay#set_active pref.Preferences.pref_annot_type_tooltips_delay;
    (*combo_annot_type_tooltips_impl#set_active pref.Preferences.pref_annot_type_tooltips_impl;*)*)
    check_search_word_at_cursor#set_active pref.Preferences.pref_search_word_at_cursor;
    check_save_all_bef_comp#set_active pref.Preferences.pref_editor_save_all_bef_comp
end

(** pref_fonts *)
and pref_fonts title ?packing () =
  let vbox         = GPack.vbox ~spacing ?packing () in
  let notebook     = GPack.notebook ~packing:vbox#add () in
  let border_width = 5 in
  let preview_text = "abcdefghijk ABCDEFGHIJK òàùèéì → •" in
  let font_editor  = GMisc.font_selection ~preview_text ~border_width () in
  let box_compl    = GPack.vbox ~border_width ~spacing:8 () in
  let font_compl   = GMisc.font_selection ~preview_text ~packing:box_compl#add () in
  let button_greek = GButton.check_button ~label:"Use greek letters in types" ~packing:box_compl#pack () in
  let font_other   = GMisc.font_selection ~preview_text ~border_width () in
  let font_odoc    = GMisc.font_selection ~preview_text ~border_width () in
  let _            = notebook#append_page ~tab_label:(GMisc.label ~text:"Editor" ())#coerce font_editor#coerce in
  let _            = notebook#append_page ~tab_label:(GMisc.label ~text:"Completion" ())#coerce box_compl#coerce in
  let _            = notebook#append_page ~tab_label:(GMisc.label ~text:"Output" ())#coerce font_other#coerce in
  let _            = notebook#append_page ~tab_label:(GMisc.label ~text:"Documentation" ())#coerce font_odoc#coerce in
object
  inherit page title vbox

  method write pref =
    pref.Preferences.pref_base_font <- font_editor#font_name;
    pref.Preferences.pref_compl_font <- font_compl#font_name;
    pref.Preferences.pref_output_font <- font_other#font_name;
    pref.Preferences.pref_odoc_font <- font_odoc#font_name;
    pref.Preferences.pref_compl_greek <- button_greek#active;

  method read pref =
    font_editor#set_font_name pref.Preferences.pref_base_font;
    font_compl#set_font_name pref.Preferences.pref_compl_font;
    font_other#set_font_name pref.Preferences.pref_output_font;
    font_odoc#set_font_name pref.Preferences.pref_odoc_font;
    button_greek#set_active pref.Preferences.pref_compl_greek;
end


(** pref_editor *)
and pref_editor title ?packing () =
  let vbox                  = GPack.vbox ~spacing ?packing () in
  let align                 = create_align ~title:"Tab Settings/Word Wrap" ~vbox () in
  let box                   = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let tbox                  = GPack.hbox ~spacing:8 ~packing:box#pack () in
  let _                     = GMisc.label ~text:"Tab width:" ~packing:tbox#pack () in
  let adjustment            = GData.adjustment ~page_size:0.0 () in
  let entry_tab_width       = GEdit.spin_button ~adjustment ~rate:1.0 ~digits:0 ~numeric:true ~packing:tbox#add () in
  let check_tab_spaces      = GButton.check_button ~label:"Use spaces instead of tabs" ~packing:tbox#add () in
  let _                     = check_tab_spaces#misc#set_sensitive false in
  let check_wrap            = GButton.check_button ~label:"Wrap text, breaking lines between words" ~packing:box#pack () in
  (** Line spacing *)
  let align                 = create_align ~title:"Spacing" ~vbox () in
  let box                   = GPack.table ~row_spacings ~col_spacings ~packing:align#add () in
  let _                     = GMisc.label ~text:"Left margin (pixels):" ~xalign:0. ~packing:(box#attach ~top:0 ~left:0) () in
  let adjustment            = GData.adjustment ~lower:1.0 ~page_size:0.0 () in
  let entry_left_margin     = GEdit.spin_button ~adjustment ~rate:1.0 ~digits:0 ~numeric:true ~packing:(box#attach ~top:0 ~left:1) () in
  let _                     = GMisc.label ~text:"Pixels above lines:" ~xalign:0. ~packing:(box#attach ~top:1 ~left:0) () in
  let adjustment            = GData.adjustment ~page_size:0.0 () in
  let entry_pixels_above    = GEdit.spin_button ~adjustment ~rate:1.0 ~digits:0 ~numeric:true ~packing:(box#attach ~top:1 ~left:1) () in
  let _                     = GMisc.label ~text:"Pixels below lines:" ~xalign:0. ~packing:(box#attach ~top:2 ~left:0) () in
  let adjustment            = GData.adjustment ~page_size:0.0 () in
  let entry_pixels_below    = GEdit.spin_button ~adjustment ~rate:1.0 ~digits:0 ~numeric:true ~packing:(box#attach ~top:2 ~left:1) () in
  (** Error indication *)
  let align                 = create_align ~title:"Error Indication" ~vbox () in
  let box                   = GPack.table ~row_spacings ~col_spacings ~packing:align#add () in
  let check_error_underline = GButton.check_button ~label:"Underline errors" ~packing:(box#attach ~top:0 ~left:0) () in
  let check_error_gutter    = GButton.check_button ~label:"Show in gutter" ~packing:(box#attach ~top:0 ~left:1) () in
  let check_error_tooltip   = GButton.check_button ~label:"Show tooltip" ~packing:(box#attach ~top:1 ~left:0) () in
object
  inherit page title vbox

  method write pref =
    pref.Preferences.pref_editor_tab_width <- entry_tab_width#value_as_int;
    pref.Preferences.pref_editor_tab_spaces <- check_tab_spaces#active;
    pref.Preferences.pref_editor_wrap <- check_wrap#active;
    pref.Preferences.pref_editor_left_margin <- entry_left_margin#value_as_int;
    pref.Preferences.pref_editor_pixels_lines <- entry_pixels_above#value_as_int, entry_pixels_below#value_as_int;
    pref.Preferences.pref_err_underline <- check_error_underline#active;
    pref.Preferences.pref_err_tooltip <- check_error_tooltip#active;
    pref.Preferences.pref_err_gutter <- check_error_gutter#active;

  method read pref =
    entry_tab_width#set_value (float pref.Preferences.pref_editor_tab_width);
    check_tab_spaces#set_active pref.Preferences.pref_editor_tab_spaces;
    check_wrap#set_active pref.Preferences.pref_editor_wrap;
    entry_left_margin#set_value (float pref.Preferences.pref_editor_left_margin);
    let above, below = pref.Preferences.pref_editor_pixels_lines in
    entry_pixels_above#set_value (float above);
    entry_pixels_below#set_value (float below);
    check_error_underline#set_active (pref.Preferences.pref_err_underline);
    check_error_tooltip#set_active (pref.Preferences.pref_err_tooltip);
    check_error_gutter#set_active (pref.Preferences.pref_err_gutter);
end

(** pref_editor_display *)
and pref_editor_display title ?packing () =
  let indent                       = 25 in
  let vbox                         = GPack.vbox ~spacing ?packing () in
  let align                        = create_align ~title:"Settings" ~vbox () in
  let box                          = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let check_show_line_numbers      = GButton.check_button ~label:"Show line numbers" ~packing:box#pack () in
  let check_highlight_current_line = GButton.check_button ~label:"Highlight current line" ~packing:box#pack () in
  let clb_vbox                     = GPack.vbox ~spacing:5 ~packing:box#pack () in
  let clb_align                    = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:clb_vbox#add () in
  let check_current_line_border    = GButton.check_button ~label:"Draw border" ~packing:clb_align#add () in
  let check_indent_lines           = GButton.check_button ~label:"Show indentation guidelines" ~packing:box#pack () in
  let il_vbox                      = GPack.vbox ~spacing:5 ~packing:box#pack () in
  let il_align                     = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:il_vbox#add () in
  let il_vbox                      = GPack.vbox ~spacing:5 ~packing:il_align#add () in
  let il_hbox                      = GPack.hbox ~spacing:5 ~packing:il_vbox#add () in
  let _                            = GMisc.label ~text:"Solid lines color:" ~packing:il_hbox#pack () in
  let il_button_solid              = GButton.color_button ~packing:(il_hbox#pack ~fill:false) () in
  let _                            = il_button_solid#set_relief `NONE in
  let _                            = GMisc.label ~text:"Dashed lines color:" ~packing:il_hbox#pack () in
  let il_button_dashed             = GButton.color_button ~packing:(il_hbox#pack ~fill:false) () in
  let _                            = il_button_dashed#set_relief `NONE in
  let check_show_dot_leaders       = GButton.check_button ~label:"Show dot leaders" ~packing:box#pack () in
  let check_code_folding           = GButton.check_button ~label:"Enable code folding" ~packing:box#pack () in
  let check_global_gutter          = GButton.check_button ~label:"Show global gutter" ~packing:box#pack () in
  let rm_vbox                      = GPack.vbox ~spacing:5 ~packing:box#pack () in
  let hbox                         = GPack.hbox ~spacing:5 ~packing:rm_vbox#pack () in
  let adjustment                   = GData.adjustment ~lower:0. ~upper:300. ~step_incr:1. ~page_size:0. () in
  let check_right_margin           = GButton.check_button ~active:false ~label:"Visible right margin at column:" ~packing:hbox#pack () in
  let entry_right_margin           = GEdit.spin_button ~numeric:true ~digits:0 ~rate:1.0 ~adjustment ~packing:hbox#pack () in
  let rm_align                     = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:rm_vbox#add () in
  let rm_hbox                      = GPack.hbox ~spacing:5 ~packing:rm_align#add () in
  let _                            = GMisc.label ~text:"Color:" ~packing:rm_hbox#pack () in
  let rm_button                    = GButton.color_button ~packing:(rm_hbox#pack ~fill:false) () in
  let _                            = rm_button#set_relief `NONE in
  let mo_vbox                      = GPack.vbox ~spacing:5 ~packing:box#pack () in
  let check_mark_occurrences       = GButton.check_button ~active:false ~label:"Highlight all occurrences of the selected word" ~packing:mo_vbox#pack () in
  let mo_align                     = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:mo_vbox#add () in
  let mo_hbox                      = GPack.hbox ~spacing:5 ~packing:mo_align#add () in
  let _                            = GMisc.label ~text:"Color:" ~packing:mo_hbox#pack () in
  let mo_button                    = GButton.color_button ~packing:(mo_hbox#pack ~fill:false) () in
  let _                            = mo_button#set_relief `NONE in
object
  inherit page title vbox

  initializer
    ignore (check_right_margin#connect#toggled ~callback:begin fun () ->
      entry_right_margin#misc#set_sensitive check_right_margin#active;
    end);
  ignore (check_mark_occurrences#connect#toggled ~callback:begin fun () ->
    mo_hbox#misc#set_sensitive check_mark_occurrences#active
  end);
  ignore (check_highlight_current_line#connect#toggled ~callback:begin fun () ->
    check_current_line_border#misc#set_sensitive check_highlight_current_line#active
  end);
  ignore (check_indent_lines#connect#toggled ~callback:begin fun () ->
    il_align#misc#set_sensitive check_indent_lines#active
  end);
  ignore (check_right_margin#connect#toggled ~callback:begin fun () ->
    rm_align#misc#set_sensitive check_right_margin#active
  end);

  method write pref =
    pref.Preferences.pref_highlight_current_line <- check_highlight_current_line#active;
    pref.Preferences.pref_show_line_numbers <- check_show_line_numbers#active;
    pref.Preferences.pref_editor_indent_lines <- check_indent_lines#active;
    let color = color_name rm_button#color in
    pref.Preferences.pref_right_margin_visible <- check_right_margin#active;
    pref.Preferences.pref_right_margin_color <- color;
    pref.Preferences.pref_right_margin <- entry_right_margin#value_as_int;
    pref.Preferences.pref_code_folding_enabled <- check_code_folding#active;
    pref.Preferences.pref_show_global_gutter <- check_global_gutter#active;
    let color = color_name mo_button#color in
    pref.Preferences.pref_editor_mark_occurrences <- check_mark_occurrences#active, color;
    pref.Preferences.pref_editor_dot_leaders <- check_show_dot_leaders#active;
    pref.Preferences.pref_editor_current_line_border <- check_current_line_border#active;
    pref.Preferences.pref_editor_indent_lines_color_s <- color_name il_button_solid#color;
    pref.Preferences.pref_editor_indent_lines_color_d <- color_name il_button_dashed#color;

  method read pref =
    check_highlight_current_line#set_active pref.Preferences.pref_highlight_current_line;
    check_show_line_numbers#set_active pref.Preferences.pref_show_line_numbers;
    let enabled = pref.Preferences.pref_editor_indent_lines in
    check_indent_lines#set_active (not enabled);
    check_indent_lines#set_active enabled;
    let enabled = pref.Preferences.pref_right_margin_visible in
    check_right_margin#set_active (not enabled);
    check_right_margin#set_active enabled;
    entry_right_margin#set_value (float pref.Preferences.pref_right_margin);
    rm_button#set_color (GDraw.color (`NAME pref.Preferences.pref_right_margin_color));
    check_code_folding#set_active (pref.Preferences.pref_code_folding_enabled);
    check_global_gutter#set_active (pref.Preferences.pref_show_global_gutter);
    let enabled, color = pref.Preferences.pref_editor_mark_occurrences in
    check_mark_occurrences#set_active (not enabled);
    check_mark_occurrences#set_active enabled;
    mo_button#set_color (GDraw.color (`NAME color));
    check_show_dot_leaders#set_active pref.Preferences.pref_editor_dot_leaders;
    check_current_line_border#set_active pref.Preferences.pref_editor_current_line_border;
    check_highlight_current_line#set_active (not pref.Preferences.pref_highlight_current_line);
    check_highlight_current_line#set_active pref.Preferences.pref_highlight_current_line;
    il_button_solid#set_color (GDraw.color (`NAME pref.Preferences.pref_editor_indent_lines_color_s));
    il_button_dashed#set_color (GDraw.color (`NAME pref.Preferences.pref_editor_indent_lines_color_d));
end

(** pref_color *)
and pref_color title ?packing () =
  let vbox                = GPack.vbox ~spacing ?packing () in
  let notebook            = GPack.notebook ~packing:vbox#add () in
  let border_width        = 5 in
  (* tags *)
  let cols                = new GTree.column_list in
  let tag_col             = cols#add Gobject.Data.string in
  let lab_col             = cols#add Gobject.Data.string in
  let tag_model           = GTree.list_store cols in
  (* Syntax coloring *)
  let color_ocaml         = GPack.vbox ~border_width ~spacing:8 () in
  let _                   = notebook#append_page ~tab_label:(GMisc.label ~text:"OCaml" ())#coerce color_ocaml#coerce in
  let hbox                = GPack.hbox ~spacing:8 ~packing:color_ocaml#pack () in
  let _                   = GMisc.label ~text:"Default background color:" ~xalign ~packing:hbox#pack () in
  let button_default_bg   = GButton.color_button ~packing:hbox#pack () in
  let _                   = button_default_bg#set_relief `NONE in
  let box_tag             = GPack.hbox ~border_width:0 ~spacing:8 ~packing:color_ocaml#pack () in
  let sw                  = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:box_tag#pack () in
  let view_tag            = GTree.view ~width:200 ~height:150 ~headers_visible:false ~model:tag_model ~packing:sw#add () in
  let renderer            = GTree.cell_renderer_text [] in
  let vc_tag              = GTree.view_column ~renderer:(renderer, ["text", tag_col]) () in
  let _                   = view_tag#append_column vc_tag in
  let vc_lab              = GTree.view_column ~title:"Text Elements" ~renderer:(renderer, ["text", lab_col]) () in
  let _                   = view_tag#append_column vc_lab in
  let _                   = vc_tag#set_visible false in
  let prop_box            = GPack.vbox ~border_width:0 ~packing:box_tag#add () in
  let table               = GPack.table ~packing:prop_box#pack () in
  let label_tag_bg        = GMisc.label ~text:"Background:" ~xalign:0.0 ~packing:(table#attach ~top:0 ~left:0) () in
  let button_tag_bg       = GButton.color_button ~packing:(table#attach ~top:0 ~left:1) () in
  let _                   = button_tag_bg#set_relief `NONE in
  let check_tag_bg        = GButton.check_button ~label:"Use default" ~packing:(table#attach ~top:0 ~left:2) () in
  let _                   = GMisc.label ~text:"Foreground:" ~xalign:0.0 ~packing:(table#attach ~top:1 ~left:0) () in
  let button_tag_fg       = GButton.color_button ~packing:(table#attach ~top:1 ~left:1) () in
  let _                   = button_tag_fg#set_relief `NONE in
  let check_tag_weight    = GButton.check_button ~label:"Bold" ~packing:(table#attach ~top:2 ~left:0) () in
  let check_tag_style     = GButton.check_button ~label:"Italic" ~packing:(table#attach ~top:3 ~left:0) () in
  let check_tag_underline = GButton.check_button ~label:"Underline" ~packing:(table#attach ~top:4 ~left:0) () in
  let _                   = check_tag_weight#set_image (GMisc.image ~stock:`BOLD ())#coerce in
  let _                   = check_tag_style#set_image (GMisc.image ~stock:`ITALIC ())#coerce in
  let _                   = check_tag_underline#set_image (GMisc.image ~stock:`UNDERLINE ())#coerce in
  let box_odoc_bg         = GPack.hbox ~spacing:5 ~packing:prop_box#pack ~show:false () in
  let _                   = GMisc.label ~xalign:0.0
    ~markup:"Paragraph background colors:" (* \n<small>(only applies to comments preceded\nby a blank line)</small> *)
    ~packing:box_odoc_bg#pack () in
  let button_odoc_bg      = GButton.color_button ~packing:box_odoc_bg#pack () in
  let _                   = button_odoc_bg#set_relief `NONE in
  let button_odoc_bg2     = GButton.color_button ~packing:box_odoc_bg#pack () in
  let _                   = button_odoc_bg2#set_relief `NONE in
  (* OCaml Preview *)
  let osw                 = GBin.scrolled_window ~height:200 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN () in
  let buffer              = new Ocaml_text.buffer ~lexical_enabled:true () in
  let _                   = buffer#set_text ocaml_preview in
  let preview             = new Ocaml_text.view ~buffer () in
  let _                   = Gmisclib.Idle.add (fun () -> preview#buffer#place_cursor ~where:(preview#buffer#start_iter#forward_lines 5)) in
  let _                   = Preferences_apply.apply (preview :> Text.view) Preferences.preferences#get in
  let _                   = preview#set_editable false in
  let _                   = osw#add preview#coerce in
  let _                   = color_ocaml#add osw#coerce in
  (* color_compl *)
  let color_compl         = GPack.vbox ~border_width () in
  let table               = GPack.table ~row_spacings ~col_spacings ~packing:color_compl#pack () in
  let _                   = GMisc.label ~text:"Background color:" ~xalign ~packing:(table#attach ~top:0 ~left:0) () in
  let _                   = GMisc.label ~text:"Foreground color:" ~xalign ~packing:(table#attach ~top:1 ~left:0) () in
  let button_tag_bg_popup = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let button_tag_fg_popup = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let _                   = notebook#append_page ~tab_label:(GMisc.label ~text:"Completion" ())#coerce color_compl#coerce in
  (* color_other *)
  let color_other         = GPack.vbox ~border_width () in
  let table               = GPack.table ~row_spacings ~col_spacings ~packing:color_other#pack () in
  let button_bg           = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let button_fg_stdin     = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let button_fg_stdout    = GButton.color_button ~packing:(table#attach ~top:2 ~left:1 ~expand:`X) () in
  let button_fg_err       = GButton.color_button ~packing:(table#attach ~top:3 ~left:1 ~expand:`X) () in
  let button_fg_warn      = GButton.color_button ~packing:(table#attach ~top:4 ~left:1 ~expand:`X) () in
  let _                   = GMisc.label ~xalign ~text:"Background color:" ~packing:(table#attach ~top:0 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Standard input:" ~packing:(table#attach ~top:1 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Standard output:" ~packing:(table#attach ~top:2 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Errors:" ~packing:(table#attach ~top:3 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Warnings:" ~packing:(table#attach ~top:4 ~left:0) () in
  (* *)
  let _                   = notebook#append_page ~tab_label:(GMisc.label ~text:"Message Pane" ())#coerce color_other#coerce in
object (self)
  inherit page title vbox
  val mutable tags = []
  val mutable current_tag = ""
  val mutable signals = []

  initializer
    ignore (view_tag#selection#connect#after#changed ~callback:self#read_tags);
    signals <- [
      check_tag_bg#coerce,        check_tag_bg#connect#toggled ~callback:self#update_preview;
      button_tag_fg#coerce,       button_tag_fg#connect#color_set ~callback:self#update_preview;
      button_tag_bg#coerce,       button_tag_bg#connect#color_set ~callback:self#update_preview;
      button_default_bg#coerce,   button_default_bg#connect#color_set ~callback:self#update_preview;
      button_odoc_bg#coerce,      button_odoc_bg#connect#color_set ~callback:self#update_preview;
      button_odoc_bg2#coerce,     button_odoc_bg2#connect#color_set ~callback:self#update_preview;
      check_tag_weight#coerce,    check_tag_weight#connect#clicked ~callback:self#update_preview;
      check_tag_style#coerce,     check_tag_style#connect#clicked ~callback:self#update_preview;
      check_tag_underline#coerce, check_tag_underline#connect#clicked ~callback:self#update_preview;
    ];
    ignore (check_tag_bg#connect#after#clicked ~callback:(fun () ->
      button_tag_bg#misc#set_sensitive (not check_tag_bg#active)));


  method write pref =
    pref.Preferences.pref_bg_color_popup <- color_name button_tag_bg_popup#color;
    pref.Preferences.pref_fg_color_popup <- color_name button_tag_fg_popup#color;
    pref.Preferences.pref_bg_color <- (color_name button_default_bg#color, false);
    pref.Preferences.pref_tags <- tags;
    let ltags, prop = List.split tags in
    Lexical.tags := ltags;
    Lexical.colors := prop;
    pref.Preferences.pref_output_bg <- color_name button_bg#color;
    pref.Preferences.pref_output_fg_stdin <- color_name button_fg_stdin#color;
    pref.Preferences.pref_output_fg_stdout <- color_name button_fg_stdout#color;
    pref.Preferences.pref_output_fg_err <- color_name button_fg_err#color;
    pref.Preferences.pref_output_fg_warn <- color_name button_fg_warn#color;
    pref.Preferences.pref_ocamldoc_paragraph_bgcolor_1 <- Some (color_name button_odoc_bg#color);
    pref.Preferences.pref_ocamldoc_paragraph_bgcolor_2 <- Some (color_name button_odoc_bg2#color);

  method read pref =
    button_tag_bg_popup#set_color (GDraw.color (`NAME pref.Preferences.pref_bg_color_popup));
    button_tag_fg_popup#set_color (GDraw.color (`NAME pref.Preferences.pref_fg_color_popup));
    tags <- List.sort Pervasives.compare pref.Preferences.pref_tags;
    button_default_bg#set_color (GDraw.color (`NAME (fst pref.Preferences.pref_bg_color)));
    tag_model#clear();
    List.iter begin fun tag ->
      try
        let row = tag_model#append () in
        let label = List.assoc (fst tag) Preferences.tag_labels in
        tag_model#set ~row ~column:tag_col (fst tag);
        tag_model#set ~row ~column:lab_col label;
      with Not_found -> begin
        fprintf stderr "Preferences_tool, tag not found: %S\n%!" (fst tag);
      end
    end tags;
    view_tag#selection#select_path (GTree.Path.create [0]);
    button_bg#set_color (GDraw.color (`NAME pref.Preferences.pref_output_bg));
    button_fg_stdin#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_stdin));
    button_fg_stdout#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_stdout));
    button_fg_err#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_err));
    button_fg_warn#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_warn));
    Gaux.may pref.Preferences.pref_ocamldoc_paragraph_bgcolor_1 ~f:(fun color -> button_odoc_bg#set_color (GDraw.color (`NAME color)));
    Gaux.may pref.Preferences.pref_ocamldoc_paragraph_bgcolor_2 ~f:(fun color -> button_odoc_bg2#set_color (GDraw.color (`NAME color)));
    self#update_preview ();

  method private read_tags () =
    match view_tag#selection#get_selected_rows with
      | path :: _ ->
        List.iter (fun (w, s) -> w#misc#handler_block s) signals;
        let row = tag_model#get_iter path in
        let tname = tag_model#get ~row ~column:tag_col in
        current_tag <- tname;
        begin
          match List_opt.assoc tname tags with
            | Some (color, weight, style, underline, _, (bg_default, bg_color)) ->
              button_tag_fg#set_color (GDraw.color color);
              check_tag_weight#set_active (weight <> `NORMAL);
              check_tag_style#set_active (style <> `NORMAL);
              check_tag_underline#set_active (underline <> `NONE);
              check_tag_bg#set_active bg_default;
              button_tag_bg#set_color (GDraw.color bg_color);
            | _ -> ()
        end;
        if Oe_config.ocamldoc_paragraph_bgcolor_enabled && tname = "ocamldoc"
        then begin
          box_odoc_bg#misc#show();
          label_tag_bg#misc#set_sensitive false;
          button_tag_bg#misc#set_sensitive false;
          check_tag_bg#misc#set_sensitive false;
        end else begin
          box_odoc_bg#misc#hide();
          label_tag_bg#misc#set_sensitive true;
          button_tag_bg#misc#set_sensitive true;
          check_tag_bg#misc#set_sensitive true;
        end;
        button_tag_bg#misc#set_sensitive (not check_tag_bg#active);
        self#update_preview();
        List.iter (fun (w, s) -> w#misc#handler_unblock s) signals;
      | [] -> ()

  method private update_preview () =
    let color      = button_tag_fg#color in
    let weight     = if check_tag_weight#active then `BOLD else `NORMAL in
    let style      = if check_tag_style#active then `ITALIC else `NORMAL in
    let underline  = if check_tag_underline#active then `SINGLE else `NONE in
    let bg_default = check_tag_bg#active in
    let bg_color   = button_tag_bg#color in
    if current_tag <> "" then begin
      tags <- (current_tag,
        (`NAME (color_name color), weight, style, underline, `MEDIUM, (bg_default, `NAME (color_name bg_color)))) ::
          (List.remove_assoc current_tag tags);
    end;
    let temp_pref = {Preferences.preferences#get with
      Preferences.pref_bg_color = (color_name button_default_bg#color, false);
      Preferences.pref_tags = tags;
      Preferences.pref_ocamldoc_paragraph_bgcolor_1 = Some (color_name button_odoc_bg#color);
      Preferences.pref_ocamldoc_paragraph_bgcolor_2 = Some (color_name button_odoc_bg2#color);
    } in
    let tag_names, colors = List.split tags in
    preview#tbuffer#init_tags ~tags:tag_names ~colors
      ~ocamldoc_paragraph_bgcolor_1:temp_pref.Preferences.pref_ocamldoc_paragraph_bgcolor_1
      ~ocamldoc_paragraph_bgcolor_2:temp_pref.Preferences.pref_ocamldoc_paragraph_bgcolor_2 ();
    Lexical.tag (preview#buffer :> GText.buffer);
    Preferences_apply.apply (preview :> Text.view) temp_pref;
end

(** pref_templ *)
and pref_templ title ?packing () =
  let vbox       = GPack.vbox ~spacing ?packing () in
  let align      = create_align ~title:"Custom Templates" ~vbox () in
  let box        = GPack.vbox ~spacing:1 ~packing:align#add () in
  let label_text = "Select an OCaml library containing custom templates" in
  let _          = GMisc.label ~xalign:0.0
    ~markup:(label_text ^ ":")
    ~packing:box#pack ()
  in
  let hbox       = GPack.hbox ~spacing:3 ~packing:box#pack () in
  let entry      = GEdit.entry ~editable:false ~packing:hbox#add () in
  let button     = GButton.button ~label:" ... " ~packing:hbox#pack () in
  let _          =
    ignore (button#connect#clicked ~callback:begin fun () ->
      let chooser    = GWindow.file_chooser_dialog ~action:`OPEN ~title:(label_text ^ " (.cma)")
        ~icon:Icons.oe ~position:`CENTER ~modal:true () in
      chooser#set_filter (GFile.filter ~patterns:["*.cma"] ());
      ignore (chooser#set_filename entry#text);
      chooser#add_button_stock `OK `OK;
      chooser#add_button_stock `CANCEL `CANCEL;
      match chooser#run () with
        | `OK ->
          Gaux.may chooser#filename ~f:(fun name -> entry#set_text name);
          chooser#destroy()
        | _ -> chooser#destroy()
    end)
  in
  let label      = GMisc.label ~markup:"<b>How to create custom templates</b>" () in
  let expander   = GBin.expander ~packing:vbox#add () in
  let _          = expander#set_label_widget label#coerce in
  let sw         = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:expander#add () in
  let markup     = Template.help in
  let label      = GMisc.label ~xalign:0.0 ~yalign:0.0 ~xpad:3 ~ypad:3 ~markup ~line_wrap:false ~selectable:true ~packing:sw#add_with_viewport () in
object
  inherit page title vbox
  method write pref =
    pref.Preferences.pref_editor_custom_templ_filename <- entry#text;
    Templ.load_custom `user;

  method read pref =
    ignore (entry#set_text pref.Preferences.pref_editor_custom_templ_filename);
end

(** create *)
let create = new preferences

