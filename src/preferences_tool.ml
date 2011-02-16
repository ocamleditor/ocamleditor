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


open Gdk
open Miscellanea
open Printf

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
  let indent = match title with None -> 0
    | Some title ->
      let label = GMisc.label ~markup:(sprintf "<b>%s</b>" title) ~xalign:0.0 ~packing:box#add () in
      indent
  in
  let align = GBin.alignment ~padding:(0, 0, indent, 0) ~packing:box#add () in
  align


let spacing = 13
let xalign = 0.0
let col_spacings = 8
let row_spacings = 5

(** page *)
class virtual page title (box : GPack.box) =
  let tbox = GPack.vbox ~spacing:3 ~packing:box#pack () in
  let label = GMisc.label ~markup:(sprintf "<big><b>%s</b></big>" title) ~xalign:0.0 ~packing:tbox#pack () in
  let sep = GMisc.separator `HORIZONTAL ~packing:tbox#pack () in
  let _ = box#reorder_child tbox#coerce ~pos:0 in
object (self)
  inherit GObj.widget box#as_widget
  method virtual write : Preferences.t -> unit
  method virtual read : Preferences.t -> unit
end


(** preferences *)
and preferences ~(editor : Editor.editor) () = 
  let window = GWindow.window ~allow_shrink:false ~allow_grow:false ~resizable:true ~width:660 
    ~type_hint:`DIALOG ~modal:true ~title:"Preferences" ~position:`CENTER ~icon:Icons.oe ~show:false () in 
  let vbox = GPack.vbox ~border_width:8 ~spacing:8 ~packing:window#add () in
  let hbox = GPack.hbox ~spacing:8 ~packing:vbox#add () in
  let cols = new GTree.column_list in
  let column  = cols#add Gobject.Data.string in
  let model = GTree.tree_store cols in
  let renderer = GTree.cell_renderer_text [] in
  let view_column = GTree.view_column ~title:"File" ~renderer:(renderer, ["text", column]) () in
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:(hbox#pack ~expand:false) () in
  let view = GTree.view ~model:model ~headers_visible:false ~reorderable:false ~width:140
    ~height:300 ~packing:sw#add () in
  let _ = view#append_column view_column in
  let _ = GMisc.separator `HORIZONTAL ~packing:vbox#pack () in
  let button_box = GPack.button_box `HORIZONTAL ~layout:`END ~border_width:5
    ~spacing:8 ~packing:(vbox#pack ~expand:false) () in
  let ok_button = GButton.button ~stock:`OK ~packing:button_box#add () in
  let cancel_button = GButton.button ~stock:`CANCEL ~packing:button_box#add () in
  let reset_button = GButton.button ~label:"Restore Defaults" ~packing:button_box#add () in
  let _ = button_box#set_child_secondary reset_button#coerce true in
object (self)
  inherit GObj.widget vbox#as_widget
  val mutable pages = []
  method show_page title =
    let page = List.assoc title pages in
    List.iter (fun (_, p) -> p#misc#hide()) pages;
    page#misc#show();
  method write () =
    List.iter (fun (_, page) -> page#write !Preferences.preferences) pages;
    editor#set_tab_pos !Preferences.preferences.Preferences.pref_tab_pos;
    editor#code_folding_enabled#set !Preferences.preferences.Preferences.pref_code_folding_enabled;
    editor#show_global_gutter#set !Preferences.preferences.Preferences.pref_show_global_gutter;
    ignore (editor#redisplay_views());
  method ok () =
    self#write();
    Preferences.save();
    window#destroy();
  method reset_defaults () =
    let defaults = Preferences.create_defaults() in
    List.iter (fun (_, page) -> page#read defaults) pages;
  method create title row  (page : string -> ?packing:(GObj.widget -> unit) -> unit -> page) =
    let page = page title ~packing:hbox#add () in
    model#set ~row ~column title;
    page#misc#hide();
    page#read !Preferences.preferences;
    pages <- (title, page) :: pages
  initializer
    (* Tree *)
    let row = model#append () in self#create "General" row (new pref_view);
    let row = model#append () in self#create "Fonts" row (new pref_fonts);
    let row as parent = model#append () in self#create "Editor" row (new pref_editor);
    let row = model#append ~parent () in self#create "Actions" row (new pref_editor_actions);
    let row = model#append ~parent () in self#create "Color" row (new pref_color);
    view#expand_all();
    view#selection#set_mode `SINGLE;
    view#selection#connect#changed ~callback:begin fun () ->
      try
        let path = List.hd view#selection#get_selected_rows in
        let title = model#get ~row:(model#get_iter path) ~column in
        self#show_page title
      with Failure "hd" -> ()
    end;
    (* Buttons *) 
    cancel_button#connect#clicked ~callback:window#destroy;
    ok_button#connect#clicked ~callback:self#ok;
    reset_button#connect#clicked ~callback:self#reset_defaults;
    (*  *)
    window#present();
    view#selection#select_path (GTree.Path.create [0])
end


(** pref_view *)
and pref_view title ?packing () =
  let vbox = GPack.vbox ~spacing ?packing () in
  let align = create_align ~title:"Tabs" ~vbox () in
  let table = GPack.table ~col_spacings ~row_spacings ~packing:align#add () in
  let _ = GMisc.label ~text:"Orientation:" ~xalign ~packing:(table#attach ~top:0 ~left:0 ~expand:`NONE) () in
  let _ = GMisc.label ~text:"Label type:" ~xalign ~packing:(table#attach ~top:1 ~left:0 ~expand:`NONE) () in
(*  let _ = GMisc.label ~text:"Insertions:" ~xalign ~packing:(table#attach ~top:2 ~left:0 ~expand:`NONE) () in*)
  let combo_orient, _ = GEdit.combo_box_text ~strings:[
      "Top"; "Right"; "Bottom"; "Left"; "Vertical on the left"; "Vertical on the right"
    ] ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let combo_labtype, _ = GEdit.combo_box_text ~strings:["Name"; "Shortname"]
    ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
(*  let combo_insert, _ = GEdit.combo_box_text ~strings:["Insert at end"; "Insert at beginning"; "Sort alphabetically"]
    ~packing:(table#attach ~top:2 ~left:1 ~expand:`X) () in*)
  (* Maximize View *)
  let align = create_align ~title:"Workspaces" ~vbox () in
  let box = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let table = GPack.table ~homogeneous:false ~col_spacings ~row_spacings ~packing:box#pack () in
  let top = ref 0 in
  let width = 65 in
  let none_action_label = GMisc.label ~text:"" ~packing:(table#attach ~top:0 ~left:0) () in
  let label_menubar = GMisc.label ~width ~text:"Show\nMenubar" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:1) () in
  let label_toolbar = GMisc.label ~width ~text:"Show\nToolbar" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:2) () in
  let label_tabbar = GMisc.label ~width ~text:"Show\nTabs" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:3) () in
  let label_messages = GMisc.label ~width ~text:"Keep\nMessages" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:4) () in
  let label_fullscreen = GMisc.label ~width ~text:"Full-Screen" ~justify:`CENTER ~xalign:0.5 ~packing:(table#attach ~top:!top ~left:6) () in
  let _ = incr top in
  let fst_action_label = GMisc.label ~text:"Workspace 1:" ~xalign:0.0 ~packing:(table#attach ~top:!top ~left:0) () in
  let check_menubar_1 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:1) () in
  let check_toolbar_1 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:2) () in
  let check_tabbar_1 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:3) () in
  let check_messages_1 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:4) () in
  let check_fullscreen_1 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:6) () in
  let _ = incr top in
(*  let snd_action_check = GButton.check_button ~label:"Second level:" ~packing:(table#attach ~top:!top ~left:0) () in*)
  let snd_action_label = GMisc.label ~text:"Workspace 2:" ~xalign:0.0 ~packing:(table#attach ~top:!top ~left:0) () in
  let check_menubar_2 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:1) () in
  let check_toolbar_2 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:2) () in
  let check_tabbar_2 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:3) () in
  let check_messages_2 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:4) () in
  let check_fullscreen_2 = GButton.check_button ~packing:(table#attach ~fill:`NONE ~top:!top ~left:6) () in
  let use_maximize = GButton.check_button ~label:"Use maximized window instead of full-screen" ~packing:box#pack () in
(*  let _ = snd_action_check#connect#toggled ~callback:begin fun () ->
    check_menubar_2#misc#set_sensitive snd_action_check#active;
    check_toolbar_2#misc#set_sensitive snd_action_check#active;
    check_tabbar_2#misc#set_sensitive snd_action_check#active;
    check_messages_2#misc#set_sensitive snd_action_check#active;
    check_fullscreen_2#misc#set_sensitive snd_action_check#active;
  end in
  let _ = snd_action_check#set_active true in
  let _ = snd_action_check#set_active false in*)
  (* Software Update *)
  let align = create_align ~title:"Software Update" ~vbox () in
  let check_software_update = GButton.check_button ~label:"Automatically check for updates" ~packing:align#add () in
object (self)
  inherit page title vbox

  method write pref = 
    pref.Preferences.pref_tab_pos <- (match combo_orient#active
      with 0 -> `TOP | 1 | 5 -> `RIGHT | 2 -> `BOTTOM | 3 | 4 -> `LEFT | _ -> assert false);
    pref.Preferences.pref_tab_vertical_text <- (match combo_orient#active
      with 0 | 1 | 2 | 3 -> false | 4 | 5 -> true | _ -> assert false);
    pref.Preferences.pref_tab_label_type <- combo_labtype#active;
    pref.Preferences.pref_check_updates <- check_software_update#active;
    pref.Preferences.pref_max_view_1_menubar <- check_menubar_1#active;
    pref.Preferences.pref_max_view_1_toolbar <- check_toolbar_1#active;
    pref.Preferences.pref_max_view_1_tabbar <- check_tabbar_1#active;
    pref.Preferences.pref_max_view_1_messages <- check_messages_1#active;
    pref.Preferences.pref_max_view_1_fullscreen <- check_fullscreen_1#active;
    pref.Preferences.pref_max_view_2_menubar <- check_menubar_2#active;
    pref.Preferences.pref_max_view_2_toolbar <- check_toolbar_2#active;
    pref.Preferences.pref_max_view_2_tabbar <- check_tabbar_2#active;
    pref.Preferences.pref_max_view_2_messages <- check_messages_2#active;
    pref.Preferences.pref_max_view_2_fullscreen <- check_fullscreen_2#active;
    pref.Preferences.pref_max_view_2 <- true (*snd_action_check#active*);
    pref.Preferences.pref_max_view_fullscreen <- not use_maximize#active;

  method read pref =
    combo_orient#set_active (match pref.Preferences.pref_tab_pos, pref.Preferences.pref_tab_vertical_text with
      | `TOP, _ -> 0 | `RIGHT, false -> 1 | `BOTTOM, _ -> 2 | `LEFT, false -> 3
      | `LEFT, true -> 4 | `RIGHT, true -> 5);
    combo_labtype#set_active pref.Preferences.pref_tab_label_type;
    check_software_update#set_active pref.Preferences.pref_check_updates;
    check_menubar_1#set_active pref.Preferences.pref_max_view_1_menubar;
    check_toolbar_1#set_active pref.Preferences.pref_max_view_1_toolbar;
    check_tabbar_1#set_active pref.Preferences.pref_max_view_1_tabbar;
    check_messages_1#set_active pref.Preferences.pref_max_view_1_messages;
    check_fullscreen_1#set_active pref.Preferences.pref_max_view_1_fullscreen;
    check_menubar_2#set_active pref.Preferences.pref_max_view_2_menubar;
    check_toolbar_2#set_active pref.Preferences.pref_max_view_2_toolbar;
    check_tabbar_2#set_active pref.Preferences.pref_max_view_2_tabbar;
    check_messages_2#set_active pref.Preferences.pref_max_view_2_messages;
    check_fullscreen_2#set_active pref.Preferences.pref_max_view_2_fullscreen;
    (*snd_action_check#set_active pref.Preferences.pref_max_view_2;*)
    use_maximize#set_active (not pref.Preferences.pref_max_view_fullscreen);
end

and pref_editor_actions title ?packing () =
  let vbox = GPack.vbox ~spacing ?packing () in
  (* Smart Keys *)
  let align = create_align ~title:"Smart Keys" ~vbox () in
  let skbox = GPack.table ~row_spacings:row_spacings ~col_spacings:col_spacings ~packing:align#add () in
  let _ = GMisc.label ~text:"Home: " ~xalign:0.0 ~packing:(skbox#attach ~top:0 ~left:0) () in
  let combo_home, _ = GEdit.combo_box_text ~active:0 ~strings:
    ["Move to beginning of first word"; "Move to first column position"]
    ~packing:(skbox#attach ~top:0 ~left:1 ~expand:`X) () in
  let _ = GMisc.label ~text:"End: " ~xalign:0.0 ~packing:(skbox#attach ~top:1 ~left:0) () in
  let combo_end, _ = GEdit.combo_box_text ~active:0 ~strings:
    ["Move to last column position (includes whitespace)"; "Move to end of last character"]
    ~packing:(skbox#attach ~top:1 ~left:1 ~expand:`X) () in
(*  (* Type Annotation *)
  let align = create_align ~title:"Type Annotations" ~vbox () in
  let box = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let table_annot_type = GPack.table ~row_spacings:row_spacings ~col_spacings:col_spacings ~packing:box#add () in
  let check_annot_type_enabled = GButton.check_button ~label:"Enable tooltips"
    ~packing:(table_annot_type#attach ~top:0 ~left:0) () in
  let combo_annot_type_tooltips_delay, _ = GEdit.combo_box_text ~strings:
    ["Show immediately"; "Show after delay"]
    ~packing:(table_annot_type#attach ~top:0 ~left:1 ~expand:`X) () in
(*  let _ = GMisc.label ~show:false ~text:"Implementation: " ~xalign:0.0 ~packing:(table_annot_type#attach ~top:1 ~left:0) () in
  let combo_annot_type_tooltips_impl, _ = GEdit.combo_box_text ~show:false ~strings:
    ["Use popups"; "Use GTK tooltips"]
    ~packing:(table_annot_type#attach ~top:1 ~left:1 ~expand:`X) () in*)
  let _ = check_annot_type_enabled#connect#toggled ~callback:begin fun () ->
    combo_annot_type_tooltips_delay#misc#set_sensitive check_annot_type_enabled#active;
  end in*)
  (* File Saving *)
  let align = create_align ~title:"File Saving" ~vbox () in
  let box = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let check_bak = GButton.check_button ~label:"Create a backup copy of files before saving" ~packing:box#pack () in
  let check_trim = GButton.check_button ~label:"Strip trailing whitespace" ~packing:box#pack () in
  (* Searching *)
  let align = create_align ~title:"Searching" ~vbox () in
  let box = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let check_search_word_at_cursor = GButton.check_button ~label:"Search word at cursor" ~packing:box#pack () in
object (self)
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

  method read pref = 
    check_bak#set_active pref.Preferences.pref_editor_bak;
    check_trim#set_active pref.Preferences.pref_editor_trim_lines;
    combo_home#set_active pref.Preferences.pref_smart_keys_home;
    combo_end#set_active pref.Preferences.pref_smart_keys_end;
(*    check_annot_type_enabled#set_active pref.Preferences.pref_annot_type_tooltips_enabled;
    combo_annot_type_tooltips_delay#set_active pref.Preferences.pref_annot_type_tooltips_delay;
    (*combo_annot_type_tooltips_impl#set_active pref.Preferences.pref_annot_type_tooltips_impl;*)*)
    check_search_word_at_cursor#set_active pref.Preferences.pref_search_word_at_cursor;
end

(** pref_fonts *)
and pref_fonts title ?packing () = 
  let vbox = GPack.vbox ~spacing ?packing () in
  let notebook = GPack.notebook ~packing:vbox#add () in
  let border_width = 5 in
  let preview_text = "abcdefghijk ABCDEFGHIJK òàùèéì" in
  let font_editor = GMisc.font_selection ~preview_text ~border_width () in
  let box_compl = GPack.vbox ~border_width ~spacing:8 () in
  let font_compl = GMisc.font_selection ~preview_text ~packing:box_compl#add () in
  let button_greek = GButton.check_button ~label:"Use greek letters in types" ~packing:box_compl#pack () in
  let font_other = GMisc.font_selection ~preview_text ~border_width () in
  let _ = notebook#append_page ~tab_label:(GMisc.label ~text:"Editor" ())#coerce font_editor#coerce in
  let _ = notebook#append_page ~tab_label:(GMisc.label ~text:"Completion" ())#coerce box_compl#coerce in
  let _ = notebook#append_page ~tab_label:(GMisc.label ~text:"Other" ())#coerce font_other#coerce in

object (self)
  inherit page title vbox

  method write pref = 
    pref.Preferences.pref_base_font <- font_editor#font_name;
    pref.Preferences.pref_compl_font <- font_compl#font_name;
    pref.Preferences.pref_output_font <- font_other#font_name;
    pref.Preferences.pref_compl_greek <- button_greek#active;

  method read pref =
    font_editor#set_font_name pref.Preferences.pref_base_font;
    font_compl#set_font_name pref.Preferences.pref_compl_font;
    font_other#set_font_name pref.Preferences.pref_output_font;
    button_greek#set_active pref.Preferences.pref_compl_greek;
end


(** pref_editor *)
and pref_editor title ?packing () =
  let vbox = GPack.vbox ~spacing ?packing () in
  (*  *)
  let align = create_align ~title:"Tab Settings/Word Wrap" ~vbox () in
  let box = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let tbox = GPack.hbox ~spacing:8 ~packing:box#pack () in
  let _ = GMisc.label ~text:"Tab width:" ~packing:tbox#pack () in
  let adjustment = GData.adjustment ~page_size:0.0 () in
  let entry_tab_width = GEdit.spin_button ~adjustment ~rate:1.0 ~digits:0 ~numeric:true ~packing:tbox#add () in
  let check_tab_spaces = GButton.check_button ~label:"Use spaces instead of tabs" ~packing:tbox#add () in
  let _ = check_tab_spaces#misc#set_sensitive false in
  let check_wrap = GButton.check_button ~label:"Wrap text, breaking lines between words" ~packing:box#pack () in
  (** Display *)
  let align = create_align ~title:"Display" ~vbox () in
  let box = GPack.table ~row_spacings ~col_spacings ~packing:align#add () in
  let check_show_line_numbers = GButton.check_button ~label:"Show line numbers" ~packing:(box#attach ~top:0 ~left:0) () in
  let check_highlight_current_line = GButton.check_button ~label:"Highlight current line" ~packing:(box#attach ~top:0 ~left:1) () in
  let check_indent_lines = GButton.check_button ~label:"Show indentation guidelines" ~packing:(box#attach ~top:1 ~left:0) () in
  let check_code_folding = GButton.check_button ~label:"Enable code folding" ~packing:(box#attach ~top:1 ~left:1) () in
  let check_global_gutter = GButton.check_button ~label:"Show global gutter" ~packing:(box#attach ~top:2 ~left:0) () in
  let hbox = GPack.hbox ~spacing:5 ~packing:(box#attach ~top:2 ~left:1) () in
  let adjustment = GData.adjustment ~lower:0. ~upper:300. ~step_incr:1. ~page_size:0. () in
  let check_right_margin = GButton.check_button ~active:false
    ~label:"Visible right margin at column:" ~packing:hbox#pack () in
  let entry_right_margin = GEdit.spin_button
    ~numeric:true ~digits:0 ~rate:1.0 ~adjustment ~packing:hbox#pack () in
  (** Error indication *)
  let align = create_align ~title:"Error indication" ~vbox () in
  let box = GPack.table ~row_spacings ~col_spacings ~packing:align#add () in
  let check_error_underline = GButton.check_button ~label:"Underline errors" ~packing:(box#attach ~top:0 ~left:0) () in
  let check_error_gutter = GButton.check_button ~label:"Show in gutter" ~packing:(box#attach ~top:0 ~left:1) () in
  let check_error_tooltip = GButton.check_button ~label:"Show tooltip" ~packing:(box#attach ~top:1 ~left:0) () in
object (self)
  inherit page title vbox
  initializer
    ignore (check_right_margin#connect#toggled ~callback:begin fun () ->
      entry_right_margin#misc#set_sensitive check_right_margin#active;
    end);

  method write pref = 
    pref.Preferences.pref_editor_tab_width <- entry_tab_width#value_as_int;
    pref.Preferences.pref_editor_tab_spaces <- check_tab_spaces#active;
    pref.Preferences.pref_editor_wrap <- check_wrap#active;
    pref.Preferences.pref_highlight_current_line <- check_highlight_current_line#active;
    pref.Preferences.pref_show_line_numbers <- check_show_line_numbers#active;
    pref.Preferences.pref_indent_lines <- check_indent_lines#active;
    pref.Preferences.pref_right_margin_visible <- check_right_margin#active;
    pref.Preferences.pref_right_margin <- entry_right_margin#value_as_int;
    pref.Preferences.pref_code_folding_enabled <- check_code_folding#active;
    pref.Preferences.pref_show_global_gutter <- check_global_gutter#active;
    pref.Preferences.pref_err_underline <- check_error_underline#active;
    pref.Preferences.pref_err_tooltip <- check_error_tooltip#active;
    pref.Preferences.pref_err_gutter <- check_error_gutter#active;

  method read pref = 
    entry_tab_width#set_value (float pref.Preferences.pref_editor_tab_width);
    check_tab_spaces#set_active pref.Preferences.pref_editor_tab_spaces;
    check_wrap#set_active pref.Preferences.pref_editor_wrap;
    check_highlight_current_line#set_active pref.Preferences.pref_highlight_current_line;
    check_show_line_numbers#set_active pref.Preferences.pref_show_line_numbers;
    check_indent_lines#set_active pref.Preferences.pref_indent_lines;
    check_right_margin#set_active (not pref.Preferences.pref_right_margin_visible);
    check_right_margin#set_active pref.Preferences.pref_right_margin_visible;
    entry_right_margin#set_value (float pref.Preferences.pref_right_margin);
    check_code_folding#set_active (pref.Preferences.pref_code_folding_enabled);
    check_global_gutter#set_active (pref.Preferences.pref_show_global_gutter);
    check_error_underline#set_active (pref.Preferences.pref_err_underline);
    check_error_tooltip#set_active (pref.Preferences.pref_err_tooltip);
    check_error_gutter#set_active (pref.Preferences.pref_err_gutter);
end

(** pref_color *)
and pref_color title ?packing () = 
  let vbox = GPack.vbox ~spacing ?packing () in
  let notebook = GPack.notebook ~packing:vbox#add () in
  let border_width = 5 in
  (* tags *)
  let cols = new GTree.column_list in
  let tag_col = cols#add Gobject.Data.string in
  let lab_col = cols#add Gobject.Data.string in
  let tag_model = GTree.list_store cols in
  (* Syntax coloring *)
  let color_ocaml = GPack.vbox ~border_width ~spacing:8 () in
  let _ = notebook#append_page ~tab_label:(GMisc.label ~text:"OCaml" ())#coerce color_ocaml#coerce in
  let hbox = GPack.hbox ~spacing:8 ~packing:color_ocaml#pack () in
  let _ = GMisc.label ~text:"Background color:" ~xalign ~packing:hbox#pack () in
  let bg_button = GButton.color_button ~packing:hbox#pack () in
  let _ = bg_button#set_relief `NONE in
(*  let check_bg_theme = GButton.check_button ~label:"From theme (requires restart)" ~packing:hbox#add () in
  let _ =
      check_bg_theme#connect#toggled ~callback:begin fun () ->
      bg_button#misc#set_sensitive (not check_bg_theme#active)
    end
  in*)
  let tag_box = GPack.hbox ~border_width:0 ~spacing:8 ~packing:color_ocaml#pack () in
  let sw = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:tag_box#pack () in
  let tag_view = GTree.view ~width:200 ~height:150 ~headers_visible:false ~model:tag_model ~packing:sw#add () in
  let renderer = GTree.cell_renderer_text [] in
  let tag_vc = GTree.view_column ~renderer:(renderer, ["text", tag_col]) () in
  let _ = tag_view#append_column tag_vc in
  let lab_vc = GTree.view_column ~title:"Text Elements" ~renderer:(renderer, ["text", lab_col]) () in
  let _ = tag_view#append_column lab_vc in
  let _ = tag_vc#set_visible false in
  let prop_box = GPack.vbox ~border_width:0 ~packing:(tag_box#pack ~expand:false) () in
  let fg_box = GPack.hbox ~spacing:5 ~packing:(prop_box#pack ~expand:false) () in
  let fg_lab = GMisc.label ~text:"Color:" ~packing:(fg_box#pack ~expand:false) () in
  let fg_button = GButton.color_button ~packing:(fg_box#pack ~expand:false) () in
  let _ = fg_button#set_relief `NONE in
  let weight_check = GButton.check_button ~label:"Bold" ~packing:(prop_box#pack ~expand:false) () in
  let style_check = GButton.check_button ~label:"Italic" ~packing:(prop_box#pack ~expand:false) () in
  let underline_check = GButton.check_button ~label:"Underline" ~packing:(prop_box#pack ~expand:false) () in
  let _ = weight_check#set_image (GMisc.image ~stock:`BOLD ())#coerce in
  let _ = style_check#set_image (GMisc.image ~stock:`ITALIC ())#coerce in
  let _ = underline_check#set_image (GMisc.image ~stock:`UNDERLINE ())#coerce in
  let opb_box = GPack.hbox ~spacing:5 ~packing:(prop_box#pack ~expand:false) ~show:false () in
  let opb_lab = GMisc.label ~xalign:0.0
    ~markup:"Paragraph background colors:" (* \n<small>(only applies to comments preceded\nby a blank line)</small> *)
    ~packing:opb_box#pack () in
  let opb_button = GButton.color_button ~packing:(opb_box#pack ~fill:false) () in
  let _ = opb_button#set_relief `NONE in
  let opb2_button = GButton.color_button ~packing:(opb_box#pack ~fill:false) () in
  let _ = opb2_button#set_relief `NONE in
  (* OCaml Preview *)
  let osw = GBin.scrolled_window ~height:200 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN () in
  let buffer = new Ocaml_text.buffer ~lexical_enabled:true () in
  let _ = buffer#set_text ocaml_preview in
  let preview = new Ocaml_text.view ~buffer () in
  let _ = Gtk_util.idle_add (fun () -> preview#buffer#place_cursor ~where:(preview#buffer#start_iter#forward_lines 5)) in
  let _ = Preferences_apply.apply (preview :> Text.view) !Preferences.preferences in
  let _ = preview#set_editable false in
  let _ = osw#add preview#coerce in
  let _ = color_ocaml#add osw#coerce in
  (* color_compl *)
  let color_compl = GPack.vbox ~border_width () in
  let table = GPack.table ~row_spacings ~col_spacings ~packing:color_compl#pack () in
  let _ = GMisc.label ~text:"Background color:" ~xalign ~packing:(table#attach ~top:0 ~left:0) () in
  let _ = GMisc.label ~text:"Foreground color:" ~xalign ~packing:(table#attach ~top:1 ~left:0) () in
  let bg_button_popup = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let fg_button_popup = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let _ = notebook#append_page ~tab_label:(GMisc.label ~text:"Completion" ())#coerce color_compl#coerce in
  (* color_other *)
  let color_other = GPack.vbox ~border_width () in
  let table = GPack.table ~row_spacings ~col_spacings ~packing:color_other#pack () in
  let button_bg = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let button_fg_stdin = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let button_fg_stdout = GButton.color_button ~packing:(table#attach ~top:2 ~left:1 ~expand:`X) () in
  let button_fg_err = GButton.color_button ~packing:(table#attach ~top:3 ~left:1 ~expand:`X) () in
  let button_fg_warn = GButton.color_button ~packing:(table#attach ~top:4 ~left:1 ~expand:`X) () in
  let _ = GMisc.label ~xalign ~text:"Background color:" ~packing:(table#attach ~top:0 ~left:0) () in
  let _ = GMisc.label ~xalign ~text:"Standard input:" ~packing:(table#attach ~top:1 ~left:0) () in
  let _ = GMisc.label ~xalign ~text:"Standard output:" ~packing:(table#attach ~top:2 ~left:0) () in
  let _ = GMisc.label ~xalign ~text:"Errors:" ~packing:(table#attach ~top:3 ~left:0) () in
  let _ = GMisc.label ~xalign ~text:"Warnings:" ~packing:(table#attach ~top:4 ~left:0) () in
  (* *)
  let _ = notebook#append_page ~tab_label:(GMisc.label ~text:"Message Pane" ())#coerce color_other#coerce in
object (self)
  inherit page title vbox
  val mutable tags = []
  val mutable current_tag = ""
  initializer
    ignore (tag_view#selection#connect#changed ~callback:begin fun () ->
      if List.length tag_view#selection#get_selected_rows > 0 then begin
        let path = List.hd tag_view#selection#get_selected_rows in
        let tname = tag_model#get ~row:(tag_model#get_iter path) ~column:tag_col in
        current_tag <- tname;
        match List.assoc tname tags with color, weight, style, underline ->
          fg_button#set_color (GDraw.color color);
          weight_check#set_active (weight <> `NORMAL);
          style_check#set_active (style <> `NORMAL);
          underline_check#set_active (underline <> `NONE);
        if Oe_config.ocamldoc_paragraph_bgcolor_enabled && tname = "ocamldoc"
        then (opb_box#misc#show()) else (opb_box#misc#hide())
      end
    end);
    List.iter begin fun b ->
      ignore(b#connect#after#clicked
        ~callback:(fun () -> self#set_tag (fg_button, weight_check, style_check, underline_check)))
    end [weight_check; style_check; underline_check];
    ignore (fg_button#connect#color_set ~callback:begin fun () ->
      self#set_tag (fg_button, weight_check, style_check, underline_check);
    end);
    ignore (bg_button#connect#color_set ~callback:begin fun () ->
      self#set_tag (fg_button, weight_check, style_check, underline_check);
    end);
    ignore (opb_button#connect#color_set ~callback:begin fun () ->
      self#set_tag (fg_button, weight_check, style_check, underline_check);
    end);
    ignore (opb2_button#connect#color_set ~callback:begin fun () ->
      self#set_tag (fg_button, weight_check, style_check, underline_check);
    end);
(*    ignore (check_bg_theme#connect#toggled ~callback:begin fun () ->
      self#set_tag (fg_button, weight_check, style_check, underline_check);
    end);*)

  method private set_tag (fgb, wc, sc, uc) =
    let color = fgb#color in
    let weight = if wc#active then `BOLD else `NORMAL in
    let style = if sc#active then `ITALIC else `NORMAL in
    let underline = if uc#active then `SINGLE else `NONE in
    if current_tag <> "" then begin
      tags <- (current_tag, (`NAME (color_name color), weight, style, underline)) :: (List.remove_assoc current_tag tags);
    end;
    let temp_pref = {!Preferences.preferences with
      Preferences.pref_bg_color = (color_name bg_button#color, false (*check_bg_theme#active*));
      Preferences.pref_tags = tags;
      Preferences.pref_ocamldoc_paragraph_bgcolor_1 = Some (color_name opb_button#color);
      Preferences.pref_ocamldoc_paragraph_bgcolor_2 = Some (color_name opb2_button#color);
    } in
    let tag_names, colors = List.split tags in
    Lexical.init_tags ~tags:tag_names ~colors
      ~ocamldoc_paragraph_bgcolor_1:temp_pref.Preferences.pref_ocamldoc_paragraph_bgcolor_1
      ~ocamldoc_paragraph_bgcolor_2:temp_pref.Preferences.pref_ocamldoc_paragraph_bgcolor_2
      (preview#buffer :> GText.buffer);
    Lexical.tag (preview#buffer :> GText.buffer);
    Preferences_apply.apply (preview :> Text.view) temp_pref;

  method write pref = 
    pref.Preferences.pref_bg_color_popup <- color_name bg_button_popup#color;
    pref.Preferences.pref_fg_color_popup <- color_name fg_button_popup#color;
    pref.Preferences.pref_bg_color <- (color_name bg_button#color, false (*check_bg_theme#active*));
    pref.Preferences.pref_tags <- tags;
    let ltags, prop = List.split tags in
    Lexical.tags := ltags;
    Lexical.colors := prop;
    pref.Preferences.pref_output_bg <- color_name button_bg#color;
    pref.Preferences.pref_output_fg_stdin <- color_name button_fg_stdin#color;
    pref.Preferences.pref_output_fg_stdout <- color_name button_fg_stdout#color;
    pref.Preferences.pref_output_fg_err <- color_name button_fg_err#color;
    pref.Preferences.pref_output_fg_warn <- color_name button_fg_warn#color;
    pref.Preferences.pref_ocamldoc_paragraph_bgcolor_1 <- Some (color_name opb_button#color);
    pref.Preferences.pref_ocamldoc_paragraph_bgcolor_2 <- Some (color_name opb2_button#color);

  method read pref = 
    bg_button_popup#set_color (GDraw.color (`NAME pref.Preferences.pref_bg_color_popup));
    fg_button_popup#set_color (GDraw.color (`NAME pref.Preferences.pref_fg_color_popup));
    tags <- List.sort Pervasives.compare pref.Preferences.pref_tags;
    bg_button#set_color (GDraw.color (`NAME (fst pref.Preferences.pref_bg_color)));
(*    check_bg_theme#set_active (snd pref.Preferences.pref_bg_color);*)
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
    tag_view#selection#select_path (GTree.Path.create [0]);
    button_bg#set_color (GDraw.color (`NAME pref.Preferences.pref_output_bg));
    button_fg_stdin#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_stdin));
    button_fg_stdout#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_stdout));
    button_fg_err#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_err));
    button_fg_warn#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_warn));
    Gaux.may pref.Preferences.pref_ocamldoc_paragraph_bgcolor_1 ~f:(fun color -> opb_button#set_color (GDraw.color (`NAME color)));
    Gaux.may pref.Preferences.pref_ocamldoc_paragraph_bgcolor_2 ~f:(fun color -> opb2_button#set_color (GDraw.color (`NAME color)));
    self#set_tag (fg_button, weight_check, style_check, underline_check);
end

let create ~editor () =
  let pref = new preferences ~editor () in
  pref

