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


open Pref_page
open Miscellanea

(** pref_view *)
class pref_view title ?packing () =
  let vbox                  = GPack.vbox ~spacing ?packing () in
  let has_themes            = Oe_config.themes_dir <> None in
  let align                 = create_align ~vbox () in
  let box                   = GPack.vbox ~spacing:row_spacings ~packing:align#add () in
  let table                 = GPack.table ~col_spacings ~row_spacings ~packing:box#add () in
  let _                     = GMisc.label ~text:"Theme:" ~xalign ~packing:(table#attach ~top:0 ~left:0 ~expand:`NONE) ~show:has_themes () in
  let combo_theme, _        = GEdit.combo_box_text ~strings:Gtk_theme.avail_themes ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) ~show:has_themes () in
  let check_splash          = GButton.check_button ~label:"Display splash screen" ~packing:box#pack () in
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
  let align                 = create_align ~title:"Workspace" ~vbox () in
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
  (*let check_remember        = GButton.check_button ~label:"Remember windows position and size" ~packing:box#pack () in*)
  let check_detach_sep      = GButton.check_button ~label:"Detach message panes separately" ~packing:box#pack () in
  (*let check_geometry_delayed    = GButton.check_button ~label:"Update delayed" ~packing:box#pack ~show:false () in*)
  object (self)
    inherit page title vbox

    initializer
      (*let callback () =
        check_detach_sep#misc#set_sensitive check_remember#active;
        check_geometry_delayed#misc#set_sensitive check_remember#active
        in
        ignore (check_remember#connect#toggled ~callback);
        callback();*)
      match Oe_config.themes_dir with
      | Some _ ->
          ignore (combo_theme#connect#changed ~callback:begin fun () ->
              let theme = self#get_theme_name() in
              Gtk_theme.set_theme ?theme ~context:self#misc#pango_context ();
              let button = GButton.button ~show:false ~packing:vbox#add () in
              let avg_text_normal = button#misc#style#text `NORMAL |> Color.rgb_of_gdk |> Color.avg in
              let avg_bg_normal = button#misc#style#bg `NORMAL |> Color.rgb_of_gdk |> Color.avg in
              button#destroy();
              Oe_config.is_dark_theme := avg_text_normal > avg_bg_normal;
              let tags =
                if !Oe_config.is_dark_theme then Preferences.(preferences#get.editor_tags_dark)
                else Preferences.preferences#get.editor_tags in
              let ltags, prop = tags |> List.map (fun t -> t.Settings_t.name, t) |> List.split in
              Lexical.tags := ltags;
              Lexical.colors := prop;
              Preferences.save();
            end);
      | _ -> ()

    method private get_theme_name () =
      try Some (List.nth Gtk_theme.avail_themes combo_theme#active) with Invalid_argument _ -> None

    method write pref =
      Option.iter
        (fun _ -> pref.theme <- self#get_theme_name())
        Oe_config.themes_dir;
      pref.splashscreen_enabled <- check_splash#active;
      pref.tab_pos <- (match combo_orient#active
                       with 0 -> `TOP | 1 | 5 -> `RIGHT | 2 -> `BOTTOM | 3 | 4 -> `LEFT | _ -> assert false);
      pref.tab_vertical_text <- (match combo_orient#active
                                 with 0 | 1 | 2 | 3 -> false | 4 | 5 -> true | _ -> assert false);
      pref.tab_label_type <- combo_labtype#active;
      (*pref.Preferences.pref_max_view_1_menubar <- check_menubar_1#active;*)
      pref.max_view_1_toolbar <- check_toolbar_1#active;
      pref.max_view_1_tabbar <- check_tabbar_1#active;
      pref.max_view_1_messages <- check_messages_1#active;
      pref.max_view_1_fullscreen <- check_fullscreen_1#active;
      (*pref.Preferences.pref_max_view_2_menubar <- check_menubar_2#active;*)
      pref.max_view_2_toolbar <- check_toolbar_2#active;
      pref.max_view_2_tabbar <- check_tabbar_2#active;
      pref.max_view_2_messages <- check_messages_2#active;
      pref.max_view_2_fullscreen <- check_fullscreen_2#active;
      (*pref.max_view <- true (*snd_action_check#active*);*)
      pref.max_view_prefer_fullscreen <- not use_maximize#active;
      (* pref.Preferences.pref_remember_window_geometry <- check_remember#active;
         Gmisclib.Window.GeometryMemo.set_enabled Preferences.geometry_memo pref.Preferences.pref_remember_window_geometry;*)
      pref.detach_message_panes_separately <- check_detach_sep#active;
      (*pref.Preferences.pref_geometry_delayed <- check_geometry_delayed#active;
        Gmisclib.Window.GeometryMemo.set_delayed Preferences.geometry_memo pref.Preferences.pref_geometry_delayed;*)

    method read pref =
      Option.iter
        (fun _ -> combo_theme#set_active (
             match pref.theme with
             | Some name -> (try Xlist.pos name Gtk_theme.avail_themes with Not_found -> -1)
             | _ -> -1))
        Oe_config.themes_dir;
      check_splash#set_active pref.splashscreen_enabled;
      combo_orient#set_active (match pref.tab_pos, pref.tab_vertical_text with
          | `TOP, _ -> 0 | `RIGHT, false -> 1 | `BOTTOM, _ -> 2 | `LEFT, false -> 3
          | `LEFT, true -> 4 | `RIGHT, true -> 5);
      combo_labtype#set_active pref.tab_label_type;
      (*check_menubar_1#set_active pref.Preferences.pref_max_view_1_menubar;*)
      check_toolbar_1#set_active pref.max_view_1_toolbar;
      check_tabbar_1#set_active pref.max_view_1_tabbar;
      check_messages_1#set_active pref.max_view_1_messages;
      check_fullscreen_1#set_active pref.max_view_1_fullscreen;
      (*check_menubar_2#set_active pref.Preferences.pref_max_view_2_menubar;*)
      check_toolbar_2#set_active pref.max_view_2_toolbar;
      check_tabbar_2#set_active pref.max_view_2_tabbar;
      check_messages_2#set_active pref.max_view_2_messages;
      check_fullscreen_2#set_active pref.max_view_2_fullscreen;
      (*snd_action_check#set_active pref.Preferences.pref_max_view_2;*)
      use_maximize#set_active (not pref.max_view_prefer_fullscreen);
      (*check_remember#set_active pref.Preferences.pref_remember_window_geometry;*)
      check_detach_sep#set_active pref.detach_message_panes_separately;
      (*check_geometry_delayed#set_active pref.Preferences.pref_geometry_delayed;*)
  end

