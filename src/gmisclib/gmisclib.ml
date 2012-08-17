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


module Button =
  struct
    let button_menu = new Button_menu.button_menu
  end

module Entry =
  struct
    let combo_search = new Entry_combo_search.widget
  end

module Idle =
  struct
    let add     = Util.idle_add
    let add_gen = Util.idle_add_gen
  end

module Text =
  struct
    let hyperlink    = new Hyperlink.hyperlink
    let undo_manager = new Undo.manager
  end

module Toolbar =
  struct
    let menu_tool_button = new Menu_tool_button.menu_tool_button
  end

module Util =
  struct
    exception Mark_deleted
    let fade_window_enabled          = Gmisclib_util.fade_window_enabled
    let fade_window                  = Gmisclib_util.fade_window
    let set_tag_paragraph_background = Gmisclib_util.set_tag_paragraph_background
    let get_iter_at_mark_safe        = Gmisclib_util.get_iter_at_mark_safe
    let get_iter_at_mark_opt         = Gmisclib_util.get_iter_at_mark_opt
    let treeview_is_path_onscreen    = Gmisclib_util.treeview_is_path_onscreen
  end

module Window =
  struct
    let popup = new Window.popup
  end
