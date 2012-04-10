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


module Idle =
  struct
    let add     = Util.idle_add
    let add_gen = Util.idle_add_gen
  end

module Text =
  struct
    module Hyperlink = Hyperlink
(*    module Popup =
      struct
        let create = Util.window
      end*)
    module Undo = Undo
  end

module Toolbar =
  struct
    module Menu_tool_button = Menu_tool_button
  end

module Util =
  struct
    exception Mark_deleted
    let fade_window_enabled          = Util.fade_window_enabled
    let fade_window                  = Util.fade_window
    let set_tag_paragraph_background = Util.set_tag_paragraph_background
    let get_iter_at_mark_safe        = Util.get_iter_at_mark_safe
    let get_iter_at_mark_opt         = Util.get_iter_at_mark_opt
    let treeview_is_path_onscreen    = Util.treeview_is_path_onscreen
  end


