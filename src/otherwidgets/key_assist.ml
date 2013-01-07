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

let markup_right = "\
<big><b>Module Browser</b> and <b>Completion Window</b></big>

<tt><b>Right</b></tt>: Displays the type of the function under the cursor and its documentation, when available. Press <tt><b>Left</b></tt> to return to the preceding view.

<tt><b>F1</b>, <b>Alt+Return</b></tt>: Shows/hides documentation pane.

<tt><b>Alt+Left</b></tt>, <tt><b>Backspace</b></tt>: Goes back one step in the navigation history.

<tt><b>Alt+Right</b></tt>: Goes forward one step in the navigation history.

<tt><b>Alt+Up</b></tt>: Goes up one level in the module hierarchy.

<tt><b>Ctrl+E</b></tt>: Searches incrementally for text within the documentation page currently in view.

<tt><b>F3</b></tt>: Moves focus to the search entry (Module Browser only).

<tt><b>Ctrl+Shift+Return</b></tt>: Insert fully qualified name (Completion only).

<tt><b>Mouse-Right-Click</b></tt>: Displays the type definition of the type identifier under the pointer.";;

let markup_left = "\
<b><big>Editor</big></b>

<tt><b>Ctrl+Q</b></tt>: Selects the nearest comment block and adds comment or removes comment. If you press <tt>Ctrl+Q</tt> when a space character is selected, you get <tt>(* &lt;cursor&gt; *)</tt>; when two space characters are selected, you get <tt>(** &lt;cursor&gt; *)</tt>.

<tt><b>Ctrl+W</b></tt>: Selects a word; when pressed repeatedly cycles through the various parts (separated by '_' or '.') of an identifier.

<tt><b>Ctrl+Shift+D</b></tt>: Selects the text within a pair of matching delimiters; if pressed a second time, extends the selection to include the delimiters.

<tt><b>Ctrl+Shift+S</b></tt>: Searches the active buffer for all occurrences of the identifier under the cursor.


<b><big>Find/Replace Text Dialog</big></b>

<tt><b>Ctrl+Return</b></tt>: Find all.";;

open Printf

let window () =
  let win = GWindow.window ~icon:Icons.oe ~modal:true ~position:`CENTER
    ~show:false ~type_hint:`DIALOG ~title:"Key Assist" () in
  win#set_skip_taskbar_hint true;
  win#set_skip_pager_hint true;
  let vbox = GPack.vbox ~packing:win#add () in
  let box = GPack.vbox  ~border_width:13 ~spacing:21 ~packing:vbox#add () in
  (*let label = GMisc.label ~markup:"" ~xalign:0.0 ~yalign:0.0 ~packing:box#pack () in*)
  let hbox = GPack.hbox ~spacing:21 ~packing:box#pack () in
  let label = GMisc.label ~markup:markup_left ~xalign:0.0 ~yalign:0.0 ~line_wrap:true ~packing:hbox#pack () in
  let label = GMisc.label ~markup:markup_right ~xalign:0.0 ~yalign:0.0 ~line_wrap:true ~packing:hbox#pack () in
  let bbox = GPack.button_box `HORIZONTAL ~border_width:8 ~packing:vbox#pack () in
  let b_ok = GButton.button ~stock:`OK ~packing:bbox#add () in
  ignore (b_ok#connect#clicked ~callback:win#destroy);
  win#present();
;;






