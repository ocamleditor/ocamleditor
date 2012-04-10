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

let text = "
OCamlEditor is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

OCamlEditor is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"

open Printf

let window () =
  let win = GWindow.window ~icon:Icons.oe ~modal:true ~position:`CENTER ~border_width:1
    ~show:false ~type_hint:`DIALOG ~title:"License" () in
  let vbox = GPack.vbox ~packing:win#add () in
  let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GText.view ~width:550 ~height:200 ~cursor_visible:false ~editable:false ~packing:sw#add () in
  view#set_left_margin 13;
  view#misc#modify_font_by_name "monospace";
  let bbox = GPack.button_box `HORIZONTAL ~border_width:8 ~packing:(vbox#pack ~expand:false) () in
  let b_ok = GButton.button ~stock:`OK ~packing:bbox#add () in
  b_ok#connect#clicked ~callback:win#destroy;
  view#buffer#set_text text;
  win






