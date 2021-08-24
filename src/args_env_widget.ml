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
[@@@warning "-48"]

let create (vbox : GPack.box) =
  (* Command Line Arguments *)
  let expander_args = GBin.expander ~expanded:true ~packing:vbox#add ~show:true () in
  let label         = GMisc.label ~markup:"Command Line Arguments" ~xalign:0.0 () in
  (*let _ = GMisc.label ~markup:"<small>Use \" to quote and \\\" inside quoted arguments</small>" ~xalign:0.0 ~packing:abox#pack () in*)
  let entry_args    = Entry_list_args.create ~packing:expander_args#add () in
  (* Environment Variables *)
  let expander_env  = GBin.expander ~packing:vbox#add () in
  let lbox          = GPack.hbox ~spacing:8 () in
  let _             = GMisc.label ~markup:"Environment Variables" ~xalign:0.0 ~packing:lbox#add () in
  let help          = GMisc.label ~markup:"(<small><tt>NAME=VALUE</tt></small>)" ~xalign:1.0 ~packing:lbox#pack () in
  let entry_env     = Entry_list_env.create ~packing:expander_env#add () in
  expander_args#set_label_widget label#coerce;
  expander_env#set_label_widget lbox#coerce;
  let callback () =
    help#misc#hide();
    expander_env#set_expanded false;
    let args = expander_args#expanded in
    let env = expander_env#expanded in
    vbox#set_child_packing ~expand:args ~fill:args expander_args#coerce;
    vbox#set_child_packing ~expand:env ~fill:env expander_env#coerce;
  in
  ignore (expander_args#connect#after#activate ~callback);
  ignore (expander_env#connect#after#activate ~callback:begin fun () ->
      help#misc#show();
      expander_args#set_expanded false;
      let env = expander_env#expanded in
      let args = expander_args#expanded in
      vbox#set_child_packing ~expand:env ~fill:env expander_env#coerce;
      vbox#set_child_packing ~expand:args ~fill:args expander_args#coerce;
    end);
  callback();
  entry_args, entry_env

