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


class widget ~project ?packing () =
  let targets         = project.Project_type.build in
  let hbox            = GPack.hbox ~spacing:5 ?packing () in
  let sw              = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add () in
  let cols            = new GTree.column_list in
  let col_target       = cols#add Gobject.Data.caml in
  let rend_target      = GTree.cell_renderer_text [] in
  let rend_pixbuf     = GTree.cell_renderer_pixbuf [] in
  let model           = GTree.list_store cols in
  let vc_target        = GTree.view_column ~title:"Target" () in
  let _               = vc_target#pack ~expand:false rend_pixbuf in
  let _               = vc_target#pack ~expand:true rend_target in
  let view            = GTree.view ~model ~headers_visible:false ~reorderable:true ~enable_search:false ~packing:sw#add () in
  let _               = view#selection#set_mode `MULTIPLE in
  let _               = view#append_column vc_target in
object (self)
  inherit GObj.widget hbox#as_widget
  initializer
    vc_target#set_cell_data_func rend_target begin fun model row ->
      let target = model#get ~row ~column:col_target in
      rend_target#set_properties [`TEXT target.Target.name];
      match target.Target.target_type with
        | Target.Executable -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.start_16; `XALIGN 0.0]
        | Target.Library -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.library; `XALIGN 0.0]
        | Target.Plugin -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.plugin; `XALIGN 0.0]
        | Target.Pack -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.library; `XALIGN 0.0]
    end;
    List.iter begin fun bc ->
      let row = model#append () in
      model#set ~row ~column:col_target bc;
    end targets;
end