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
  let bconfigs        = project.Project_type.build in
  let hbox            = GPack.hbox ~spacing:5 ?packing () in
  let sw              = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add () in
  let cols            = new GTree.column_list in
  let col_bconf       = cols#add Gobject.Data.caml in
  let rend_bconf      = GTree.cell_renderer_text [] in
  let rend_pixbuf     = GTree.cell_renderer_pixbuf [] in
  let model           = GTree.list_store cols in
  let vc_bconf        = GTree.view_column ~title:"Target" () in
  let _               = vc_bconf#pack ~expand:false rend_pixbuf in
  let _               = vc_bconf#pack ~expand:true rend_bconf in
  let view            = GTree.view ~model ~headers_visible:false ~reorderable:true ~enable_search:false ~packing:sw#add () in
  let _               = view#selection#set_mode `MULTIPLE in
  let _               = view#append_column vc_bconf in
object (self)
  inherit GObj.widget hbox#as_widget
  initializer
    vc_bconf#set_cell_data_func rend_bconf begin fun model row ->
      let bconf = model#get ~row ~column:col_bconf in
      rend_bconf#set_properties [`TEXT bconf.Target.name];
      match bconf.Target.outkind with
        | Target.Executable -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.start_16; `XALIGN 0.0]
        | Target.Library -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.library; `XALIGN 0.0]
        | Target.Plugin -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.plugin; `XALIGN 0.0]
        | Target.Pack -> rend_pixbuf#set_properties [`VISIBLE true; `PIXBUF Icons.library; `XALIGN 0.0]
    end;
    List.iter begin fun bc ->
      let row = model#append () in
      model#set ~row ~column:col_bconf bc;
    end bconfigs;
end