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

open Printf

class widget ~editor ?packing () =
  let vbox            = GPack.vbox ~border_width:3 ?packing () in
  let hbox            = GPack.paned `HORIZONTAL ~packing:vbox#add () in
  let sw              = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add1 () in
  let swr             = GBin.scrolled_window ~width:640 ~height:400 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:hbox#add2 () in
  (*let frame           = GBin.frame ~packing:hbox#add () in*)
  let cols            = new GTree.column_list in
  let col_bm          = cols#add Gobject.Data.caml in
  let col_view        = cols#add Gobject.Data.caml in
  let model           = GTree.list_store cols in
  let view            = GTree.view ~model ~headers_visible:false ~width:150 ~packing:sw#add () in
  let renderer        = GTree.cell_renderer_text [`YPAD 0] in
  let renderer_pixbuf = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
  let vc              = GTree.view_column ~title:"" () in
  let _               = vc#pack ~expand:false renderer_pixbuf in
  let _               = vc#pack ~expand:false renderer in
  let _               = view#append_column vc in
object (self)
  inherit GObj.widget_full vbox#as_widget

  method view = view

  method set_bookmarks bookmarks =
    List.iter begin fun bm ->
      let row = model#append () in
      model#set ~row ~column:col_bm bm;
      model#set ~row ~column:col_view None;
    end (List.sort (fun b1 b2 -> Pervasives.compare b1.Bookmark.num b2.Bookmark.num) bookmarks)

  method private connect_bookmarks_changed () =
    ignore (Bookmark.bookmarks#connect#changed ~callback:begin fun bookmarks ->
      self#set_bookmarks bookmarks
    end);

  method private bookmark_goto ~bookmark ~view =
    Bookmark.apply bookmark begin function
      | `OFFSET offset ->
        let _ = Bookmark.offset_to_mark (view#buffer :> GText.buffer) bookmark in
        self#bookmark_goto ~bookmark ~view;
      | `ITER it ->
        let where = new GText.iter it in
        view#scroll_lazy where;
        view#buffer#place_cursor ~where;
        view#misc#grab_focus();
    end;

  method private connect_selection_changed () =
    ignore (view#selection#connect#after#changed ~callback:begin fun () ->
      try
        (try swr#remove swr#child with Gpointer.Null -> ());
        let path = List.hd view#selection#get_selected_rows in
        let row = model#get_iter path in
        let bookmark = model#get ~row ~column:col_bm in
        begin
          match model#get ~row ~column:col_view with
            | Some text_view ->
              self#bookmark_goto ~bookmark ~view:text_view;
              swr#add text_view#coerce
            | _ ->
              let filename = bookmark.Bookmark.filename in
              begin
                match editor#get_page (Editor_types.File (File.create filename ())) with
                  | Some page ->
                    let text_view = new Ocaml_text.view ~project:page#project ~buffer:page#buffer () in
                    text_view#set_editable false;
                    text_view#set_cursor_visible false;
                    Preferences_apply.apply (text_view :> Text.view) !Preferences.preferences;

                    self#bookmark_goto ~bookmark ~view:text_view;

                    model#set ~row ~column:col_view (Some text_view);
                    swr#add text_view#coerce;
                  | _ -> ()
              end;
        end;
      with Failure "hd" -> ()
    end);

  method private set_cell_data_func () =
    vc#set_cell_data_func renderer begin fun model row ->
      let bm = model#get ~row ~column:col_bm in
      let pixbuf = List.assoc bm.Bookmark.num Bookmark.icons in
      let name = Filename.basename bm.Bookmark.filename in
      renderer_pixbuf#set_properties [ `PIXBUF pixbuf];
      renderer#set_properties [`TEXT name]
    end;

  initializer
    self#set_bookmarks Bookmark.bookmarks#get;
    self#connect_bookmarks_changed();
    self#set_cell_data_func();
    self#connect_selection_changed();
end













